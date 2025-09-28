(in-package #:dhticl)

;;;; TODO: general interface
(defvar *settings-location*
  (xdg-config-home "dhticl/settings.sexp"))

(defvar *hashes* (list)
  "The list of info_hashes the DHT program will use.")

;;; TODO: sanitize settings
(defun load-settings ()
  "Loads settings."
  (let ((file (probe-file *settings-location*)))
    (when file
      (load file))))

(defun save-settings ()
  "Saves settings."
  (macrolet ((make-setting (setting)
               `(list 'setf ',setting ,setting)))
    (with-open-file (file *settings-location*
                          :direction :output
                          :if-exists :overwrite
                          :if-does-not-exist :create)
      (format file "~{~S~^~%~}"
              (list (make-setting *routing-table-location*)
                    (make-setting *default-port*)
                    (make-setting *use-implied-port-p*)
                    (make-setting *hashes*))))))

(defun bootstrap-node (host port)
  "Bootstraps the routing table using a known node at HOST and PORT."
  (send-message :find_node host port (generate-transaction-id)
                :info-hash *id*)
  (parse-message))

(defun initiate-lookups ()
  (mapc #'initiate-lookup *hashes*))
;;; TODO: find_node each found node for nodes near the hash
(defun main-loop ()
  ;; bootstrap the DHT with known nodes
  ;; (taken from qbittorrent's bootstrap list)
  (bootstrap-node "router.bittorrent.com" 6881)
  (bootstrap-node "dht.libtorrent.org" 25401)
  ;; FIXME: these 4 nodes don't seem to respond to pings
  (bootstrap-node "router.utorrent.com" 6881)
  (bootstrap-node "dht.transmissionbt.com" 6881)
  (bootstrap-node "dht.aelitis.com" 6881)
  (bootstrap-node "bootstrap.jami.net" 4222)
  ;; TODO: wait for bootstrapping before initiating lookups
  (initiate-lookups)
  (loop with start-time = (get-universal-time)
        do (parse-message)
          ;; TODO: routing table upkeep
          (when (= 0 (mod (minutes-since start-time) 10))
            (iterate-table (lambda (bucket)
                             (purge-stale-nodes bucket)
                             (handle-questionable-nodes bucket)
                             (purge-bad-nodes bucket)
                             (ping-old-nodes bucket)))
            (maybe-replace-nodes)
            (refresh-tokens))))

(defun setup (hashes)
  "Performs setup on program startup. Sets up initial variables, etc."
  (load-settings)
  (setf *listening-dht-socket* (socket-connect nil nil
                                               :protocol :datagram
                                               :local-port *default-port*)
        *secret-rotation-thread* (start-sercret-rotation-thread))
  (when hashes
    (mapc (lambda (hash) (push hash *hashes*))
          (remove-duplicates hashes :test #'equalp))))

(defun close-peer-sockets (peers-table)
  "Closes the all the open sockets in PEERS-TABLE."
  (loop for peer-future being the hash-values of peers-table
        do (when-let (socket (force peer-future))
             (socket-close socket))))

(defun cleanup ()
  "Performs cleanup on program shutdown. Closes sockets, destroys threads, and
saves settings."
  (socket-close *listening-dht-socket*)
  (loop for peers-table being the hash-values of *peer-list*
        do (close-peer-sockets peers-table))
  (destroy-thread *secret-rotation-thread*)
  (save-settings))

(defun dht (&rest hashes)
  "Initiates the distributed hash table."
  (setup hashes)
  (unwind-protect (main-loop)
    (cleanup)))