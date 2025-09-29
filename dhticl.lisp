(in-package #:dhticl)

;;;; TODO: general interface
(defvar *settings-location*
  (xdg-config-home "dhticl/settings.sexp"))

(defvar *hashes* (list)
  "The list of info_hashes the DHT program will use.")

;;; TODO: sanitize settings
(defun load-settings ()
  "Loads settings."
  (when-let (file (probe-file *settings-location*))
    (load file)))

(defun save-settings ()
  "Saves settings."
  (macrolet ((make-setting (setting)
               `(list 'setf ',setting ,setting)))
    (with-open-file (file *settings-location*
                          :direction :output
                          :if-exists :overwrite
                          :if-does-not-exist :create)
      (format file "~{~S~^~%~}"
              (list (make-setting *default-port*)
                    (make-setting *use-implied-port-p*)
                    (make-setting *hashes*))))))

(defun bootstrap-node (host port)
  "Bootstraps the routing table using a known node at HOST and PORT."
  (send-message :find_node host port (generate-transaction-id)
                :info-hash *id*)
  (parse-message))
;;; TODO: find_node each found node for nodes near the hash
(defun main-loop ()
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

(defvar *main-dht-thread* nil
  "The thread that the main loop of the DHT is running in.")

(defun add-hash (hash)
  "Adds HASH to the active hashes the DHT is using."
  (pushnew hash *hashes* :test #'equalp)
  (initiate-lookup hash))

(defun connect-datagram-socket (port)
  "Attempts to connect a UDP socket listening on PORT. Returns a usocket object
if successful, NIL if it fails."
  (handler-case (socket-connect nil nil :protocol :datagram :local-port port)
    (error ())))

(defun try-ports (port)
  "Tries to create a listening UDP socket listening on PORT. If the attempt
fails, try ports 6881 through 6889."
  (setf *listening-dht-socket* (connect-datagram-socket port))
  (when *listening-dht-socket*
    (return-from try-port))
  ;; if *listening-dht-socket* is nil,
  ;; the connect operation failed.
  ;; try ports 6881 through 6889
  (loop for port-candidate from 6881 upto 6889
        until *listening-dht-socket*
        do (setf *listening-dht-socket*
                 (connect-datagram-socket port-candidate)))
  (unless *listening-dht-socket*
    (error "Unable to connect on port ~D" port)))

(defun setup (hashes)
  "Performs setup on program startup. Sets up initial variables, etc."
  (load-settings)
  (try-ports *default-port*)
  (setf *default-port* (get-local-port *listening-dht-socket*))
  (setf *listening-dht-socket* (socket-connect nil nil
                                               :protocol :datagram
                                               :local-port *default-port*)
        *secret-rotation-thread* (start-sercret-rotation-thread)
        *main-dht-thread* (make-thread #'main-loop))
  ;; bootstrap the DHT with known nodes
  ;; (taken from qbittorrent's bootstrap list)
  (bootstrap-node "router.bittorrent.com" 6881)
  (bootstrap-node "dht.libtorrent.org" 25401)
  ;; FIXME: these 4 nodes don't seem to respond to pings
  (bootstrap-node "router.utorrent.com" 6881)
  (bootstrap-node "dht.transmissionbt.com" 6881)
  (bootstrap-node "dht.aelitis.com" 6881)
  (bootstrap-node "bootstrap.jami.net" 4222)
  (mapc #'add-hash hashes))

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
  (destroy-thread *main-dht-thread*)
  (save-settings))

(defun dht (&rest hashes)
  "Initiates the distributed hash table."
  (setup hashes)
  (unwind-protect (main-loop)
    (cleanup)))