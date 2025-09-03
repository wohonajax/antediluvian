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
  (send-message :find_node host port (generate-transaction-id)
                :info-hash *id*))

(defun initiate-lookups ()
  (mapc (lambda (hash)
          (let ((node-list (find-closest-nodes hash)))
            (dotimes (i +alpha+) ; FIXME: handle the whole list
              (let ((node (nth i node-list)))
                (send-message :find_node (node-ip node) (node-port node)
                              (generate-transaction-id) :info-hash hash)))))
        *hashes*))
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
            (refresh-tokens))))

(defun dht (&rest hashes)
  "Initiates the distributed hash table."
  (load-settings)
  (setf *listening-dht-socket* (socket-connect nil nil
                                               :protocol :datagram
                                               :local-port *default-port*))
  (when hashes
    (mapc (lambda (hash) (push hash *hashes*))
          hashes))
  (unwind-protect
       (main-loop)
    (progn (iterate-table #'purge-bad-nodes)
           (iterate-table #'purge-stale-nodes)
           (socket-close *listening-dht-socket*)
           (destroy-thread *secret-rotation-thread*)
           (save-settings))))