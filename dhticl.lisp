(in-package :dhticl)

;;;; TODO: general interface
(defvar *settings-location*
  (merge-pathnames ".dhticl/settings.sexp" (user-homedir-pathname)))

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
      (format file "~{~S~}" (list (make-setting *routing-table-location*)
                                  (make-setting *default-port*))))))

(defun bootstrap-node (host port)
  (send-message :find_node host port (generate-transaction-id)
                :info-hash *id*))

;;; TODO: find_node each found node for nodes near the hash
(defun main-loop ()
  (with-listening-usocket socket
    (setf *listening-socket* socket)
    ;; bootstrap the DHT with known nodes
    ;; (taken from qbittorrent's bootstrap list)
    (bootstrap-node "router.bittorrent.com" 6881)
    (bootstrap-node "router.utorrent.com" 6881)
    (bootstrap-node "dht.transmissionbt.com" 6881)
    (bootstrap-node "dht.libtorrent.org" 25401)
    (bootstrap-node "dht.aelitis.com" 6881)
    (mapc (lambda (hash)
            (let ((node-list (find-closest-nodes hash)))
              (dotimes (i +alpha+)
                (let ((node (nth i node-list)))
                  (send-message :find_node (node-ip node) (node-port node)
                                (generate-transaction-id)
                                :info-hash hash)))))
          *hashes*)
    (let ((start-time (get-universal-time)))
      (loop (parse-message)
            ;; TODO: routing table upkeep
            (when (= 0 (mod (minutes-since start-time) 10))
              (iterate-table (lambda (bucket)
                               (purge-bad-nodes bucket)
                               (handle-questionable-nodes bucket)
                               (ping-old-nodes bucket)))
              (refresh-tokens))))))

(defun dht (&rest hashes)
  "Initiates the distributed hash table."
  (load-settings)
  (load-table)
  (when hashes
    (mapc (lambda (hash) (push hash *hashes*))
          hashes))
  (unwind-protect
       (main-loop)
    (progn (save-settings)
           (save-table))))