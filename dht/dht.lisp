(in-package #:antediluvian)

;;;; TODO: general interface
(defvar *hashes* (list)
  "The list of info_hashes the DHT program will use.")

(defun bootstrap-node (host port)
  "Bootstraps the routing table using a known node at HOST and PORT."
  (send-message :find_node host port (generate-transaction-id)
                :info-hash *id*))
;;; TODO: find_node each found node for nodes near the hash
(defun main-loop ()
  (loop with start-time = (get-universal-time)
        do (parse-message)
          ;; FIXME: MOD might be too exact if PARSE-MESSAGE blocks for a while
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
  (unless (member hash *hashes* :test #'equalp)
    (push hash *hashes*)
    (initiate-lookup hash)))

(defun connect-datagram-socket (port)
  "Attempts to connect a UDP socket listening on PORT. Returns a usocket object
if successful, NIL if it fails."
  (handler-case (socket-connect nil nil :protocol :datagram :local-port port)
    (error ())))

(defun try-ports (port)
  "Tries to create a listening UDP socket listening on PORT. If the attempt
fails, try ports 6881 through 6889."
  (loop initially (setf *listening-dht-socket* (connect-datagram-socket port))
        ;; if *listening-dht-socket* is nil,
        ;; the connect operation failed.
        ;; try a few default ports as well
        for candidate-port from 6881 upto 6889
        until *listening-dht-socket*
        do (setf *listening-dht-socket*
                 (connect-datagram-socket candidate-port)))
  (unless *listening-dht-socket*
    (error "Unable to connect on port ~D or on any default ports." port)))

(defun setup (hashes)
  "Performs setup on program startup. Sets up initial variables, etc."
  (load-settings)
  ;; TODO: expose number of worker threads as a setting
  (setf lparallel:*kernel* (make-kernel 1))
  (try-ports *default-port*)
  (setf *default-port* (get-local-port *listening-dht-socket*))
  (setf *secret-rotation-thread* (start-sercret-rotation-thread)
        *main-dht-thread* (make-thread #'main-loop :name "Main DHT thread"))
  ;; bootstrap the DHT with known nodes
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
  (end-kernel)
  (save-settings))

(defun dht (&rest hashes)
  "Initiates the distributed hash table."
  (setup hashes)
  ;; FIXME: MAIN-LOOP is already in its own thread
  (unwind-protect (main-loop)
    (cleanup)))
