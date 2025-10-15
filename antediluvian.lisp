;;;; General interface

(in-package #:antediluvian)

(defun setup (sources)
  "Sets up antediluvian and initiates the DHT using SOURCES, which should be a
list of SHA1 hashes, magnet links, or torrent file paths."
  (setf *listening-peer-socket* (socket-listen *wildcard-host* *default-port*)
        *peer-listener-thread* (start-listener-thread)
        *file-writer-thread* (start-file-writer-thread))
  ;; FIXME: Figure out a better interface than the DHT function
  (apply #'dht (mapcar #'torrent-info-hash (parse-sources sources))))

(defun cleanup ()
  "Performs cleanup on shutdown."
  (dht-cleanup)
  (socket-close *listening-peer-socket*)
  (destroy-thread *peer-listener-thread*)
  (mapc #'destroy-thread *listening-threads*)
  (loop for peer-table being the hash-values of *peer-list*
        do (loop for peer being the hash-values of peer-table
                 do (when-let (socket (force (peer-socket peer)))
                      (socket-close socket))))
  (destroy-thread *file-writer-thread*))

(defun start (&rest sources)
  "Starts up the torrent client with SOURCES, which should be SHA1 hashes,
magnet links, or torrent file specifiers."
  (setup sources))