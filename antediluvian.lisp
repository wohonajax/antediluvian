;;;; General interface

(in-package #:antediluvian)

(defun setup (sources)
  "Sets up antediluvian and initiates the DHT using SOURCES, which should be a
list of SHA1 hashes, magnet links, or torrent file paths."
  (setf *listening-peer-socket* (socket-listen *wildcard-host* *port*
                                               :element-type '(unsigned-byte 8))
        *peer-listener-thread* (start-listener-thread)
        *file-writer-thread* (start-file-writer-thread))
  (lret ((torrents (parse-sources sources)))
    (mapc #'add-torrent torrents)
    ;; FIXME: Figure out a better interface than the DHT function
    (apply #'dht (mapcar #'torrent-info-hash torrents))))

(defun cleanup ()
  "Performs cleanup on shutdown."
  (dht-cleanup)
  (socket-close *listening-peer-socket*)
  (destroy-thread *peer-listener-thread*)
  (mapc (lambda (thread)
          (when (thread-alive-p thread)
            (destroy-thread thread)))
        *peer-connection-threads*)
  (with-lock-held (*peer-list-lock*)
    (mapc (lambda (peer)
            (with-lock-held ((peer-lock peer))
              (socket-close (peer-socket peer))))
          *peer-list*))
  (destroy-thread *file-writer-thread*))

(defun start (&rest sources)
  "Starts up the torrent client with SOURCES, which should be SHA1 hashes,
magnet links, or torrent file specifiers."
  (setup sources))

(defun add-torrent (torrent)
  "Adds TORRENT to the registry of active torrents. Must be a torrent object."
  (let ((info-hash (torrent-info-hash torrent)))
    (unless (member info-hash *torrents* :key #'torrent-info-hash :test #'equalp)
      (push torrent *torrents*)
      (setf (gethash info-hash *torrent-hashes*) torrent)
      (mapc #'connect-to-peer (torrent-announce torrent)))))

(defun add-source-as-torrent (source)
  "Adds a torrent from SOURCE, which should be a magnet link, a filespec to a
torrent file, or a SHA1 hash."
  (lret ((torrent (parse-source source)))
    (add-torrent torrent)
    (add-hash (torrent-info-hash torrent))))