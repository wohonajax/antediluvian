;;;; General interface

(in-package #:antediluvian)

(defun setup (sources)
  "Sets up antediluvian and initiates the DHT using SOURCES, which should be a
list of SHA1 hashes, magnet links, or torrent file paths."
  (setf *listening-peer-socket* (socket-listen *wildcard-host* *default-port*)
        *peer-listener-thread* (start-listener-thread))
  ;; FIXME: Figure out a better interface than the DHT function
  (apply #'dht (mapcar #'torrent-info-hash (parse-sources sources))))

(defun cleanup ()
  "Performs cleanup on shutdown."
  (dht-cleanup)
  (socket-close *listening-peer-socket*)
  (mapc #'socket-close *accepted-connections*)
  (destroy-thread *peer-listener-thread*))

(defun start (&rest sources)
  "Starts up the torrent client with SOURCES, which should be SHA1 hashes,
magnet links, or torrent file specifiers."
  (setup sources))