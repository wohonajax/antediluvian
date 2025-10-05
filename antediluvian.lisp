;;;; General interface

(in-package #:antediluvian)

(defun setup (sources)
  "Sets up antediluvian and initiates the DHT using SOURCES, which should be a
list of SHA1 hashes, magnet links, or torrent file paths."
  (setf *listening-peer-socket* (socket-listen *wildcard-host* *default-port*)
        *peer-listener-thread* (start-listener-thread))
  ;; FIXME: handle magnet links
  (apply #'dht (parse-sources sources)))

(defun cleanup ()
  "Performs cleanup on shutdown."
  (socket-close *listening-peer-socket*)
  (mapc #'socket-close *accepted-connections*)
  (destroy-thread *peer-listener-thread*))

(defun start (&rest sources)
  "Starts up the torrent client with SOURCES, which should be SHA1 hashes or
magnet links."
  (setup sources)
  (unwind-protect (main-loop)
    (cleanup)))