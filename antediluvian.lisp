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
;;; TODO: keep track of how much we've uploaded/downloaded
(defun send-stopped-announces ()
  "Sends announces to all trackers associated with each active torrent that
we're not active anymore."
  (dolist (torrent *torrents*)
    (let ((metainfo (torrent-info torrent))
          (info-hash (torrent-info-hash torrent)))
      (flet ((send-stop (url)
               (handler-case (send-announce url info-hash :event :stopped)
                 (error () (return-from send-stop)))))
        (if-let (announce-list (gethash "announce-list" metainfo))
          (dolist (announce-tier announce-list)
            (dolist (announce-url announce-tier)
              (send-stop announce-url)))
          (send-stop (gethash "announce" metainfo)))))))

(defun cleanup ()
  "Performs cleanup on shutdown."
  (dht-cleanup)
  (socket-close *listening-peer-socket*)
  (destroy-thread *peer-listener-thread*)
  (mapc (lambda (thread)
          (when (thread-alive-p thread)
            (destroy-thread thread)))
        *peer-connection-threads*)
  (setf *peer-connection-threads* nil)
  (with-lock-held (*peer-list-lock*)
    (mapc #'close-peer-socket *peer-list*))
  (mapc #'remove-peer-from-peer-list *peer-list*)
  (destroy-thread *file-writer-thread*)
  (send-stopped-announces))

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