(in-package #:antediluvian)

(defparameter *version* "0000"
  "String denoting client version. Four characters for major and minor
versions, e.g.: 0.00.0")

(defvar *port* 6881
  "The default port for antediluvian to use for network communications.")

(defvar *use-implied-port-p* nil
  "Whether peers should use the visible port (T) or the given port (NIL).
Useful for NATs.")

(defun settings-pathspec (relative-path)
  "Returns an absolute pathname to RELATIVE-PATH relative to the platform's
configuration directory."
  #-mezzano (xdg-config-home relative-path)
  #+mezzano (merge-pathnames relative-path (user-homedir-pathname)))

(defvar *settings-location* (settings-pathspec (filepaths:join "antediluvian" "settings.sexp"))
  "Where to store and load settings.")

(defvar *download-directory*
        (lret ((path (merge-pathnames (filepaths:ensure-directory "Downloads")
                                      (user-homedir-pathname))))
          (ensure-directories-exist path))
  "The default directory to download torrents into.")
;;; TODO: sanitize settings
(defun load-settings ()
  "Loads settings."
  (when-let (file (probe-file *settings-location*))
    (load file)))

(defun save-settings ()
  "Saves settings."
  (macrolet ((make-setting (setting)
               `(list 'setf ',setting ,setting)))
    (ensure-directories-exist (settings-pathspec (filepaths:ensure-directory "antediluvian")))
    (with-open-file (file *settings-location*
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
      (format file "~{~S~^~%~}"
              (list (make-setting *port*)
                    (make-setting *use-implied-port-p*)
                    (make-setting *download-directory*))))))