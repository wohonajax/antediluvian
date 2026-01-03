(in-package #:antediluvian)

(defvar *default-port* 6881
  "The default port for antediluvian to use for network communications.")

(defvar *use-implied-port-p* nil
  "Whether peers should use the visible port (T) or the given port (NIL).
Useful for NATs.")

(defvar *settings-location* (xdg-config-home "antediluvian/settings.sexp")
  "Where to store and load settings.")

(defvar *default-download-directory*
        (lret ((path (truename "~/Downloads/")))
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
    (ensure-directories-exist (xdg-config-home "antediluvian/"))
    (with-open-file (file *settings-location*
                          :direction :output
                          :if-exists :overwrite
                          :if-does-not-exist :create)
      (format file "~{~S~^~%~}"
              (list (make-setting *default-port*)
                    (make-setting *use-implied-port-p*)
                    (make-setting *default-download-directory*))))))