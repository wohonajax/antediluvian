(in-package #:antediluvian)

(defparameter *version* "0000"
  "String denoting client version. Four characters for major and minor
versions, e.g.: 0.00.0")

(defvar *port* 6881
  "The default port for antediluvian to use for network communications.")

(defvar *use-implied-port-p* nil
  "Whether peers should use the visible port (T) or the given port (NIL).
Useful for NATs.")

(defvar *settings-location*
        #-mezzano (xdg-config-home "antediluvian/settings.sexp")
        #+mezzano (merge-pathnames "antediluvian;settings.sexp"
                                   (user-homedir-pathname))
  "Where to store and load settings.")

(defvar *download-directory*
        (lret ((path (merge-pathnames (make-pathname :directory '(:relative "Downloads"))
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
    (ensure-directories-exist #-mezzano (xdg-config-home "antediluvian/")
                              #+mezzano (merge-pathnames "antediluvian;"
                                                         (user-homedir-pathname)))
    (with-open-file (file *settings-location*
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
      (format file "~{~S~^~%~}"
              (list (make-setting *port*)
                    (make-setting *use-implied-port-p*)
                    (make-setting *download-directory*))))))