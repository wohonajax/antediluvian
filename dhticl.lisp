(in-package :dhticl)

;;;; TODO: general interface
(defvar *settings-location*
  (merge-pathnames ".dhticlrc" (user-homedir-pathname)))

(defvar *ipv6p* nil)

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
				  (make-setting *default-port*)
				  (make-setting *ipv6p*))))))
(defun main-loop () "Silences compilation warnings." t) ; FIXME: delete
(defun dht ()
  "Initiates the distributed hash table."
  (load-settings)
  (load-table)
  (unwind-protect
       (main-loop) ; TODO: make the loop in routing.lisp
    (progn (save-settings)
	   (save-table))))
