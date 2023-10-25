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

(define-condition peer-requested () ())
(define-condition peer-request () ())
(define-condition kill-signal () ())

(defun main-loop ()
  (handler-bind ((peer-requested (lambda () ))
                 (peer-request (lambda () ))
                 (kill-signal (lambda () (return-from main-loop))))
    (with-listening-usocket-stream (socket stream usocket:*wildcard-host*
                                           *default-port*)
      (loop :for line := (read-line stream nil) :doing
         (switch (line :test #'string-equal)
           ;; TODO
           (""))))))

(defun dht ()
  "Initiates the distributed hash table."
  (load-settings)
  (load-table)
  (unwind-protect
       (main-loop)
    (progn (save-settings)
           (save-table))))
