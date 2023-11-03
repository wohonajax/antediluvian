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

(defvar *listening-socket*)

(defun main-loop ()
  (handler-bind ((peer-requested (lambda (c) c))
                 (peer-request (lambda (c) c))
                 (kill-signal (lambda (c) (declare (ignore c))
                                (return-from main-loop))))
    (with-listening-usocket socket
      (setf *listening-socket* socket)
      (loop :do (multiple-value-bind (buffer size host port)
                    (receive-data 65507) ; max UDP packet payload size
                  (let* ((packet (subseq buffer 0 size))
                         (dict (bencode:decode packet)))
                    (alexandria:switch ((gethash "y" dict) :test #'string=)
                      ("q" (parse-query dict host port))
                      ("r" (parse-response dict host port))
                      ("e" ;; TODO handle errors
                       ))))))))

(defun dht ()
  "Initiates the distributed hash table."
  (load-settings)
  (load-table)
  (unwind-protect
       (main-loop)
    (progn (save-settings)
           (save-table))))
