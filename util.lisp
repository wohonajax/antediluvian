(in-package #:dhticl-util)

(defun within (number first-bound end-bound)
  "Tests whether NUMBER is contained within the range bounded by FIRST-BOUND
  and END-BOUND."
  (= number
     (alexandria:clamp number
		       first-bound
		       end-bound)))

(defun minutes-since (time)
  "Returns the time in minutes that has elapsed since TIME."
  (declare (type fixnum time))
  (/ (the fixnum
	  (- (the fixnum (get-universal-time))
	     time))
     60))

(defmacro awhen (test &body forms)
  "Your usual anaphoric WHEN."
  `(let ((it ,test))
     (when it
       ,@forms)))
