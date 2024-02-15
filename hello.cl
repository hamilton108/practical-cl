(defun hello-world ()
  (format t "Hello, World dude!"))

;; (ql:add-to-init-file)
;; (ql:update-dist "quicklisp")

(ql:quickload "cl-csv")

(defvar *db* nil)

(defun make-cd (title artist rating ripped)
  (list :title title :artist artist :rating rating :ripped ripped))

(defun add-rec (cd) (push cd *db*))


(defun populate ()
  (add-rec (make-cd "Hu" "Woo" 3 t)))

(defun dump-db ()
  (dolist (cd *db*)
    (format t "~{~a:~10t~a~%~}̃~%" cd)))

(defun save-db (fname)
  (with-open-file (out fname
                  :direction :output
                  :if-exists :supersede)
    (with-standard-io-syntax 
      (print *db* out))))

(defun load-db (fname)
  (with-open-file (in fname)
    (with-standard-io-syntax 
      (setf *db* (read in)))))