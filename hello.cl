;; (ql:add-to-init-file)
;; (ql:update-dist "quicklisp")

(ql:quickload "cl-csv")
;(ql:quickload "alexandria")
(ql:quickload "str")
(ql:quickload "cl-ppcre")

#| (defun hello-world ()
  (format t "Hello, World dude!"))

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

 |#
;(defun delimiterp (c) (or (char= c #\Space) (char= c #\,)))

(defun delimiterp (c) (char= c #\_))

(defun my-split (string &key (delimiterp #'delimiterp))
  (loop 
    :for beg = (position-if-not delimiterp string)
    :then (position-if-not delimiterp string :start (1+ end))
    :for end = (and beg (position-if delimiterp string :start beg))
    :when beg :collect (subseq string beg end)
    :while end))


;(defun demo3 () (nth 1 (my-split "A_B_C")))

(defvar *photos* nil)

(defparameter pappi '("153" "Bingo" "74" "250" "NULL" "2017-05-06" "PAPP.jpg"))

(defparameter pappe '("153" "Bingo" "EA4" "250" "NULL" "2017-05-06" "PAPP.jpg"))

(defun make-photo (item)
  (let ((gps (nth 0 item))
        (ednum (nth 2 item))
        (dx (nth 5 item)))
  (list :gps gps :ednum ednum :dx dx :gpsx nil)))

(defun add-rec (photo) (push photo *photos*))

(defun parse-ednum (edn)
  edn)

(defun create-key (photo) 
  (let* ((edn (getf photo :ednum))
         (is-num (num-string-p edn)))
    (if is-num
      (concatenate 'string (getf photo :gps) ":3:" edn)
      (concatenate 'string (getf photo :gps) ":4:" edn))))

(defun num-string-p (s)
  (numberp (read-from-string s)))

(defun lc2 ()
  (let ((x 
          (cl-csv:read-csv 
            #P"gpsx2.csv" 
            :separator ";")))
  (nth 0 (first (rest x)))))

(defun gpsx-dict ()
  (let* 
    ((y (make-hash-table :test 'equal))
     (x 
        (cl-csv:read-csv 
          #P"gpsx2.csv" 
            :separator ";"
            :map-fn #'(lambda (row) 
                        (let ((key (nth 0 row)))
                          (setf (gethash key y) row))))))
    ;;(remhash "kx" y)
    y))

(defparameter my-hash (gpsx-dict))

(defparameter papp-jpgs "azure_papp2_jpgs")

(defun papp-items ()
  (let ((in (open papp-jpgs :if-does-not-exist nil)))
    (when in 
      (loop for line = (read-line in nil)
        while line do (format t "̃~a~%" line))
        (close in))))


;(defun papp-items-2 ()
;  (first (alexandria:read-file-into-string papp-jpgs)))

(defun papp-items-3 ()
  (map 'list #'my-split 
    (str:lines (str:from-file papp-jpgs))))


(defun demo ()
  (gethash "105:4:i" my-hash))

(defun print-hash-entry (key value)
  (format t "The value associated with the key ~S is ~S~%"
            key value))

(defun demo2 ()
  (maphash #'print-hash-entry my-hash))