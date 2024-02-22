;; (ql:add-to-init-file)
;; (ql:update-dist "quicklisp")

(ql:quickload "cl-csv")
;(ql:quickload "alexandria")
(ql:quickload "str")
(ql:quickload "cl-ppcre")

(defun delimiterp (c) (char= c #\_))

(defun my-split (string &key (delimiterp #'delimiterp))
  (loop 
    :for beg = (position-if-not delimiterp string)
    :then (position-if-not delimiterp string :start (1+ end))
    :for end = (and beg (position-if delimiterp string :start beg))
    :when beg :collect (subseq string beg end)
    :while end))

(defvar *photos* nil)

(defparameter pappi '("153" "Bingo" "74" "250" "NULL" "2017-05-06" "PAPP.jpg"))

(defparameter pappe '("153" "Bingo" "EA4" "250" "NULL" "2017-05-06" "PAPP.jpg"))

(defun parse-ednum (edn)
  (let ((edn1 (string-downcase edn)))
    (cond 
      ((num-string-p edn) (list :edt 3 :val edn))
      ((equal (subseq edn1 0 2) "ea") (list :edt 4 :val (subseq edn 2)))
      ((equal (subseq edn1 0 2) "tp") (list :edt 1 :val (subseq edn 2)))
      (t (list :edt 0 :val "Noope")))))

(defun make-photo (item)
  (let* ((gps (nth 0 item))
        (ednum (nth 2 item))
        (dx (nth 5 item))
        (ped (parse-ednum ednum))
        (pwf (nth 4 item))
        (pwf1 (if (equal pwf "NULL") nil pwf)))
  (list :photo nil :gps gps :ednum (getf ped :val) :dx dx :edtype (getf ped :edt) :pwf pwf1 :gpsx nil)))

(defun add-rec (photo) (push photo *photos*))

(defun num-string-p (s)
  (numberp (read-from-string s)))

(defun make-key (photo) 
  (let ((gps (getf photo :gps))
        (edt (write-to-string (getf photo :edtype)))
        (edn (getf photo :ednum)))
    (concatenate 'string gps ":"  edt ":" edn)))

(defun populate-ht (ht)
  (cl-csv:read-csv 
    #P"gpsx2.csv" 
      :separator ";"
      :map-fn #'(lambda (row) 
                  (let ((key (nth 0 row)))
                    (setf (gethash key ht) row)))))

(defun gpsx-dict ()
  (let 
    ((y (make-hash-table :test 'equal)))
      (populate-ht y)
      y))

(defparameter my-hash (gpsx-dict))

(defparameter papp-jpgs "azure_papp2_jpgs")

(defun papp-items ()
  (map 'list #'my-split 
    (str:lines (str:from-file papp-jpgs))))


(defun print-hash-entry (key value)
  (format t "The value associated with the key ~S is ~S~%"
            key value))

(defun curry (function &rest initial-args)
  "Returns a function which will call FUNCTION passing it
  INITIAL-ARGS and then any other args.
  (funcall (curry #'list 1) 2) ==> (list 1 2)"

  (lambda (&rest args)
    (apply function (append initial-args args))))

(defun demo2 ()
  (maphash #'print-hash-entry my-hash))

(defun assign-gpsx (item)
  (let* ((key (make-key item))
         (gpsx (gethash key my-hash)))
    (if gpsx 
      (setf (getf item :gpsx) (nth 3 gpsx)))
    item))

(defun demo3 ()
  (let 
    ((items (map 'list #'make-photo (papp-items))))
    (map 'list #'assign-gpsx items)))


(defun ab (a b c) (* a b c))

(defun curry-demo ()
  (funcall (curry #'ab 10 20) 30))