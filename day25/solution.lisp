;; --- Day 25: Full of Hot Air ---
;; TODO: trans to chez scheme

(defpackage aoc2022.day25
  (:use :cl)
  (:export
   :puzzle1)
  (:import-from :aoc2021.util
                :parse-input))

(in-package :aoc2022.day25)

(defconstant +base+ 5)

;; 4890 to 2=-1=0
(defun decimal-snafu (num)
  (labels ((col (remain digits)
             (cond
               ((< remain +base+) (cons remain digits))
               (t (multiple-value-bind (v m) (floor remain +base+)
                    (col v (cons m digits)))))))
    (col num '())))

(defun niltozero (v)
  (cond
    ((null v) 0)
    (t v)))

(defun expand (digits)
  (labels ((col (ds c res)
             (let ((d (+ c (niltozero (car ds)))))
               (cond
                 ((null ds) (if (= c 0)
                                res
                                (cons c res)))
                 ((= d 5) (col (cdr ds)
                               1
                               (cons 0 res)))
                 ((= d 4) (col (cdr ds)
                               1
                               (cons '- res)))
                 ((= d 3) (col (cdr ds)
                               1
                               (cons '= res)))
                 (t (col (cdr ds)
                         0
                         (cons (+ c (car ds)) res)))))))
    (col (reverse digits) 0 '())))

(defun snafu-decimal (digits)
  (labels ((col (digits pos res)
             (cond
               ((null digits) res)
               ((equal (car digits) '-) (col (cdr digits)
                                             (1+ pos)
                                             (+ res (* -1 (expt +base+ pos)))))
               ((equal (car digits) '=) (col (cdr digits)
                                             (1+ pos)
                                             (+ res (* -2 (expt +base+ pos)))))
               (t (col (cdr digits)
                       (1+ pos)
                       (+ res (* (car digits) (expt +base+ pos))))))))
    (col (reverse digits)
         0
         0)))

(defun parse-line (line)
  (map 'list
       (lambda (c)
         (cond
           ((char= c #\-) '-)
           ((char= c #\=) '=)
           (t (parse-integer (string c)))))
       line))

(defun puzzle1 (path)
  (expand (decimal-snafu  (apply #'+  (map 'list #'snafu-decimal (parse-input path #'parse-line))))))
