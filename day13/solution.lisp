;; --- Day 13: Distress Signal ---
;; trans this code to chez scheme

(defpackage aoc2022.day13
  (:use :cl)
  (:export :puzzle1
           :parse-list
           :parse-list-v2
           :tokenize)
  (:import-from :aoc2021.util
                :parse-input
                :parse-int
                ))

(in-package :aoc2022.day13)

(defun open-paren? (ele)
  (string= ele "["))

(defun close-paren? (ele)
  (string= ele "]"))

(defun delim? (ele)
  (or (string= ele ",")
      (string= ele "[")
      (string= ele "]"))
  )
;; start-with "[" and end with "]"
;; assume that input is perfect
(defun parse-list (toks curr)

  (cond
    ;; ((null toks) (values (reverse  curr) '()))
    ((open-paren? (cadr toks)) (multiple-value-bind (lst r)
                                   (parse-list (cdr toks) '())
                                 (parse-list r (cons lst curr))))
    ((close-paren? (cadr toks)) (values (reverse  curr) (cdr toks)))
    (t (parse-list (cdr toks) (cons (cadr toks) curr)))))

;; first tok has readed
;; [[8,1,[2,3,3,[6,7,7,2,6]],[[8,10,1]],[9]],[1,7,[7,[3,6],7,7,10]]]
(defun parse-list-v2 (toks curr)
  (cond
    ((null toks) (error "unbalanced parens"))
    ((open-paren? (cadr toks)) (multiple-value-bind (lst r)
                                   (parse-list-v2 (cdr toks) '())
                                 (if (close-paren? (car r))
                                     (parse-list-v2 r (cons lst curr))
                                     (error "unbalanced parens"))))
    ((close-paren? (cadr toks)) (values (reverse  curr) (cdr toks)))
    ((delim? (cadr toks)) (parse-list-v2 (cdr toks) curr))
    (t (parse-list-v2 (cdr toks) (cons (cadr toks) curr)))))

(defun parselist (toks)
  (cond
    ((null toks) nil)
    (t (parse-list-v2 toks '()))))

(defun open? (c)
  (char= #\[ c))

(defun close? (c)
  (char= #\] c))

(defun is-delim? (c)
  (or (char= #\[ c)
      (char= #\] c)))


(defun is-sep? (c)
  (char= #\, c))

(defun add1 (x)
  (+ x 1))

(defun tokenize (s)
  (let ((slen (length s)))
    (labels ((scan (start)
               ;; (uiop:println start)
               (cond
                 ((>= start slen) (values nil nil))
                 ((delim? (char s start)) (values (string (char s start)) (1+ start)))
                 (t (labels ((lp (start curr)
                               (cond
                                 ((delim? (char s start)) (values curr start))
                                 (t (lp (1+ start) (uiop:strcat curr (string (char s start))))))))
                      (lp start "")))
                 ))
             (mloop (tok next agg)
               (cond
                 ((null tok) (reverse agg))
                 (t (multiple-value-bind (nextok next)
                        (scan next)
                      (if (equal "," tok)
                          (mloop nextok next agg)
                          (mloop nextok next (cons tok agg))))))))
      (multiple-value-call #'mloop (scan 0) '()))))

;; TODO: generic
(defun compare-atom (l r)
  (let ((li (parse-int l))
        (ri (parse-int r)))
    (cond
      ((< li ri) 1)
      ((= li ri) 0)
      ((> li ri) -1))))

(defun compare-list (left right)
  (cond
    ((and (null left) (null right)) 0)
    ((null right) -1)
    ((null left) 1)
    (t (+ (compare-elem (car left) (car right))
          (compare-list (cdr left) (cdr right)))
       )))

(defun compare-list-v2 (left right)
  (cond
    ((and (null left) (null right)) 0)
    ((null left) 1)

    ((null right) -1)

    (t (let ((res (compare-elem (car left) (car right))))
         (cond
           ((= -1 res) res)
           ((= 1 res) res)
           (t (compare-list-v2 (cdr left) (cdr right)))
           ))
       )))

(defun atomp (e)
  (cond
    ((listp e) nil)
    (t t)))

(defun compare-elem (left right)
  (cond
    ((and (atomp left)
          (atomp right))
     (compare-atom left right))
    ((and (listp left)
          (listp right))
     (compare-list-v2 left right))
    (t (compare-list-v2 (uiop:ensure-list left)
                        (uiop:ensure-list right)))))

(defun parse-data ()
  (parse-input "input.txt"
               (lambda (line)
                 (if (string= line "")
                     'newline
                     (parselist (tokenize line))
                     )
                 )))

(defun group-pair (lst)
  (cond
    ((null lst) '())
    (t (cons  (list (first lst)
                    (second lst))
              (group-pair (cddr lst))))))

(defun pairs ()
  (group-pair  (remove-if (lambda (x) (eql 'newline x))
                          (parse-data))))

(defun filter-with-index (pred lst)
  (labels ((lp (index lst agg)
             (cond
               ((null lst) agg)
               ((funcall pred (car lst)) (lp (add1 index) (cdr lst) (cons index agg)))
               (t (lp (add1 index) (cdr lst) agg)))))
    (lp 1 lst '())))

(defun puzzle1 ()
  (filter-with-index (lambda (tree) (not (= -1 tree)))
                     (mapcar (lambda (e) (compare-list-v2 (first e) (second e)))
                             (pairs))))

(defun puzzle-in ()
  (mapcar (lambda (e) (compare-list-v2 (first e) (second e)))
          (pairs)))
