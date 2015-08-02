(defun add (a b)
  (let ((x (+ a b)))
    (format t "The sum is ~a" x)
    x))

(defmacro let1 (var val &body body)
  `(let ((,var ,val))
     ,@body))

(let1 foo (+ 2 3)
  (* foo foo))

(defun mac-add (a b)
  (let1 x (+ a b)
    (format t "The sum is ~a" x)
    x))

(defmacro split (val yes no)
  (let1 g (gensym)
    `(let1 g ,val
       ;; (print g)
       (if g
           (let ((head (car g))
                 (tail (cdr g)))
             ,yes)
           ,no))))

(defun my-length (lst)
  (labels ((f (lst acc)
             (split lst
                    (f tail (1+ acc))
                    acc)))
    (f lst 0)))

(let1 x 100
  (split '(2 3)
         (+ x head)
         nil))

;; (defun my-length (lst)
;;   (labels ((f (lst acc)
;;              (split lst
;;                     (f tail (1+ acc))
;;                     acc)))
;;     (f lst 0)))

(defun pairs (lst)
  (labels ((f (lst acc)
             (split lst
                    (if tail
                        (f (cdr tail) (cons (cons head (car tail)) acc))
                        (reverse acc))
                    (reverse acc))))
    (f lst nil)))

(defmacro recurse (vars &body body)
  (let1 p (pairs vars)
    `(labels ((self ,(mapcar #'car p)
                ,@body))
       (self ,@(mapcar #'cdr p)))))

;; (recurse (n 9)
;;   (fresh-line)
;;   (if (zerop n)
;;       (princ "lift-off!")
;;       (progn (princ n)
;;              (self (1- n)))))

;; (defun my-length (lst)
;;   (recurse (lst lst acc 0)
;;     (split lst
;;            (self tail (1+ acc))
;;            acc)))

(defun my-length (lst)
  (reduce (lambda (x i)
            (1+ x))
          lst
          :initial-value 0))
