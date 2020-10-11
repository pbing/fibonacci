;;;; Compare algorithms for computing Fibonacci numbers

(defun fib-1 (n)
  "Simple recursive
Computational complexity O(2^n)"
  (cond ((zerop n) 0)
        ((= 1 n) 1)
        (t (+ (fib-1 (1- n)) (fib-1 (- n 2))))))

(defun fib-2 (n &optional (a 0) (b 1))
  "Tail recursive
Computational complexity O(n)"
  (if (zerop n)
      a
      (fib-2 (1- n) b (+ a b))))

(defun fib-3 (n)
  "Loop
Computational complexity O(n)"
  (do ((a 0 b)
       (b 1 (+ a b)))
      ((zerop n) a)
    (decf n)))

(defun fib-4 (n)
  "Exponentiation by squaring
Computational complexity O(log n)"
  (if (zerop n)
      0
      (let ((res (make-array '(2 2) :initial-contents '((1 1) (1 0)))))
        (do ((k (1- n) (truncate k 2))
             (x res (square-2x2 x)))
            ((zerop k) (aref res 0 1))
          (when (oddp k)
            (setf res (mult-2x2 res x)))))))

(defun fib-5 (n)
  "An efficient recursive algorithm for computing a Fibonacci number.
   f (2n) = 2 * f n * f (n+1) - (f n)^2
   f (2n+1) = (f n)^2 + (f (n+1))^2
http://www.vex.net/~trebla/haskell/calculator/colour/Calculator.html#fib
Computational complexity O(log n)"
  (if (zerop n)
      (values 0 1)
      (multiple-value-bind (fn fn+1) (fib-5 (truncate n 2))
        (let* ((fn^2   (* fn fn))
               (fn+1^2 (* fn+1 fn+1))
               (f2n    (- (* 2 fn fn+1) fn^2))
               (f2n+1  (+ fn^2 fn+1^2)))
          (if (evenp n)
              (values f2n f2n+1)
              (values f2n+1 (+ f2n f2n+1)))))))


(defun mult-2x2 (m1 m2)
  "Multiply two 2x2 matrices"
  (let ((res (make-array '(2 2))))
    (setf (aref res 0 0) (+ (* (aref m1 0 0) (aref m2 0 0)) (* (aref m1 0 1) (aref m2 1 0)))
          (aref res 0 1) (+ (* (aref m1 0 0) (aref m2 0 1)) (* (aref m1 0 1) (aref m2 1 1)))
          (aref res 1 0) (+ (* (aref m1 1 0) (aref m2 0 0)) (* (aref m1 1 1) (aref m2 1 0)))
          (aref res 1 1) (+ (* (aref m1 1 0) (aref m2 0 1)) (* (aref m1 1 1) (aref m2 1 1))))
    res))

(defun square-2x2 (m)
  "Square a 2x2 matrix"
  (let* ((res (make-array '(2 2)))
         (a (aref m 0 0))
         (b (aref m 0 1))
         (c (aref m 1 0))
         (d (aref m 1 1))
         (s (+ (* a c) (* b d))))
    (setf (aref res 0 0) (+ (* a a) (* b b))
          (aref res 0 1) s
          (aref res 1 0) s
          (aref res 1 1) (+ (* c c) (* d d)))
    res))
