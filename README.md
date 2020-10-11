# Fibonacci numbers
Compare algorithms for computing [Fibonacci numbers](https://en.wikipedia.org/wiki/Fibonacci_number).

## Recursive with computational complexity O(2^N)

This algorithm implements the formal definition of the Fibonacci
numbers.

F(0) = 0
F(1) = 1
F(n) = F(n-1) + F(n-1)

```lisp
(defun fib-1 (n)
  "Simple recursive
Computational complexity O(2^n)"
  (cond ((zerop n) 0)
        ((= 1 n) 1)
        (t (+ (fib-1 (1- n)) (fib-1 (- n 2))))))
```

| n  | time     | memory |
|----|----------|--------|
| 1  | 5e-6     | 0      |
| 2  | 5e-6     | 0      |
| 5  | 6e-6     | 0      |
| 10 | 6e-6     | 0      |
| 20 | 85e-6    | 0      |
| 30 | 9.622e-3 | 0      |
| 31 | 15.76e-3 | 0      |
| 32 | 24.74e-3 | 0      |
| 33 | 39.25e-3 | 0      |
| 34 | 61.70e-3 | 0      |
| 35 | 98.11e-3 | 0      |
| 36 | 157.4e-3 | 0      |
| 37 | 250.4e-3 | 0      |
| 38 | 404.3e-3 | 0      |
| 39 | 651.2e-3 | 0      |
| 40 | 1.060    | 0      |
| 41 | 1.702    | 0      |
| 42 | 2.750    | 0      |
| 43 | 4.456    | 0      |
| 44 | 7.258    | 0      |


## Computational complexity O(N)

### Tail recursive 

```lisp
(defun fib-2 (n &optional (a 0) (b 1))
  "Tail recursive
Computational complexity O(n)"
  (if (zerop n)
      a
      (fib-2 (1- n) b (+ a b))))
```

|       | time     | memory  |
|-------|----------|---------|
| 1     | 5e-6     | 0       |
| 2     | 5e-6     | 0       |
| 5     | 5e-6     | 0       |
| 10    | 5e-6     | 0       |
| 20    | 5e-6     | 0       |
| 50    | 6e-6     | 0       |
| 87    | 6e-6     | 0       |
| 88    | 6e-6     | 16      |
| 89    | 8e-6     | 48      |
| 90    | 9e-6     | 80      |
| 100   | 12e-6    | 400     |
| 200   | 20e-6    | 3.600e3 |
| 500   | 53e-6    | 18.88e3 |
| 1000  | 131e-6   | 6.128e3 |
| 2000  | 550e-6   | 211.6e3 |
| 5000  | 2.612e-3 | 1.181e6 |
| 10000 | 9.251e-3 | 1.537e6 |

### Loop

```lisp
(defun fib-3 (n)
  "Loop
Computational complexity O(n)"
  (do ((a 0 b)
       (b 1 (+ a b)))
      ((zerop n) a)
    (decf n)))
```

| n     | time     | memory  |
|-------|----------|---------|
| 1     | 5e-6     | 0       |
| 2     | 6e-6     | 0       |
| 5     | 6e-6     | 0       |
| 10    | 5e-6     | 0       |
| 20    | 5e-6     | 0       |
| 50    | 6e-6     | 0       |
| 87    | 5e-6     | 0       |
| 88    | 6e-6     | 16      |
| 89    | 8e-6     | 48      |
| 90    | 9e-6     | 80      |
| 100   | 10e-6    | 400     |
| 200   | 18e-6    | 3.600e3 |
| 500   | 51e-6    | 18.88e3 |
| 1000  | 147e-6   | 6.128e3 |
| 2000  | 548e-6   | 211.6e3 |
| 5000  | 2.893e-3 | 1.183e6 |
| 10000 | 9.990e-3 | 1.537e6 |

## Computational complexity O(log N)

### Matrix exponentiation by squaring

```lisp
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
```

| n     | time   | memory |
|-------|--------|--------|
| 1     | 7e-6   | 112    |
| 2     | 11e-6  | 336    |
| 5     | 11e-6  | 560    |
| 10    | 12e-6  | 784    |
| 20    | 12e-6  | 1008   |
| 50    | 13e-6  | 1120   |
| 87    | 17e-6  | 1632   |
| 88    | 17e-6  | 1792   |
| 89    | 17e-6  | 1664   |
| 90    | 18e-6  | 1840   |
| 100   | 17e-6  | 2016   |
| 200   | 23e-6  | 2672   |
| 500   | 28e-6  | 4928   |
| 1000  | 35e-6  | 7488   |
| 2000  | 44e-6  | 11280  |
| 5000  | 81e-6  | 24000  |
| 10000 | 189e-6 | 43920  |

### Recursive calculation by identity

F(2n)   = 2 * F(n) * F(n+1) - (F(n))^2
F(2n+1) = (F(n))^2 + (F(n+1))^2

```lisp
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
 ```
 
|       | time  | memory |
|-------|-------|--------|
| 1     | 6e-6  | 0      |
| 2     | 7e-6  | 0      |
| 5     | 7e-6  | 0      |
| 10    | 7e-6  | 0      |
| 20    | 7e-6  | 0      |
| 50    | 7e-6  | 0      |
| 87    | 7e-6  | 0      |
| 88    | 10e-6 | 96     |
| 89    | 11e-6 | 128    |
| 90    | 13e-6 | 144    |
| 100   | 12e-6 | 192    |
| 200   | 15e-6 | 448    |
| 500   | 16e-6 | 896    |
| 1000  | 18e-6 | 1568   |
| 2000  | 21e-6 | 2720   |
| 5000  | 29e-6 | 6000   |
| 10000 | 45e-6 | 11344  |
