;(defun f (a b c d)
;     "Sum of multiplications"
;     (+ (* a b) (* c d)))
;
;(defun rec (a n)
;     "Recursive function"
;     (if (/= n 0)
;           (+ a (rec a (- n 1)))
;           0))
;
;(defun recter (a n s)
;      "Terminal recursive function"
;      (if (/= n 0)
;         (recter a (- n 1) (+ s a))
;         s))
;
;(defun recf (f init a n)
;     "Recursive higher-order function"
;     (if (/= n 0)
;           (f (recf f init a (- n 1)) a)
;           init))
;
;(defun sum (xs)
;     "Sum numbers in the list"
;     (if (List.null xs)
;           0
;           (+ (List.head xs) (sum (List.tail xs)))))
;
(defun map (f xs)
     "Map f on xs"
     (if (List.null xs)
          '()
          (List.cons
                (f (List.head xs))
                (map f (List.tail xs)))))


(defun constant () 10)

(defun matrix () '(
   '(11 12 13 14 15 16 17 18 19)
   '(21 22 23 24 25 26 27 28 29)
   '(31 32 33 34 35 36 37 38 39)
   '(41 42 43 44 45 46 47 48 49)
   '(51 52 53 54 55 56 57 58 59)
   '(61 62 63 64 65 66 67 68 69)
   '(71 72 73 74 75 76 77 78 79)
   '(81 82 83 84 85 86 87 88 89)
   '(91 92 93 94 95 96 97 98 99)))

(defun matrix2 () '(
   '(11 12 13)
   '(21 22 23)
   '(31 32 33)))

(defun trimatrix () '(
   '(11 12 13 14 15 16 17 18 19)
   '(22 23 24 25 26 27 28 29)
   '(33 34 35 36 37 38 39)
   '(44 45 46 47 48 49)
   '(55 56 57 58 59)
   '(66 67 68 69)
   '(77 78 79)
   '(88 89)
   '(99)))

(defun trimatrix3 () '(
   '(11 12 13)
   '(22 23)
   '(33)))

(defun trimatrix4 () '(
   '(11 12 13 14)
   '(22 23 24)
   '(33 34)
   '(44)))

(defun trimatrix5 () '(
   '(11 12 13 14 15)
   '(22 23 24 25)
   '(33 34 35)
   '(44 45)
   '(55)))

(defun letsgo (a b)
   "Test for let"
   (let ((c (+ a b)))
      (+ c c)))

(defun letsgo* (a b)
   "Test for let*"
   (let* (
      (c (+ a b)) 
      (d (- a b))
      (e (+ c d)))
         (+ e e)))

(defun reclet (m n)
   (let* (
      (a (+ m b))
      (b n))
         (if (== n 0)
            (+ (reclet m 1000) (reclet m 500))
            a)))

(defun zipWith (f xs ys)
   (if (List.null xs)
      '()
      (if (List.null ys)
         '()
         (List.cons
            (f (List.head xs) (List.head ys))
            (zipWith f (List.tail xs) (List.tail ys))))))

(defun zipWith2D (f xs ys)
   (zipWith (zipWith f) xs ys))

(defun crossWith (f xs ys)
   (map (lambda (y)
      (map (lambda (x) (f x y))
         xs))
         ys))

(defun cholesky (m)
   (let* (
      (l (map (map (f l)) mIndexes))

      (mIndexes (zipWith2D (lambda (r idx) (List.cons r idx)) m indexes)))

      l))

(defun mIndexes ()
   (zipWith2D (lambda (r idx) (List.cons r idx)) m indexes))

(defun chol ()
   (map (map (f chol)) mIndexes))

(defun m ()
   (split 16 16 (mul t (transpose t))))

(defun f (l xs) 
   (let (
      (v (index 0 xs))
      (x (index 1 xs))
      (y (index 2 xs)))
         (f' (g x y v l) x y l)))


(defun f' (v x y l)
   (if (> x y) v
      (if (== x y) (potrf v)
         (trsm (cell x x l) v))))

(defun g (x y v l)
   (if (== x 0) v
      (sub v (h x y l))))


(defun h (x y l)
   (dotProduct
      (leftOf x y l)
      (map transpose (leftOf x x l))))

(defun leftOf (x y l)
   (take x (index y l)))

(defun cell (x y l)
   (index x (index y l)))

(defun vecIndexes (n)
   (List.cons n (vecIndexes (+ n 1))))

(defun indexes ()
   (crossWith (lambda (x y) '(x y)) (vecIndexes 0) (vecIndexes 0)))

(defun drop (n xs)
   (if (== 0 n) xs (drop (- n 1) (List.tail xs))))

(defun take (n xs)
   (take' n xs '()))
   
(defun take' (n xs ys)
   (if (== 0 n)
      (reverse ys)
      (take' (- n 1) (List.tail xs) (List.cons (List.head xs) ys))))

(defun concat (xs ys)
   (concat' (reverse xs) ys))

(defun concat' (xs ys)
   (if (List.null xs) ys
      (concat' (List.tail xs) (List.cons (List.head xs) ys))))

(defun index (n xs)
   (List.head (drop n xs)))

(defun reduce (f xs)
   (if (List.null xs) error
      (List.head (reduce' f xs))))

(defun reduce' (k xs)
   (if (or (List.null xs) (List.null (List.tail xs)))
      xs
      (reduce' k ((List.cons
         (f (List.head xs) (index 1 xs))
         (reduce' k (drop 2 xs)))))))


(defun reverse (xs)
   (reverse' xs '()))

(defun reverse' (xs rs)
   (if (List.null xs)
      rs
      (reverse' (List.tail xs) (List.cons (List.head xs) rs))))

(defun reduce (f xs)
   (if (List.null (List.tail xs))
      (List.head xs)
      (reduce f (List.cons 
         (f (List.head xs) (List.head (List.tail xs)))
         (List.tail (List.tail xs))))))

(defun dotProduct (xs ys)
   (reduce add (zipWith mul xs ys)))

(defun transpose' (xs)
   (if (List.null (List.head xs))
      '()
      (List.cons 
         (map List.head xs)
         (transpose' (map List.tail xs)))))

(defun matmul (xs ys)
   (crossWith dotProduct xs (transpose' ys)))
