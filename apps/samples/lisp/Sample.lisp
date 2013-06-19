(defun f (a b c d)
     "Sum of multiplications"
     (+ (* a b) (* c d)))

(defun rec (a n)
     "Recursive function"
     (if (/= n 0)
           (+ a (rec a (- n 1)))
           0))

(defun recter (a n s)
      "Terminal recursive function"
      (if (/= n 0)
         (recter a (- n 1) (+ s a))
         s))

(defun recf (f init a n)
     "Recursive higher-order function"
     (if (/= n 0)
           (f (recf f init a (- n 1)) a)
           init))

(defun sum (xs)
     "Sum numbers in the list"
     (if (List.null xs)
           0
           (+ (List.head xs) (sum (List.tail xs)))))

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

(defun potrf' (m)
   (transpose' (potrf'' (transpose' m))))

(defun zipWithTriangular (f n a b)
   (List.concat 
      (List.take n a)
      (zipWith f (List.drop n a) (List.drop n b))))

(defun zipWithTriangular2D (f n a b)
   (if (empty a)
      a
      (List.cons
         (zipWithTriangular f n (List.head a) (List.head b))
         (zipWithTriangular2D f (+ n 1) (List.tail a) (List.tail b)))))

(defun empty (m)
   (or 
      (List.null m)
      (List.null (List.head m))))

(defun singleton (m)
   (and 
      (List.null (List.tail m))
      (List.null (List.tail (List.head m)))))

(defun potrf'' (m)
   (let* (
      (a (List.head (List.head m)))
      (b (List.tail (List.head m)))
      (c (map List.tail (List.tail m)))
      (d (map List.head (List.tail m)))
      (a' (potrf a))
      (b' (map (lambda (x) (trsm x a')) b))
      (c' (crossWith sgemm b' b'))
      (c'' (zipWithTriangular2D sub 0 c c'))
      )
         (if (singleton m)
            '('(a'))
            (List.cons (List.cons a' b') (zipWith List.cons d (potrf'' c''))))))


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
   (List.reduce add (zipWith mul xs ys)))

(defun transpose' (xs)
   (if (List.null (List.head xs))
      '()
      (List.cons 
         (map List.head xs)
         (transpose' (map List.tail xs)))))

(defun matmul (xs ys)
   (crossWith dotProduct xs (transpose' ys)))
