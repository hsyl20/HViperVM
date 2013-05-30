(defun f (a b c d)
     "Sum of multiplications"
     (+ (* a b) (* c d)))

(defun rec (a n)
     "Recursive function"
     (if (/= n 0)
           (+ a (rec a (- n 1)))
           0))

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

(defun trimatrix2 () '(
   '(11 12 13)
   '(22 23)
   '(33)))

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

(defun tri (n m)
   (if (List.null m) 
      '()
      (let (
         (x (List.head m))
         (xs (List.tail m)))
            (List.cons
               (List.drop n x)
               (tri (+ n 1) xs)))))

(defun triangularize (m)
   (tri 0 m))

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

;(defun crossWith (f xs ys)
;   (map (lambda (y)
;      (map (lambda (x) (f x y))
;         xs))
;         ys))

(defun crossWith (f xs ys)
   (map (crossWith' f xs) ys))

(defun flip (f x y)
   (f y x))

(defun crossWith' (f xs y)
   (map (flip f y) xs))

(defun ap (a b)
   (a b))

(defun cholesky (m)
   (let* (
      (a (List.head (List.head m)))
      (b (List.tail (List.head m)))
      (c (List.tail m))
      (a' (potrf a))
      (b' (map (trsm a') b))
      (diag (zipWith syrk b' (map List.head c)))
      (cOps (triangularize (crossWith sgemm b' b')))
      (cOps' (map List.tail cOps))
      (c' (zipWith2D ap cOps' (map List.tail c)))
      (c'' ((zipWith List.cons diag (List.snoc c' '()))))
      )
         (if (List.null m)
            m
            (List.cons (List.cons a' b') (cholesky c'')))))
