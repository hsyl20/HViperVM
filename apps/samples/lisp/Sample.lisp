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
   '(10 9 8 7 6 5 4 3 2 1)
   '(10 9 8 7 6 5 4 3 2 1)
   '(10 9 8 7 6 5 4 3 2 1)
   '(10 9 8 7 6 5 4 3 2 1)
   '(10 9 8 7 6 5 4 3 2 1)
   '(10 9 8 7 6 5 4 3 2 1)
   '(10 9 8 7 6 5 4 3 2 1)
   '(10 9 8 7 6 5 4 3 2 1)
   '(10 9 8 7 6 5 4 3 2 1)
   '(10 9 8 7 6 5 4 3 2 1)))

(defun trimatrix () '(
   '(10 9 8 7 6 5 4 3 2 1)
   '(9 8 7 6 5 4 3 2 1)
   '(8 7 6 5 4 3 2 1)
   '(7 6 5 4 3 2 1)
   '(6 5 4 3 2 1)
   '(5 4 3 2 1)
   '(4 3 2 1)
   '(3 2 1)
   '(2 1)
   '(1)))

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
      (b'' (List.tail b'))
      (cOps (triangularize (crossWith sgemm b'' b'')))
      (c' (zipWith2D ap cOps (map List.tail c)))
      (c'' ((zipWith List.cons diag (List.snoc '() c'))))
      )
         (if (List.null m)
            '()
            (List.cons (List.cons a' b') c''))))
