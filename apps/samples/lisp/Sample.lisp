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

(defun matrix1 () '(
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
   (let ((c (+ a b)) (d (- a b)))
      (+ c d)))
