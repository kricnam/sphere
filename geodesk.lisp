(defun ph() (/ (+ (sqrt 5) 1) 2))
(defun rph() (/ 1 (ph)))
(defun dodecahedron ()  
"Genenrate dodecahedron base Cartesian coordinates"
( list
                         (list 1 1 -1)
                         (list 1 1 1)
                         (list 1 -1 1)
                         (list -1 1 1)
                         (list -1 -1 1)
                         (list 1 -1 -1)
                         (list -1 1 -1)
                         (list -1 -1 -1)
                         (list 0 (rph) (ph))
                         (list 0 (* -1 (rph)) (ph))
                         (list 0 (rph) (* -1 (ph)))
                         (list 0 (* -1 (rph)) (* -1 (ph)))
                         (list (rph) (ph) 0)
                         (list (* -1 (rph)) (ph) 0)
                         (list (rph) (* -1 (ph)) 0)
                         (list (* -1 (rph)) (* -1 (ph)) 0)
                         (list (ph) 0 (rph))
                         (list (* -1 (ph)) 0 (rph))
                         (list (ph) 0 (* -1 (rph)))
                         (list (* -1 (ph)) 0 (* -1 (rph)))
                   )
  )

(defun distance (pt1 pt2)
"Return the distance of two point, in 3D cartesion coordiation"
(let ((x  (mapcar #'- pt1 pt2)))
(sqrt (apply #'+ (mapcar #'* x x)))))


(defun min_line(pts) 
"get edge"
(let ((pt (car pts)) (ptlist (cdr pts)) (a))
  (loop for p in ptlist do (push (list (distance pt p)  p pt) a))
(print a)))










