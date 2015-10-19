(defun ph() (/ (+ (sqrt 5) 1) 2))
(defun rph() (/ 1 (ph)))
(defun dodecahedron_vertex ()
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
  "Return the distance of two point, in cartesion coordiation"
  (let ((x  (mapcar #'- pt1 pt2)))
    (sqrt (apply #'+ (mapcar #'* x x)))))


(defun min_edge_length(pts)
  "return the minimum distance of one to multi points"
  (let ((pt (car pts)) (ptlist (cdr pts)) (length '()))
    (loop for p in ptlist do (push (distance pt p)  length))
    (apply 'min length)))

(defun edge_from_vertex_step (pts size)
  "return the list of vertex that its length is equal to the inupt size"
  (let ((pt (car pts)) (ptlist (cdr pts)) (edges '()) (mins size))
    (loop
       for p in ptlist
       do (
           ;; treat the different less than one of 10th value as no diffence
           if (< (abs (- (distance pt p) mins)) (/ mins 10))
              (push (list pt p) edges)
              )
         )

    (return-from edge_from_vertex_step edges)
    ))

(defun edge_from_vertex (pts)
  "return the edge list of given vertex set"
  (let ((min_size (min_edge_length pts)) (edges '()) )
    (loop
       (setq edges (append edges (edge_from_vertex_step pts min_size)))
       (if (> (length pts) 2)
           (setf pts (cdr pts))
           (return-from edge_from_vertex edges)
           )
       )
    ))

