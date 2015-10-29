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
    (list 0 (- (rph)) (ph))
    (list 0 (rph) (- (ph)))
    (list 0 (- (rph)) (- (ph)))
    (list (rph) (ph) 0)
    (list (- (rph)) (ph) 0)
    (list (rph) (- (ph)) 0)
    (list (- (rph)) (- (ph)) 0)
    (list (ph) 0 (rph))
    (list (- (ph)) 0 (rph))
    (list (ph) 0 (- (rph)))
    (list (- (ph)) 0 (- (rph)))
    )
  )

(defun truncked_icosahedron_vertex ()
  (list
   (list )
   )
  )


(defvar *center* '(0 0 0))

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

(defun acad_draw_cmd (edges)
  "Generate ACDA draw command draw the lines"
  (loop for edge in edges do
       (format t "(command \"line\" '~S '~S \"\")~%" (car edge) (car (cdr edge)))
       ))

(defun barycentric_normalize (x)
  (let ((sum (apply '+ x)))
    (mapcar #'(lambda(x) (setf x (/ x sum))) x)
    ))

(defun circumcenter(triangle_point)
  "triangle point is a list as ((Ax Ay Az) (Bx By Bz) (Cx Cy Cz))
 return the center point (Ox Oy Oz)"
  (let* ((pa (elt triangle_point 0))
         (pb (elt triangle_point 1))
         (pc (elt triangle_point 2))
         (c (expt (distance pa pb) 2))
         (a (expt (distance pb pc) 2))
         (b (expt (distance pc pa) 2))
         (barycentric)
         (trans)
         (carte)
         )
    ;;calculate barycentric coordiation
    (setq barycentric (barycentric_normalize
                       (list (* a (+ (- a) b  c ))
                             (* b (+ a  (- b) c ))
                             (* c (+ a  b (- c))))))
    ;;transform from bartcentic to cartesion coordiation
    (mapc #'(lambda(&rest x) (setf trans (append trans (list x)))) pa pb pc)
    (loop for te in trans do
         (setf carte (append carte (list (apply '+ (mapcar #'* barycentric te)))))
         )
    (return-from circumcenter carte)
    ))

(defun point_in_list (pt pt_list)
  (loop for p in pt_list
     if (< (distance pt p) 0.001)
     do (return pt)
     finally
       (return nil)
       )
  )

(defun edges_to_vertex (edges)
  (let ((vertex nil))
    (loop for edge in edges do

         (loop for pt in edge do
              (if (and vertex (point_in_list pt vertex)) ()
                  (push pt vertex))
              ))
    (setq vertex vertex)
    ))

(defun sibling_vertex_on_edge_vertex(vertex edge edges)
  (let ((pts ) (sibling_edge))
    (loop for e in edges do
         (if (find vertex e :test #'equal) (push e sibling_edge)))
    (setf sibling_edge (remove edge sibling_edge :test #'equal))
    (setf pts (edges_to_vertex sibling_edge))
    (setf pts (remove vertex pts :test #'equal))
    ))


(defun generate_sub_edges (edge edges)
  (let* ((pts (reverse (edges_to_vertex (list edge)))) ;;use reverse to restore the order of points
         (vtx)
         (centers) (sub_point) (sub_edge) (len)
         (radius (distance *center* (car pts)))
         )
    (setf vtx (sibling_vertex_on_edge_vertex (car pts) edge edges))
    (setf centers (list (circumcenter (append pts (list (car vtx))))))
    (setf centers (append centers (list (circumcenter (append pts (cdr vtx))))))
    (setf len (/ (distance (car edge) (car (cdr edge))) 2))
                                        ;(format t "~S,~S   ~D~%" (car edge) (car (cdr edge)) len)
                                        ;(format t "center:~S~%" centers)
    (loop for c in centers
       do
         (loop for p in pts
            do
              (setf sub_point (append sub_point (list (project_circle_point (split_point p  c (split_ratio len (distance p (project_circle_point c radius)) radius) ) radius))))
            finally (progn

                      (setf sub_edge (append sub_edge (list sub_point)))
                      (format t "~S  ~D ~D~%" sub_point (distance (car sub_point) (car (cdr sub_point))) (distance (car pts) (car sub_point)))
                      (setf sub_point ()))
              )
       finally (return sub_edge)
         )

    ))

(defun split_sphere (edges)
  (let* ((result))
    (loop for edge in edges do
         (setf result (append result (generate_split_edge edge edges)))
         )
    (setf result (append result edges))
    (edges_to_vertex result)
    )
  )

(defun generate_split_edge (edge edges)
  (let* (
         (sibling_vtx (sibling_vertex_on_edge_vertex (car edge) edge edges))
         (a (circumcenter (append edge (list (car sibling_vtx)))))
         (b (circumcenter (append edge (list (cadr sibling_vtx)))))
         (radius (distance (car edge) *center*))
         )
    (triple_split_chord (mapcar #'(lambda(x) (project_circle_point x  radius))  (list a b)))
    ))

(defun split_sphere_3x (edges)
  (let* ((result))
    (loop for edge in edges do
         (setf result (append result (list (generate_split_edge edge edges))))
         )
    (edges_to_vertex result)
    )
  )

(defun angle (center pa pb)
  "Calcult the angle of center by the law of cosines, center/a/b are triangle's three point, each point denote as (x y z)"
  (let ((a (distance center pa))
        (b (distance center pb))
        (c (distance pa pb)))
    (acos (/ (+ (expt a 2) (expt b 2) (- (expt c 2))) (* 2 a b)))
    ))

(defun split_point (a b ratio)
  "return the split point coordinate. for line AB, split by C,
 ratio = AC/CB , a and b denote as (x y z)"
  (mapcar #'(lambda (x y) (/ (+ x (* y ratio)) (+ 1 ratio))) a b)
  )



(defun project_circle_point (x r)
  "project the point x (X Y Z) to the sphere surface with radiant r"
  (let* ((ratio (/ r (distance x *center*)))
         )
    (mapcar #'(lambda(x) (* x ratio)) x)
    ))

(defun chord_to_angle(size r)
  "return the angle of a chord span in a circle with radius r"
  (* (asin (/ size (* 2 r))) 2)
  )

(defun split_ratio (target_chord_size init_chord_size r)
  "calculate the ratio,  of which the desired edge length map to an initial chord"
  (let* ((target_angle  (chord_to_angle target_chord_size r))
         (init_angle (chord_to_angle init_chord_size r))
         (left_angle (- init_angle target_angle)))
    (- (/ (tan init_angle)  (tan left_angle)) 1)
    ))

(defun edge_length (edge)
  (distance (car edge) (car (cdr edge)))
  )


(defun triple_split_chord (edge)
  "Given two ponit of chord, retune the two points in the circle,that triple split the angle of chord spanned. note: will fail when the angle is pi"
  (let* (
         (radius (distance (car edge) *center*))
         (angle (chord_to_angle (edge_length edge) radius))
         (side_angle (/ (- pi angle) 2))
         (part_len (* (/ radius (sin (- pi (/ angle 3) side_angle))) (sin (/ angle 3))))
         (full_len (distance (car edge) (cadr edge)))
         (point_a (split_point (car edge) (cadr edge) (/ part_len (- full_len part_len))))
         (point_b (split_point (car edge) (cadr edge) (/ (- full_len part_len) part_len))))
    (list (project_circle_point point_a radius) (project_circle_point point_b radius))
    )
  )
