;;;; Level handling. Possible source of Ugly Hacks.

(defparameter *level-grid* ())

(defun reinitialize-level-grid () ;FIXME hardcoded
  (defparameter *level-grid* '( #(100 300) #(100 500)
                               #(300 100) #(300 300) #(300 500)
                               #(500 100) #(500 300) #(500 500)
                               #(700 100) #(700 300) #(700 500))))

(defun level-get-grid-coord ()
  (let ((coord (nth (random (length *level-grid*)) *level-grid*)))
    (setf *level-grid* (remove coord *level-grid*))
    (add-vectors coord (make-vector-2d (random +max-x-grid-deviation+) (random +max-y-grid-deviation+)))))

(defun load-level (level-name)
  (format t "Loading level ~A ok!~%" level-name)
  (reinitialize-level-grid))

(defun level-get-background-image-name ()
  "level-background.png")

(defmacro with-game-objects-init (&body body)
;  `(mapcar #'init (list ,@body)))
  `(mapcar (lambda (what) (init what) what) (list ,@body)))

(defun random-small ()
  (= 0 (random 2)))

(defun make-celestial-body ()
  (let ((is-small (random-small)))
    (with-game-objects-init
        (make-instance 'celestial-body
                       :position (level-get-grid-coord)
                       :mass (+ +default-planet-mass+
                                (random +max-additional-planet-mass+))
                       :radius (if is-small +small-planet-radius+ +large-planet-radius+)
                       :small is-small
                       :dimmensions (if is-small
                                        +small-planet-dimmensions+
                                        +large-planet-dimmensions+)))))


(defun level-get-celestial-bodies ()
;  (with-game-objects-init
;      (make-instance 'celestial-body
;                     :position (make-vector-2d 250 250)
;                     :mass 500
;                     :radius 94
;                     :dimmensions (make-vector-2d 300 300))
;    (make-instance 'celestial-body
;                   :position (make-vector-2d 700 70)
;                   :mass 600
;                   :radius 94
;                   :dimmensions (make-vector-2d 300 300))
;    (make-instance 'celestial-body
;                   :position (make-vector-2d 500 500)
;                   :mass 300
;                   :small t
;                   :radius 47
;                   :dimmensions (make-vector-2d 150 150))))
  (let ((bodies ()))
    (dotimes (i (+ (random +additional-planet-cnt-max+) +default-planet-cnt+))
      (setf bodies (append bodies (make-celestial-body))))
    bodies))

(defun level-get-ball ()
  (car ; CAR - because this macro returns a list. FIXME looks like a hack
   (with-game-objects-init
       (make-instance 'ball
                      :position +default-ball-starting-position+;(level-get-grid-coord)
                      :mass 100000
                      :radius 11
                      :dimmensions (make-vector-2d 22 22)
                      :velocity (make-vector-2d)))))

(defun level-get-hole ()
  (car ; CAR - because this macro returns a list. FIXME looks like a hack
   (with-game-objects-init
       (make-instance 'hole
                      :position (level-get-grid-coord)
                      :mass 50
                      :radius 15
                      :dimmensions (make-vector-2d 48 48)))))