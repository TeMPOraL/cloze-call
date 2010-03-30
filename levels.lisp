;;;; Level handling. Possible source of Ugly Hacks.

(defun load-level (level-name)
  (format t "Loading level ~A ok!~%" level-name))

(defun level-get-background-image-name ()
  "level-background.bmp")

(defmacro with-game-objects-init (&body body)
;  `(mapcar #'init (list ,@body)))
  `(mapcar (lambda (what) (init what) what) (list ,@body)))

(defun level-get-celestial-bodies ()
  (with-game-objects-init
      (make-instance 'celestial-body
                     :position (make-vector-2d 250 250)
                     :mass 0
                     :radius 32
                     :dimmensions 64)
    (make-instance 'celestial-body
                   :position (make-vector-2d 500 500)
                   :mass 0
                   :radius 20
                   :dimmensions 40)))

(defun level-get-ball ()
  (car ; CAR - because this macro returns a list. FIXME looks like a hack
   (with-game-objects-init
       (make-instance 'ball
                      :position (make-vector-2d 400 300)
                      :mass 100000
                      :radius 16
                      :dimmensions 32))))

(defun level-get-hole ()
  (car ; CAR - because this macro returns a list. FIXME looks like a hack
   (with-game-objects-init
       (make-instance 'hole
                      :position (make-vector-2d 100 50)
                      :mass 0
                      :radius 16
                      :dimmensions 32))))