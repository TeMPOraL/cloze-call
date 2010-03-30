;;;; Game objects - defines all in-game objects
;;;; for Main Game State.
(in-package :trc.cloze-call)

;;; Game Object class
(defclass game-object ()
  ((position
    :initarg :position
    :documentation "Spatial (2D) position of this game object.");TODO :initform vector
   (mass
    :initarg :mass
    :initform 0
    :documentation "Physical mass of this game object.")
   (radius
    :initarg :radius
    :initform 0
    :documentation "Spatial radius of this object. Used in physics / collision detection.")
   (size
    :initarg :dimmensions
    :documentation "2D dimmensions for image of this object in pixels. Used in drawing.")
   (image
    :initarg :image
    :documentation "Image that will be drawn as a representation of this object.")))

;;; Game Object functions
(defgeneric init (object)
  (:documentation "Initialize object - give it a chance to load gfx, ect."))

(defgeneric deinit (object)
  (:documentation "Deinitialize object - give it a chance to free resources, ect."))

(defgeneric update (object dt)
  (:documentation "Update object properties using deltaTime."))

(defgeneric draw (object)
  (:documentation "Draw game object on screen."))

(defgeneric collide-p (object1 object2)
  (:documentation "Checks collision between two game objects."))

;;; Default implementations of methods.

(defmethod draw (something)
;  (format t "Doing some drawing.")
  (print something)
  (error "Something is drawn, that shouldn't be."))

(defmethod draw ((object game-object))
  (with-slots (position image) object
    (draw-image image :position position)))

(defmethod deinit ((object game-object))
  (declare (ignore object)))

;;; Celestial Body class - defines objects that represent celestial bodies,
;;; which generate gravity on the map.
(defclass celestial-body (game-object) 
  () )

(defmethod init ((object celestial-body))
  (setf (slot-value object 'image)
        (load-image "planet1.bmp" :color-key-at #(0 0))))


;;; Ball class - defines the ball that player has to shoot into
;;; a hole.
(defclass ball (game-object)
  ((velocity
    :initarg :velocity
    :initform (make-vector-2d 0 0)
    :documentation "Spatial (2D) velocity of this game object.")
   (forces
    :initform (make-vector-2d 0 0)
    :documentation "Sum of all forces that are applied to this object in current frame")))


(defmethod init ((object ball))
  (setf (slot-value object 'image)
        (load-image "ball.bmp")))

;;; Hole class - defines the target for player to shoot ball at.
(defclass hole (game-object)
  () )

(defmethod init ((object hole))
  (setf (slot-value object 'image)
        (load-image "hole.bmp")))
