;;;; Game objects - defines all in-game objects
;;;; for Main Game State.
(in-package :trc.cloze-call)

;;; Game Object class
(defclass game-object ()
  ((position
    :initarg :position
    :documentation "Spatial (2D) position of this game object.");TODO :initform vector
   (velocity
    :initarg :velocity
    :documentation "Spatial (2D) velocity of this game object.")
   (forces
    :initarg :sum-of-forces
    :documentation "Sum of all forces that are applied to this object in current frame"); TODO remove initializer, add default 0-vector
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
    :documentation "2D dimmensions for image of this object in pixels. Used in drawing.")))

;;; Game Object functions
(defgeneric update (object dt)
  (:documentation "Update object properties using deltaTime."))

(defgeneric draw (object)
  (:documentation "Draw game object on screen."))

(defgeneric collide-p (object1 object2)
  (:documentation "Checks collision between two game objects."))

;;; Celestial Body class - defines objects that represent celestial bodies,
;;; which generate gravity on the map.
(defclass celestial-body (game-object) 
  () )

;;; Ball class - defines the ball that player has to shoot into
;;; a hole.
(defclass ball (game-object)
  () )

;;; Hole class - defines the target for player to shoot ball at.
(defclass hole (game-object)
  () )