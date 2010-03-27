;;;; Screen game state - used to create Victory / Defeat / Intro game states.
(in-package trc.cloze-call)

(defclass screen-game-state (game-state)
  ((big-picture-name
    :initarg :picture-name
    :initform (error "Game screen needs to have a picture specified.")
    :documentation "Picture to be displayed on screen")
   (big-picture
    :documentation "Surface to be drawn on-screen.")
   (picture-position
    :initarg :picture-position
    :initform (make-vector-2d 0 0)
    :documentation "Position of picture on-screen as a 2D vector.")
   (time
    :initarg :screen-time
    :initform +default-game-screen-time+
    :documentation "How long (in sec) should the screen be visible.")
   (accumulator
    :initform 0
    :documentation "Accumulator counts time spent on showing this screen.")
   (next-state
    :initarg :next-state
    :initform nil
    :documentation "Next state to be called when this screen times out.")))

(defmethod initialize-state ((game-state screen-game-state)
                             gsm)
  (declare (ignore gsm))
  (with-slots (big-picture-name big-picture) game-state
    (setf big-picture (sdl:load-image
                       (merge-pathnames big-picture-name
                                        +gfx-asset-path+)))))

(defmethod deinitialize-state ((game-state screen-game-state)
                               gsm)
  (declare (ignore game-state gsm)))

(defmethod update-logic ((game-state screen-game-state)
                         gsm
                         dt)
  (declare (ignore gsm dt))
  ()); TODO - handle image processing and state chagne here!

(defmethod render ((game-state screen-game-state)
                   gsm)
  (declare (ignore gsm))
  (with-slots (big-picture picture-position) game-state
    (sdl:draw-surface-at big-picture picture-position)))
