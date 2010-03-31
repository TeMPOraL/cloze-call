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
  (with-slots (big-picture-name big-picture accumulator) game-state
    (setf accumulator 0.0) ; zero the time accumulator
    (setf big-picture (load-image big-picture-name :color-key-at #(0 0)))))

(defmethod deinitialize-state ((game-state screen-game-state)
                               gsm)
  (declare (ignore game-state gsm)))

;;; TODO add fade-ins and fade-outs

(defmethod update-logic ((game-state screen-game-state)
                         gsm
                         dt)
  (with-slots (accumulator time next-state) game-state
    (setf accumulator (+ accumulator dt))
    (if (> accumulator time)
        (change-state gsm next-state)
        t)))

(defmethod render ((game-state screen-game-state)
                   gsm)
  (declare (ignore gsm))
  (sdl:clear-display sdl:*black*)
  (with-slots (big-picture picture-position) game-state
    (draw-image big-picture)))
