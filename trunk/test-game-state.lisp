;;;; Test Game State - used to test game state manager and state mechanics.
(in-package :trc.cloze-call)

(defclass test-game-state (game-state)
  ((test-picture
    :initform nil
    :documentation "Test bitmap to draw on-screen.")
   (picture-position
    :initform (make-vector-2d))))

(defmethod initialize-state ((game-state test-game-state)
                             gsm)
  (declare (ignore gsm))
  (with-slots (test-picture) game-state
    (setf test-picture (sdl:load-image
                        (merge-pathnames "lisp.bmp"
                                         +gfx-asset-path+)
                        :color-key-at #(0 0))))
  (format t "test-game-state - init~%"))

(defmethod deinitialize-state ((game-state test-game-state)
                               gsm)
  (declare (ignore game-state gsm))
  (format t "test-game-state - deinit~%"))

(defmethod update-logic ((game-state test-game-state)
                         gsm
                         dt)
  (declare (ignore gsm dt)))

(defmethod render ((game-state test-game-state)
                   gsm)
  (declare (ignore gsm))
  (sdl:clear-display sdl:*blue*)
  (with-slots (test-picture picture-position) game-state
    (sdl:draw-surface-at test-picture picture-position)))
