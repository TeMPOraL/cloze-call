;;;; Test Game State - used to test game state manager and state mechanics.
(in-package :trc.cloze-call)

(defclass test-game-state (game-state)
  ())

(defmethod initialize-state ((game-state test-game-state)
                             gsm)
  (declare (ignore game-state gsm))
  (format t "test-game-state - init~%"))

(defmethod deinitialize-state ((game-state test-game-state)
                               gsm)
  (declare (ignore game-state gsm))
  (format t "test-game-state - deinit~%"))

(defmethod update-logic ((game-state test-game-state)
                         gsm
                         dt)
  (declare (ignore game-state gsm dt)))

(defmethod render ((game-state test-game-state)
                   gsm)
  (declare (ignore game-state gsm))
  (sdl:clear-display sdl:*blue*))
