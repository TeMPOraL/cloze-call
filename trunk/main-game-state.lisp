;;;; Main Game State - handles the core game simulation and rules
(in-package trc.cloze-call)

(load "game-objects.lisp") ; game classes
(load "levels.lisp") ; level handling

(defclass main-game-state (game-state)
  ((celestial-bodies
    :initform ()
    :documentation "List of celestial bodies that affect the ball.")
   (ball
    :initform nil
    :documentation "The ball that player plays with.")
   (hole
    :initform nil
    :documentation "The hole that player needs to shot into.")
   (simulation-running
    :initform nil
    :documentation "Flag - true if we should do physics.")
   (background-image
    :initform nil
    :documentation "Image used as the level background.")
   (next-level
    :initarg :run-level
    :initform (error "A default level must be specified!")
    :documentation "Level that will be loaded as next.")))

(defmethod initialize-state ((game-state main-game-state)
                             gsm)
  (declare (ignore gsm))
  (with-slots (celestial-bodies ball hole simulation-running
                                background-image next-level)  game-state
    
    (load-level next-level)
    ;; * Load planets from level
    (setf celestial-bodies (level-get-celestial-bodies))
    ;; * Load other important objects
    (setf background-image (load-image (level-get-background-image-name)))

    (setf ball (level-get-ball))
    (setf hole (level-get-hole))

    (setf simulation-running t)))

(defmethod deinitialize-state ((game-state main-game-state)
                               gsm)
  (declare (ignore game-state gsm)))

(defun apply-force (ball force dt)
  (with-slots (position velocity mass) ball
    (setf velocity (add-vectors velocity
                                (scaled-vector force
                                               (/ dt
                                                  ;mass))))
                                                  1))))
    (setf position (add-vectors position
                                (scaled-vector velocity
                                               dt)))))

(defun compute-gravity-field (bodies ball)
  (with-slots (position) ball
    (reduce (lambda (total body) (add-vectors total
                                              (scaled-vector
                                               (normalized-vector (add-vectors position (negative-vector (slot-value body 'position))))
                                               (/ (slot-value body 'mass)
                                                  (square (distance-between-vectors position
                                                                                    (slot-value body 'position)))))))
            bodies
            :initial-value (make-vector-2d 0.0 0.0))))

(defun collisions-p (celestial-bodies ball)
  "Returns true if the ball collides with any planet"
  (not (null (find-if
         (lambda (body) (< (distance-between-vectors (slot-value body 'position)
                                                     (slot-value ball 'position))
                           (+ (slot-value body 'radius)
                              (slot-value ball 'radius))))
         celestial-bodies))))

(defun offworld-p (celestial-bodies ball)
  "Returns true if the ball is far outside game world and has no chance of gettiing back in any reasonable amount of time."
  (declare (ignore celestial-bodies ball))
  nil)

;;; MAIN GAME STATES
;;; Main game has been separated to several game states:
;;; * Beginning/aiming
;;; * Simulation
;;; * Hole collision
;;; * Planet collision
;;; * Off-world
;;;
;;; Each of these states gets its own pair of update/render functions.

;;; Beginning / aiming
(defun update-aiming (game-state dt)
  (declare (ignore game-state dt)))

(defun render-aiming (game-state)
  (declare (ignore game-state)))

;;; Simulation
(defun update-simulation (game-state dt)
  (declare (ignore game-state dt)))

(defun render-simulation (game-state)
  (declare (ignore game-state)))

;;; Hole collision
(defun update-hole-collision (game-state dt)
  (declare (ignore game-state dt)))

(defun render-hole-collision (game-state)
  (declare (ignore game-state)))

;;; Planet collision
(defun update-planet-collision (game-state dt)
  (declare (ignore game-state dt)))

(defun render-planet-collision (game-state)
  (declare (ignore game-state)))

;;; Off-world
(defun update-off-world (game-state dt)
  (declare (ignore game-state dt)))

(defun render-off-world (game-state)
  (declare (ignore game-state)))

;;; GENERIC UPDATE/RENDER FUNCTIONS BELOW
(defmethod update-logic ((game-state main-game-state)
                         gsm
                         dt)
  (declare (ignore gsm))
  (with-slots (celestial-bodies ball hole simulation-running) game-state
    (if simulation-running
        (progn
          (apply-force ball
                       (scaled-vector
                        (compute-gravity-field celestial-bodies ball)
                                        ;(* +G+ (slot-value ball 'mass)))
                        +G+)
                       dt)
          (if (collisions-p celestial-bodies ball)
            ;(disable-simulation game-state)))))
              (setf simulation-running nil))))))
; TODO handle ball-hole collision

(defmethod render ((game-state main-game-state)
                   gsm)
  ;;TODO:
  ;; * Draw background
  ;; * Draw game objects
  ;; * Draw foreground / overlays
  (declare (ignore gsm))
  (with-slots (background-image celestial-bodies hole ball) game-state
    (sdl:clear-display sdl:*black*)
    (draw-image background-image)
    (map nil #'draw celestial-bodies)
    (draw hole)
    (draw ball))) ; TODO draw UI
