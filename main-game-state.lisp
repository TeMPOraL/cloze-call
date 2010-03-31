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
   (background-image
    :initform nil
    :documentation "Image used as the level background.")
   (state
    :initform :aiming
    :documentation "One of main-game substates.")
   (next-level
    :initarg :run-level
    :initform (error "A default level must be specified!")
    :documentation "Level that will be loaded as next.")
   (mouse-left-button-state
    :initform nil
    :documentation "Current state of left mouse button. T if pressed, NIL if depressed.")
   (mouse-prev-left-button-state
    :initform nil
    :documentation "State of left mouse button before last check. T if pressed, NIL if depressed.")
   (lives
    :initform 3
    :documentation "How many attempts the player has.")))

(defun rising-edge (signal-prev signal-curr)
  (and (null signal-prev) (not (null signal-curr))))

(defun falling-edge (signal-prev signal-curr)
  (and (not (null signal-prev)) (null signal-curr)))

(defun change-game-state (game-state new-state)
  (with-slots (state) game-state
    (setf state new-state)))

(defun lerp (a b param)
  (+ (* (- 1 param)
        a)
     (* param
        b)))

(defun lerp-sdl-colors (colorA colorB param)
  (multiple-value-bind (r1 g1 b1) (sdl:color-* colorA)
    (multiple-value-bind (r2 g2 b2) (sdl:color-* colorB)
      (sdl:color :r (lerp r1 r2 param)
                 :g (lerp g1 g2 param)
                 :b (lerp b1 b2 param)))))

(defmethod initialize-state ((game-state main-game-state)
                             gsm)
  (declare (ignore gsm))
  (with-slots (celestial-bodies ball hole simulation-running
                                background-image next-level
                                lives state)  game-state
    
    (load-level next-level)
    ;; * Load planets from level
    (setf celestial-bodies (level-get-celestial-bodies))
    ;; * Load other important objects
    (setf background-image (load-image (level-get-background-image-name)))

    (setf lives 3)
    (setf state :aiming)
    (setf ball (level-get-ball))
    (setf hole (level-get-hole))))

(defun reinitialize-game (game-state)
  (with-slots (ball) game-state
    (change-game-state game-state :aiming)
    (setf ball (level-get-ball)))) ; FIXME assumes that level doesn't do magic on ball position (ie. using grid coord list!

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

(defun collision-between-objects-p (object-one object-two)
  "Checks if two objects described by their centers and bounding sphere radii are colliding"
  (< (distance-between-vectors (slot-value object-one 'position)
                               (slot-value object-two 'position))
     (+ (slot-value object-one 'radius)
        (slot-value object-two 'radius))))

(defun collisions-p (celestial-bodies ball)
  "Returns true if the ball collides with any planet"
  (not (null (find-if
;         (lambda (body) (< (distance-between-vectors (slot-value body 'position)
;              (slot-value ball 'position))
;                           (+ (slot-value body 'radius)
;                              (slot-value ball 'radius))))
              (lambda (body) (collision-between-objects-p body ball))
              celestial-bodies))))

(defun offworld-p (celestial-bodies ball)
  "Returns true if the ball is far outside game world and has no chance of gettiing back in any reasonable amount of time."
  (declare (ignore celestial-bodies ball))
  nil)

(defun collided-with-hole-p (ball hole)
  (collision-between-objects-p ball hole))

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
(defun compute-velocity-from-position-and-mouse-pos (position)
  (scaled-vector (normalized-vector (add-vectors (sdl:mouse-position)
                                                 (negative-vector position)))
                 ;+default-ball-velocity+))
                 (distance-between-vectors position (sdl:mouse-position))))

(defun compute-force-indicator (position)
  (clamp (* (distance-between-vectors position (sdl:mouse-position))
            +force-indicator-1/max-length+)
         0
         1))

(defun update-aiming (game-state dt)
  (declare (ignore dt))
  (with-slots ((lmb mouse-left-button-state)
               (lmb-prev mouse-prev-left-button-state)
               ball)
      game-state
    (when (falling-edge lmb-prev lmb)
      (setf (slot-value ball 'velocity)
            (compute-velocity-from-position-and-mouse-pos (slot-value ball 'position)))
      (change-game-state game-state :simulation))))

(defun render-aiming (game-state)
  (with-slots (ball) game-state
    (sdl:draw-line-* (elt (slot-value ball 'position) 0)
                     (elt (slot-value ball 'position) 1)
                     (sdl:mouse-x)
                     (sdl:mouse-y)
                     :aa t
                     :color (lerp-sdl-colors
                             sdl:*green*
                             sdl:*red*
                             (compute-force-indicator (slot-value ball 'position)))
                     :clipping t)))

;;; Simulation
(defun update-simulation (game-state dt)
  (with-slots (celestial-bodies ball hole state) game-state
    (apply-force ball
                 (scaled-vector
                  (compute-gravity-field celestial-bodies ball)
                        ;(* +G+ (slot-value ball 'mass)))
                  +G+)
                 dt)
    (if (collisions-p celestial-bodies ball)
        (change-game-state game-state :planet-collision))
    (if (offworld-p celestial-bodies ball)
        (change-game-state game-state :off-world))
    (if (collided-with-hole-p hole ball)
        (change-game-state game-state :hole-collision))))

; TODO handle ball-hole collision

(defun render-simulation (game-state)
  (declare (ignore game-state)))

;;; Hole collision
(defun update-hole-collision (game-state dt gsm)
  (declare (ignore game-state dt))
  (change-state gsm :victorious))

(defun render-hole-collision (game-state)
  (declare (ignore game-state)))

;;; Planet collision
(defun update-planet-collision (game-state dt gsm)
  (declare (ignore dt))
  (with-slots (lives) game-state
    (setf lives (- lives 1))
    (if (= lives 0)
        (change-state gsm :defeated)
        (progn
          (reinitialize-game game-state)
          (change-game-state game-state :aiming)))))


(defun render-planet-collision (game-state)
  (declare (ignore game-state)))

;;; Off-world
(defun update-off-world (game-state dt gsm)
  (declare (ignore dt))
  (with-slots (lives) game-state
    (setf lives (- lives 1))
    (if (= lives 0)
        (change-state gsm :defeated)
        (progn
          (reinitialize-game game-state)
          (change-game-state game-state :aiming)))))

(defun render-off-world (game-state)
  (declare (ignore game-state)))

;;; GENERIC UPDATE/RENDER FUNCTIONS BELOW
(defun update-common (game-state dt)
  (declare (ignore dt))
  (with-slots (mouse-left-button-state mouse-prev-left-button-state) game-state
  ;; check and update mouse
    (setf mouse-prev-left-button-state mouse-left-button-state) ; <- setf works A <- B this way ;)
    (setf mouse-left-button-state (sdl:mouse-left-p))))

(defun render-common (game-state)
  (with-slots (background-image celestial-bodies hole ball) game-state
    (sdl:clear-display sdl:*black*)
    (draw-image background-image)
    (map nil #'draw celestial-bodies)
    (draw hole)
    (draw ball))) ; TODO draw UI

(defmethod update-logic ((game-state main-game-state)
                         gsm
                         dt)
  (update-common game-state dt)
  (case (slot-value game-state 'state)
    (:aiming (update-aiming game-state dt))
    (:simulation (update-simulation game-state dt))
    (:hole-collision (update-hole-collision game-state dt gsm))
    (:planet-collision (update-planet-collision game-state dt gsm))
    (:off-world (update-off-world game-state dt gsm))))

(defmethod render ((game-state main-game-state)
                   gsm)
  ;;TODO:
  ;; * Draw background
  ;; * Draw game objects
  ;; * Draw foreground / overlays
  (declare (ignore gsm))
  (render-common game-state)
  (case (slot-value game-state 'state)
    (:aiming (render-aiming game-state))
    (:simulation (render-simulation game-state))
    (:hole-collision (render-hole-collision game-state))
    (:planet-collision (render-planet-collision game-state))
    (:off-world (render-off-world game-state))))

