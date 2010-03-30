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

;;; LEVEL FUNCTIONS
;;; Will be redefined by loaded levels.
;;; Below are function templates (stubs).

;;; FIXME
;;; .:20:18:03:. <tcr> Strictly speaking, their definition should not be in the same file as their uses, as implementations are permitted to arbitrarily inline in that case.
;;; .:20:18:39:. <tcr> I'd probably add an extra level of indirection
;;; .:20:18:42:. <stassats`> there's notinline, but still the whole thing doesn't sound right to me
;;; .:20:18:46:. <lhz> can't you muffle that style-warning (along with redefine SW)?
;;; .:20:19:00:. <tcr> (defun foo (args...) (funcall *foo-function* ...))

;(defgeneric level-get-celestial-bodies ()
 ; (:documentation "Load ")

;(defun level-

;;; END OF LEVEL FUNCTIONS

(defmethod initialize-state ((game-state main-game-state)
                             gsm)
  (declare (ignore gsm))
  (with-slots (celestial-bodies ball hole simulation-running
                                background-image next-level)  game-state
    
    ;;TODO:
    ;; * Load level
    (load-level next-level)
    ;; * Load planets from level
    (setf celestial-bodies (level-get-celestial-bodies))
    
    ;; * Load other important objects
    (setf background-image (load-image (level-get-background-image-name)))
    ;; * Set proper flags
    (setf simulation-running t)
    (setf ball (level-get-ball))
    (setf hole (level-get-hole))
    
    ;; Loading level could be done by (load)ing a level file
    ;; with functions that would return a list of planets,
    ;; a background file name, ect. Or maybe even variables - 
    ;; we don't want to have a mess here.
    ))

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
  (not (null (find-if
         (lambda (body) (< (distance-between-vectors (slot-value body 'position)
                                                     (slot-value ball 'position))
                           (+ (slot-value body 'radius)
                              (slot-value ball 'radius))))
         celestial-bodies))))

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


;  (flet ((compute-gravity-field (total-field body point-in-space)
;           (with-slots (body-position body-mass) body
;             (* +G+ (/ body-mass
;                       (distance-between-vectors point-in-space body-position))))))
;    (with-slots (celestial-bodies ball) game-state
;      (let ((gravity-field (reduce (lambda (total body)
;  (compute-gravity-field total body (slot-value ball 'position)))
;  celestial-bodies)))
;        (format t "Compute physics here.")))))

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
