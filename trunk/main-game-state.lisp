;;;; Main Game State - handles the core game simulation and rules
(in-package trc.cloze-call)

(load "game-objects.lisp") ; game classes

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

(defun load-level (level-name)
  (load (merge-pathnames level-name +levels-asset-path+)))

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
    (setf background-image (sdl:load-image
                            (merge-pathnames (level-get-background-image-name)
                                             +gfx-asset-path+)))
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

(defmethod update-logic ((game-state main-game-state)
                         gsm
                         dt)
  (declare (ignore game-state gsm dt)))

(defmethod render ((game-state main-game-state)
                   gsm)
  ;;TODO:
  ;; * Draw background
  ;; * Draw game objects
  ;; * Draw foreground / overlays
  (declare (ignore game-state gsm)))
