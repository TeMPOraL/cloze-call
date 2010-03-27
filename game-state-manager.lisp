;;;; Game State Manager - it manages game states, such as main game,
;;;; main menu, victory screens, ect.
(in-package :trc.cloze-call)

;;; Game State Manager class
(defclass game-state-manager ()
  ((states
    :initform ()
    :documentation "Keyword list of available game states")
   (current-state
    :initform ()
    :documentation "Currently used game state")
   (next-state
    :initform nil
    :documentation "State ID (keyword) that will be selected at the end of state manager update loop")))

;;; Game State Manager private interface
(defgeneric enforce-proper-gsm-state (gsm)
  (:documentation "See if GSM should not change state. If it should, then handle game state change."))

;;; Game State Manager public interface
(defgeneric initialize-gsm (gsm)
  (:documentation "Initialize game state manager. Allow it to grab resources, ect."))

(defgeneric deinitialize-gsm (gsm)
  (:documentation "Deinitialize game state manager."))

(defgeneric update-gsm (gsm dt)
  (:documentation "Update game state manager (and current state)."))

(defgeneric render-gsm (gsm)
  (:documentation "Render current game to screen."))

(defgeneric register-game-state (gsm new-state-kw new-state)
  (:documentation "Register new game state to state manager."))

(defgeneric change-state (gsm new-state-kw)
  (:documentation "Change game state to new one, named by new-state-kw keyword."))

;;; Game State Manager private interface implementation
(defmethod enforce-proper-gsm-state ((gsm game-state-manager))
  (with-slots (current-state states next-state) gsm
    (unless (null next-state)
      (let ((next-state-object (getf states next-state)))
        (if (null next-state-object) (error "Nonexisting game state selected."))
        (unless (null current-state)
          (deinitialize-state current-state gsm)) ; deinitialize current game state if not null
        (setf current-state next-state-object) ; change state
        (setf next-state nil) ; clear state change request
        (initialize-state current-state gsm))))) ;i nitialize new state

;;; Game State Manager public interface implementation
(defmethod initialize-gsm ((gsm game-state-manager))
  (format t "GSM init~%"))

(defmethod deinitialize-gsm ((gsm game-state-manager))
  (format t "GSM deinit~%"))

(defmethod update-gsm ((gsm game-state-manager) dt)
  (with-slots (current-state) gsm
    (enforce-proper-gsm-state gsm)
    (update-logic current-state gsm dt)))

(defmethod render-gsm ((gsm game-state-manager))
  (with-slots (current-state) gsm
    (render current-state gsm)))

(defmethod register-game-state ((gsm game-state-manager)
                                new-state-kw new-state)
  (with-slots (states) gsm
    (if (null (getf states new-state-kw))
        (setf states (cons new-state-kw (cons new-state states))) ; append at the beginning
        (error "Game state already registered!"))))

(defmethod change-state ((gsm game-state-manager)
                         new-state-kw)
  (setf (slot-value gsm 'next-state) new-state-kw))

;;; Utilities
(defmacro with-game-state-manager (gsm-name &body body)
  `(let ((,gsm-name (make-instance 'game-state-manager)))
    (initialize-gsm ,gsm-name)
    ,@body
    (deinitialize-gsm ,gsm-name)))

;;; Game State base class
(defclass game-state ()
  () )

;;; Game State public interface
(defgeneric initialize-state (game-state gsm)
  (:documentation "Perform game state initialization - load resources, create data structures, ect."))

(defgeneric deinitialize-state (game-state gsm)
  (:documentation "Perform game state deinitialization - unload resources, save data to disk, ect."))

(defgeneric update-logic (game-state gsm dt)
  (:documentation "Update game state physics/logic stuff."))

(defgeneric render (game-state gsm)
  (:documentation "Render game world."))