;;;; Main file for Cloze-Call game.
(load "package.lisp")
(in-package :trc.cloze-call)

(load "configure.lisp") ; game configuration
(load "math.lisp")      ; math module
(load "pictures.lisp")  ; image library :)
(load "game-state-manager.lisp") ; game state manager module
(load "test-game-state.lisp") ; test game state
(load "game-screen-state.lisp") ; game screens - victory, intro, ect.
(load "main-game-state.lisp")  ; main game state - the real game is here ;)


(format t "Hello from Cloze-Call~%")

(defun run-game ()
  (sdl:with-init (sdl:SDL-INIT-VIDEO sdl:SDL-INIT-AUDIO)
    (sdl:window +screen-width+ +screen-height+  ;screen resolution
                :title-caption +window-title+
                :fps (make-instance 'sdl:fps-timestep
                                    :max-dt +maximum-dt+ ; timestep upper bound
                                    :dt +fixed-dt+); fixed time step
                :video-driver "directx"
                :double-buffer t)
    (with-game-state-manager gsm
      ;;; REGISTER GAME STATES
      (register-game-state gsm :test-state (make-instance 'test-game-state))
      (register-game-state gsm :intro-screen (make-instance 'screen-game-state
                                                            :picture-name "intro1.png"
                                                            :next-state :intro-screen-2))
      (register-game-state gsm :intro-screen-2 (make-instance 'screen-game-state
                                                             :picture-name "intro2.png"
                                                             :next-state :main-game))
      (register-game-state gsm :main-game (make-instance 'main-game-state
                                                         :run-level "ojej"))
      (register-game-state gsm :defeated (make-instance 'screen-game-state
                                                        :picture-name "defeated.png"
                                                        :next-state :main-game))
      (register-game-state gsm :victorious (make-instance 'screen-game-state
                                                          :picture-name "victorious.png"
                                                          :next-state :main-game))
      (change-state gsm :intro-screen)
      (enforce-proper-gsm-state gsm)
      ;;; EVENT PUMP / MAIN LOOP
      (sdl:with-events ()
        ;; INPUT EVENTS

        ;; UPDATE EVENT
        (:idle
         (sdl:with-timestep ()
           ;; update game state manager here
           (update-gsm gsm (float (/ (sdl:dt) 1000)))) ; also convert dt from miliseconds to seconds
         ;; render game state manager
         (render-gsm gsm)
         (sdl:update-display))
      
        ;; QUIT EVENT
        (:quit-event ()
                     (format t "quit event received~%")
                     t)))))
