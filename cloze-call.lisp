;;;; Main file for Cloze-Call game.
(load "package.lisp")
(in-package :trc.cloze-call)

(load "configure.lisp")

(format t "Hello from Cloze-Call~%")

(sdl:with-init (sdl:SDL-INIT-VIDEO sdl:SDL-INIT-AUDIO)
  (sdl:window +screen-width+ +screen-height+  ;screen resolution
              :title-caption "Cloze-Call" ; title
              :fps (make-instance 'sdl:fps-timestep))
  (sdl:with-events ()
    (:idle
     (sdl:clear-display sdl:*red*)
     (sdl:update-display))
    (:quit-event () t)))
