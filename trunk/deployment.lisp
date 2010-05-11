;;;; Deployment of Cloze-Call
;;;; Should be loaded to REPL after loading lispbuilder-sdl stuff.

(load (compile-file "cloze-call.lisp"))

(defun main ()
  ;; Load SDL
  (cffi:define-foreign-library sdl
			       (t (:default "SDL")))           ; Windows only, see below for portable version
  (cffi:define-foreign-library sdl-gfx
			       (t (:default "SDL_gfx")))
  (cffi:define-foreign-library sdl-image
			       (t (:default "SDL_image")))

  (cffi:use-foreign-library sdl)
  (cffi:use-foreign-library sdl-gfx)
  (cffi:use-foreign-library sdl-image)

  (trc.cloze-call:run-game)

  (quit))
