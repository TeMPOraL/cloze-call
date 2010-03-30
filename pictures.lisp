;;;; Image loading / drawing utilities.

(in-package :trc.cloze-call)

(defun load-image (image-name &key (color-key-at (make-vector-2d 0 0) color-key-supplied-p))
  (let ((pathname (merge-pathnames image-name
                                   +gfx-asset-path+)))
  (if color-key-supplied-p
      (sdl:load-image pathname :color-key-at color-key-at)
      (sdl:load-image pathname))))


(defun draw-image (image &key (position (make-vector-2d)))
  (sdl:draw-surface-at image position))