;;;; Image loading / drawing utilities.

(in-package :trc.cloze-call)

(defun load-image (image-name &key (color-key-at (make-vector-2d 0 0) color-key-supplied-p))
  (let ((pathname (merge-pathnames image-name
                                   +gfx-asset-path+)))
  (if color-key-supplied-p
      (sdl:load-image pathname :color-key-at color-key-at :alpha 255)
      (sdl:load-image pathname :alpha 255))))


(defun draw-image (image &key
                   (position (make-vector-2d))
                   (dimmensions (make-vector-2d)))
  (sdl:draw-surface-at-* image
                          (round (- (elt position 0)
                                    (/ (elt dimmensions 0) 2)))
                          (round (- (elt position 1)
                                    (/ (elt dimmensions 1) 2)))))