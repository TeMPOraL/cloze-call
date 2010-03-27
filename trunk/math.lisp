;;;; Math functions and tools.
(in-package :trc.math)

(defun make-vector-2d (&optional (a 0) (b 0))
  "Creates a new 2D vector"
  (vector a b))

(defun add-vectors (vec1 vec2)
  "Sums two vectors."
  (map 'vector #'+ vec1 vec2))

(defun add-to-vector (vec1 vec2)
  "Sums two vectors into the first one"
  (map-into vec1 #'+ vec1 vec2))

(defun scaled-vector (vec1 scale)
  "Returns a new vector - vec1 scaled by given scalar"
  (map 'vector (lambda (elem) (* elem scale)) vec1))

(defun scale-vector (vec1 scale)
  "Scales vector by given scalar."
  (map-into vec1 (lambda (elem) (* elem scale)) vec1))

(defun negative-vector (vec1)
  "Returns a new vector - negation of given one"
  (map 'vector #'- vec1))

(defun negate-vector (vec1)
  "Negates current vector."
  (map-into vec1 #'- vec1))