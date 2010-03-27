;;;;Package definition for Cloze Call games
(defpackage :trc.math
  (:use :common-lisp)
  (:export :make-vector-2d
           :add-vectors
           :add-to-vector
           :scaled-vector
           :scale-vector
           :negative-vector
           :negate-vector))
;;TODO exports

(defpackage :trc.cloze-call
  (:use :common-lisp :trc.math)
  (:export :run-game))
;;TODO exports
	
