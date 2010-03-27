;;;; Configuration file for Cloze-Call game.
;;;; ...
;;;; TODO

(in-package :trc.cloze-call)

(format t "Hello from configure!~%")

;;; Display constants
(defparameter +screen-width+ 800)
(defparameter +screen-height+ 600)
(defparameter +window-title+ "(Cloze Call)")

;;; Simulation constants
(defparameter +fixed-dt+ (floor (/ 1000 30))) ; 30 steps / second
(defparameter +maximum-dt+ 500) ; maximum dt allowed - time step
                                ;will be trimmed to that value

;;; Resource constants
(defparameter +gfx-asset-path+ "data/gfx/")
(defparameter +sfx-asset-path+ "data/sfx/")
(defparameter +levels-asset-path+ "data/levels/")
