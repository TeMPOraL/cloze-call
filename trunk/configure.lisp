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

;;; Game constants
(defparameter +default-game-screen-time+ 2.0) ; in seconds
(defparameter +G+ -5000) ; gravity constant 7*10^-11
(defparameter +default-ball-velocity+ 150)
(defparameter +force-indicator-1/max-length+ (/ 1 300.0))

(defparameter +default-planet-mass+ 500)
(defparameter +max-additional-planet-mass+ 500)
(defparameter +small-planet-radius+ 47)
(defparameter +large-planet-radius+ 94)
(defparameter +small-planet-dimmensions+ #(150 150))
(defparameter +large-planet-dimmensions+ #(300 300))
(defparameter +default-planet-cnt+ 3)
(defparameter +additional-planet-cnt-max+ 2)
(defparameter +max-x-grid-deviation+ 25)
(defparameter +max-y-grid-deviation+ 25)

(defparameter +default-ball-starting-position+ #(50 50))