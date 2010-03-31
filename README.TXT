================
   CLOZE CALL
================
A simple gravity-golf game.

Aim with your mouse to set direction and speed of the ball.
Press your left mouse button to fire it.
Try to hit the hole.

You have three lives (but the counter is invisible :P).
================

Game written in 7 days for 2010 Lisp Game Design Challenge.

Platform: Clozure Common Lisp.
Tested on Windows 7 (32bit).
Used libraries: lispbuilder-sdl, lispbuilder-sdl-gfx,
     lispbuilder-sdl-image

(C) Jacek Zlydach
http://temporal.pr0.pl/devblog

The game is released on MIT licence.
It was my first Common Lisp project :).

Credits and attributions can be found in CREDITS.TXT file.

================
LAUNCHING THE GAME
================
First, you need to have lispbuilder-sdl, lispbuilder-sdl-gfx
and lispbuilder-sdl-image installed. Load them in your REPL.

Then change working directory to ClozeCall dir and:
(load "cloze-call.lisp")
(trc.cloze-call:run-game)

Known issues:
 * Sometimes instead of loading the REPL reports missing
   method. Just try to re-load it.
