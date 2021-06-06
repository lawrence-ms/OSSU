;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname space_invaders) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; Space invaders game
;; ====================


;; Constants:

(define WIDTH 250)
(define HEIGHT 400)
(define MTS (rectangle WIDTH HEIGHT "outline" "black"))

(define TANK-IMG (above (rectangle 8 12 "solid" "black")
                        (rectangle 30 15 "solid" "black")))
(define TANK-Y (- HEIGHT 15))
(define TANK-SPEED 2)

(define BULLET-IMG (rectangle 6 10 "solid" "red"))
(define BULLET-SPEED -4)

(define UFO-IMG (ellipse 30 12 "solid" "green"))
(define UFO-SPEED-X 1)
(define UFO-SPEED-Y 1)

(define HIT-RANGE 20)
(define INVADE-RATE 100)
;; ====================


;; Data definitions:

(define-struct tank (x dir))
;; Tank is (make-tank Integer Integer[-1, 1])
;; interp. a tank with x position that moves TANK-SPEED pixels per tick
;; to the right if dir = 1 and to the left if dir = -1

(define T1 (make-tank 10 1))   ;tank at x position 10 going to the right
(define T2 (make-tank 120 -1)) ;tank at x position 120 going to the left
#;
(define (fn-for-tank t)
  (... (tank-x t)
       (tank-dir t)))

;; template rules used:
;;  - compound: 2 fields
;; ====================

(define-struct bullet (x y))
;; Bullet is (make-bullet Integer Integer)
;; interp a bullet with x and y position moving at BULLET-SPEED pixels per tick

(define B1 (make-bullet 25 40)) ;bullet at position (25,40)
#;
(define (fn-for-bullet b)
  (... (bullet-x b)
       (bullet-y b)))

;; template rules used:
;;  - compound: 2 fields
;; ====================

;; ListOfBullet is one of:
;;  - empty
;;  - (cons Bullet ListOfBullet)
;; interp. a list of Bullet objects

(define LOB1 empty)                           ;empty list of bullets
(define LOB2 (cons (make-bullet 20 50) empty)) ;list with 1 bullet at position (20,50)
#;
(define (fn-for-lob lob)
  (cond [(empty? lob) (...)]
        [else
         (... (fn-for-bullet (first lob))
              (fn-for-lob (rest lob)))]))

;; template rules used:
;;  - one of: 2 cases
;;  - atomic distinct: empty
;;  - compound: (cons Bullet ListOfBullet)
;;  - reference: (first lob) is Bullet
;;  - self-reference: (rest lob) is ListOfBullet
;; ====================

(define-struct ufo (x y dir))
;; Ufo is (make-ufo Integer Integer Integer[-1, 1])
;; interp. an ufo with x and y positions moving at UFO-SPEED-X and UFO-SPEED-Y
;; when dir = 1 it moves 45 degrees to the right, dir = -1 it moves 45 to the left

(define U1 (make-ufo 10 30 1))  ;ufo at position (10, 30) going to the right
(define U2 (make-ufo 20 50 -1)) ;ufo at position (20, 50) going to the left
#;
(define (fn-for-ufo u)
  (... (ufo-x u)
       (ufo-y u)
       (ufo-dir u)))

;; template rules used:
;;  - compound: 2 fields
;; ====================

;; ListOfUfo is one of:
;;  - empty
;;  - (cons Ufo ListOfUfo)
;; interp. a list of Ufo objects

(define LOU1 empty)                         ;empty list of ufos
(define LOU2 (cons (make-ufo 20 50 1) empty)) ;list with 1 ufo at position (20,50)
#;
(define (fn-for-lou lou)
  (cond [(empty? lou) (...)]
        [else
         (... (fn-for-ufo (first lou))
              (fn-for-lou (rest lou)))]))

;; template rules used:
;;  - one of: 2 cases
;;  - atomic distinct: empty
;;  - compound: (cons Ufo ListOfUfo)
;;  - reference: (first lou) is Ufo
;;  - self-reference: (rest lou) is ListOfUfo
;; ====================

(define-struct game (ufos bullets tank))
;; Game is (make-game (ListOfUfos ListOfBullets Tank))
;; interp. a game with 0 or more ufos, 0 or more bullets and 1 tank

(define G1 (make-game
            (cons (make-ufo 20 40 1) (cons (make-ufo 200 300 -1) empty))
            (cons (make-bullet 30 40) (cons (make-bullet 150 200) (cons (make-bullet 100 50) empty)))
            (make-tank 150 1))) ;game with 2 ufos, 3 bullets and a tank
#;
(define (fn-for-game g)
  (... (fn-for-lou (game-ufos g))
       (fn-for-lob (game-bullets g))
       (fn-for-tank (game-tank g))))

;; template rules used:
;;  - compound: 3 fields
;;  - reference: (game-ufos g) is ListOfUfo
;;  - reference: (game-bullets g) is ListOfBullet
;;  - reference: (game-tank g) is Tank


;; Function definitions:

;; Game -> Game
;; start the world with (main (make-game empty empty (make-tank 0 1)))
;; 
(define (main g)
  (big-bang g                ; Game
    (on-tick   next-game)    ; Game -> Game
    (to-draw   render-game)  ; Game -> Image
    (stop-when end-game?)    ; Game -> Boolean
    (on-key    handle-key))) ; Game KeyEvent -> Game


;; Game -> Game
;; produce the next game state advancing all the entities
(check-expect (next-game (make-game
                          (cons (make-ufo 200 300 -1) empty)
                          (cons (make-bullet 30 40) empty)
                          (make-tank 150 1)))
              (make-game
               (cons (make-ufo (+ 200 (* -1 UFO-SPEED-X)) (+ 300 UFO-SPEED-Y) -1) empty)
               (cons (make-bullet 30 (+ 40 BULLET-SPEED)) empty)
               (make-tank (+ 150 (* 1 TANK-SPEED)) 1))) ;default state, objects don't change directions or colide

(check-expect (next-game (make-game
                          (cons (make-ufo 200 300 -1) empty)
                          (cons (make-bullet 30 40) empty)
                          (make-tank (- WIDTH 1) 1)))
              (make-game
               (cons (make-ufo (+ 200 (* -1 UFO-SPEED-X)) (+ 300 UFO-SPEED-Y) -1) empty)
               (cons (make-bullet 30 (+ 40 BULLET-SPEED)) empty)
               (make-tank WIDTH -1))) ;tank tries to pass the edge, stays at the edge and changes dir (x=0 also apply)

(check-expect (next-game (make-game
                          (cons (make-ufo 200 300 -1) empty)
                          (cons (make-bullet 30 0) empty)
                          (make-tank 150 1)))
              (make-game
               (cons (make-ufo (+ 200 (* -1 UFO-SPEED-X)) (+ 300 UFO-SPEED-Y) -1) empty)
               empty
               (make-tank (+ 150 (* 1 TANK-SPEED)) 1))) ;bullet leaves screen, is removed from list of bullets

(check-expect (next-game (make-game
                          (cons (make-ufo WIDTH 300 1) empty)
                          (cons (make-bullet 30 40) empty)
                          (make-tank 150 1)))
              (make-game
               (cons (make-ufo WIDTH 300 -1) empty)
               (cons (make-bullet 30 (+ 40 BULLET-SPEED)) empty)
               (make-tank (+ 150 (* 1 TANK-SPEED)) 1))) ;ufo hits wall, changes direction

(check-expect (next-game (make-game
                          (cons (make-ufo 200 300 -1) empty)
                          (cons (make-bullet 200 310) empty)
                          (make-tank 150 1)))
              (make-game
               empty
               empty
               (make-tank (+ 150 (* 1 TANK-SPEED)) 1))) ;bullet hits ufo (HIT-RANGE), both of them are removed from their lists

;(define (next-game g) g) ;stub

(define (next-game g)
  (make-game (next-lou (game-ufos g) (game-bullets g))
             (next-lob (game-bullets g) (game-ufos g))
             (next-tank (game-tank g))))


;; Tank -> Tank
;; given a tank object, produce the next tank with new x and dir
(check-expect (next-tank (make-tank 100 1)) (make-tank (+ 100 (* 1 TANK-SPEED)) 1))    ;tank going to the right
(check-expect (next-tank (make-tank (- WIDTH 1) 1)) (make-tank WIDTH -1))              ;tank tries to pass right edge
(check-expect (next-tank (make-tank 100 -1)) (make-tank (+ 100 (* -1 TANK-SPEED)) -1)) ;tank going to the left
(check-expect (next-tank (make-tank 1 -1)) (make-tank 0 1))                            ;tank tries to pass left edge

;(define (next-tank t) t) ;stub

(define (next-tank t)
  (cond [(> (+ (tank-x t) (* (tank-dir t) TANK-SPEED)) WIDTH)
         (make-tank WIDTH (- (tank-dir t)))]
        [(< (+ (tank-x t) (* (tank-dir t) TANK-SPEED)) 0)
         (make-tank 0 (- (tank-dir t)))]
        [else
         (make-tank (+ (tank-x t) (* (tank-dir t) TANK-SPEED)) (tank-dir t))]))


;; ListOfBullet ListOfUfo -> ListOfBullet
;; given a list of bullets and a list of ufos, returns the new list of bullets based on positions and colisions
(check-expect (next-lob empty empty) empty)
(check-expect (next-lob empty (cons (make-ufo 80 90 1) empty)) empty)
(check-expect (next-lob (cons (make-bullet 30 40) (cons (make-bullet 100 40) empty)) empty)
              (cons (make-bullet 30 (+ 40 BULLET-SPEED)) (cons (make-bullet 100 (+ 40 BULLET-SPEED)) empty)))
(check-expect (next-lob (cons (make-bullet 30 40) empty) (cons (make-ufo 80 90 1) empty))
              (cons (make-bullet 30 (+ 40 BULLET-SPEED)) empty))
(check-expect (next-lob (cons (make-bullet 75 89) empty) (cons (make-ufo 80 90 1) empty))
              empty)
(check-expect (next-lob (cons (make-bullet 75 89) (cons (make-bullet 130 200) empty)) (cons (make-ufo 80 90 1) (cons (make-ufo 125 195 -1) empty)))
              empty)

;(define (next-lob lob lou) lob) ;stub

(define (next-lob lob lou)
  (collide-bullets
   (filter-bullets
    (tick-bullets lob))
   lou))


;; ListOfBullet -> ListOfBullet
;; given a list of bullets, produces the next one moving their y position by BULLET-SPEED
(check-expect (tick-bullets empty) empty)
(check-expect (tick-bullets (cons (make-bullet 20 50) (cons (make-bullet 10 90) empty)))
              (cons (make-bullet 20 (+ 50 BULLET-SPEED)) (cons (make-bullet 10 (+ 90 BULLET-SPEED)) empty)))

;(define (tick-bullets lob) lob) ;stub

(define (tick-bullets lob)
  (cond [(empty? lob) empty]
        [else
         (cons (tick-bullet(first lob))
               (tick-bullets(rest lob)))]))


;; Bullet -> Bullet
;; given a bullet object, advance it's y position by BULLET-SPEED
(check-expect (tick-bullet (make-bullet 40 50)) (make-bullet 40 (+ 50 BULLET-SPEED)))

;(define (tick-bullet b) b) ;stub

(define (tick-bullet b)
  (make-bullet (bullet-x b)
               (+ (bullet-y b) BULLET-SPEED)))


;; ListOfBullet -> ListOfBullet
;; given a list of bullets, filter out from it the bullets that have left the screen (y<0)
(check-expect (filter-bullets empty) empty)
(check-expect (filter-bullets (cons (make-bullet 20 40) empty)) (cons (make-bullet 20 40) empty))
(check-expect (filter-bullets (cons (make-bullet 70 0) empty)) (cons (make-bullet 70 0) empty))
(check-expect (filter-bullets (cons (make-bullet 70 90) (cons (make-bullet 140 -1) empty)))
              (cons (make-bullet 70 90) empty))

;(define (filter-bullets lob) empty) ;stub

(define (filter-bullets lob)
  (cond [(empty? lob) empty]
        [else
         (if (< (bullet-y (first lob)) 0)
             (filter-bullets(rest lob))
             (cons (first lob) (filter-bullets (rest lob))))]))


;; ListOfBullet ListOfUfo -> ListOfBullet
;; given a list of bullets, test for collision and filter out bulled that collided with ufos
(check-expect (collide-bullets empty empty) empty)
(check-expect (collide-bullets empty (cons (make-ufo 80 90 1) empty)) empty)
(check-expect (collide-bullets (cons (make-bullet 30 40) empty) (cons (make-ufo 80 90 1) empty))
              (cons (make-bullet 30 40) empty))
(check-expect (collide-bullets (cons (make-bullet 75 85) empty) (cons (make-ufo 80 90 1) empty))
              empty)
(check-expect (collide-bullets (cons (make-bullet 75 85) (cons (make-bullet 130 200) empty)) (cons (make-ufo 80 90 1) (cons (make-ufo 125 195 -1) empty)))
              empty)

;(define (collide-bullets lob lou) lob) ;stub

(define (collide-bullets lob lou)
  (cond [(empty? lob) empty]
        [else
         (if (collide-bullet? (first lob) lou)
             (collide-bullets (rest lob) lou)
             (cons (first lob) (collide-bullets (rest lob) lou)))]))


;; Bullet ListOfUfo -> Boolean
;; given a bullet and a list of ufos, filter out bullet if it's distance to any ufo is less or equal HIT-RANGE
(check-expect (collide-bullet? (make-bullet 40 90) empty) false)
(check-expect (collide-bullet? (make-bullet 10 20) (cons (make-ufo 40 50 1) (cons (make-ufo 60 70 -1) empty)))
              false)
(check-expect (collide-bullet? (make-bullet 10 20) (cons (make-ufo 15 25 1) (cons (make-ufo 60 70 -1) empty)))
              true)

;(define (collide-bullet? b lou) false) ;stub

(define (collide-bullet? b lou)
  (cond [(empty? lou) false]
        [else
         (if (> (bullet-distance b (first lou)) HIT-RANGE)
             (collide-bullet? b (rest lou))
             true)]))


;; Bullet Ufo -> Number
;; given a bullet and an ufo, return the distance between then (straight line)
(check-expect (bullet-distance (make-bullet 20 30) (make-ufo 30 40 1))
              (inexact->exact (sqrt (+ (sqr (- 20 (+ 30 (* 1 UFO-SPEED-X)))) (sqr (- 30 (+ 40 UFO-SPEED-Y)))))))

;(define (bullet-distance b u) 0) ;stub

(define (bullet-distance b u)
  (inexact->exact (sqrt (+ (sqr (- (bullet-x b) (+ (ufo-x u) (* (ufo-dir u) UFO-SPEED-X))))
                           (sqr (- (bullet-y b) (+ (ufo-y u) UFO-SPEED-Y)))))))


;; ListOfUfo ListOfBullet -> ListOfUfo
;; given a list of ufos and a list of bullets, returns the new list of ufos based on positions and colisions
(check-expect (next-lou empty empty) empty)
(check-expect (next-lou empty (cons (make-bullet 80 30) empty)) empty)
(check-expect (next-lou (cons (make-ufo 100 40 -1)(cons (make-ufo 80 20 1) empty)) empty)
              (cons (make-ufo (+ 100 (* -1 UFO-SPEED-X)) (+ 40 UFO-SPEED-Y) -1)
                    (cons (make-ufo (+ 80 (* 1 UFO-SPEED-X)) (+ 20 UFO-SPEED-Y) 1) empty)))
(check-expect (next-lou (cons (make-ufo 0 40 -1) (cons (make-ufo WIDTH 20 1) empty)) (cons (make-bullet 100 200) empty))
              (cons (make-ufo 0 40 1) (cons (make-ufo WIDTH 20 -1) empty)))
(check-expect (next-lou (cons (make-ufo 100 40 -1) (cons (make-ufo WIDTH 20 1) empty)) (cons (make-bullet 97 38) empty))
              (cons (make-ufo WIDTH 20 -1) empty))

;(define (next-lou lou lob) empty) ;stub

(define (next-lou lou lob)
  (generate-ufos(collide-ufos
                 (bounce-ufos
                  (tick-ufos lou))
                 lob)))


;; ListOfUfo -> ListOfUfo
;; given a list of ufos, move each one of them by UFO-SPEED-X and UFO-SPEED-Y pixels
(check-expect (tick-ufos empty) empty)
(check-expect (tick-ufos (cons (make-ufo 200 340 1) (cons (make-ufo 10 20 -1) empty)))
              (cons (make-ufo (+ 200 (* 1 UFO-SPEED-X)) (+ 340 UFO-SPEED-Y) 1)
                    (cons (make-ufo (+ 10 (* -1 UFO-SPEED-X)) (+ 20 UFO-SPEED-Y) -1) empty)))

;(define (tick-ufos lou) lou) ;stub

(define (tick-ufos lou)
  (cond [(empty? lou) empty]
        [else
         (cons (tick-ufo (first lou))
               (tick-ufos (rest lou)))]))


;; Ufo -> Ufo
;; given an ufo object, move it's x position by UFO-SPEED-X and y position by UFO-SPEED-Y
(check-expect (tick-ufo (make-ufo 23 43 -1))
              (make-ufo (+ 23 (* -1 UFO-SPEED-X)) (+ 43 UFO-SPEED-Y) -1))

;(define (tick-ufo u) u) ;stub

(define (tick-ufo u)
  (make-ufo (+ (ufo-x u) (* (ufo-dir u) UFO-SPEED-X))
            (+ (ufo-y u) UFO-SPEED-Y)
            (ufo-dir u)))


;; ListOfUfo -> ListOfUfo
;; given a list of ufos, bounce (change their dir) the ones that try to pass x position 0 or WIDTH
(check-expect (bounce-ufos empty) empty)
(check-expect (bounce-ufos (cons (make-ufo 34 10 1) (cons (make-ufo 100 20 -1) empty)))
              (cons (make-ufo 34 10 1) (cons (make-ufo 100 20 -1) empty)))
(check-expect (bounce-ufos (cons (make-ufo (+ 3 WIDTH) 10 1) (cons (make-ufo 100 20 -1) empty)))
              (cons (make-ufo WIDTH (- 10 UFO-SPEED-Y) -1) (cons (make-ufo 100 20 -1) empty)))
(check-expect (bounce-ufos (cons (make-ufo 34 10 1) (cons (make-ufo -2 20 -1) empty)))
              (cons (make-ufo 34 10 1) (cons (make-ufo 0 (- 20 UFO-SPEED-Y) 1) empty)))

;(define (bounce-ufos lou) lou) ;stub

(define (bounce-ufos lou)
  (cond [(empty? lou) empty]
        [else
         (cons (cond [(< (ufo-x (first lou)) 0)
                      (make-ufo 0 (- (ufo-y (first lou)) UFO-SPEED-Y) 1)]
                     [(> (ufo-x (first lou)) WIDTH)
                      (make-ufo WIDTH (- (ufo-y (first lou)) UFO-SPEED-Y) -1)]
                     [else (first lou)])
               (bounce-ufos (rest lou)))]))


;; ListOfUfo ListOfBullet -> ListOfUfo
;; given a list of ufos and a list of bullets, test for collisions and remove collided ufos from list
(check-expect (collide-ufos empty empty) empty)
(check-expect (collide-ufos empty (cons (make-bullet 2 90) empty)) empty)
(check-expect (collide-ufos (cons (make-ufo 15 90 -1) empty) empty) (cons (make-ufo 15 90 -1) empty))
(check-expect (collide-ufos (cons (make-ufo 15 90 -1) (cons (make-ufo 20 30 1) empty)) (cons (make-bullet 20 85) empty))
              (cons (make-ufo 20 30 1) empty))
(check-expect (collide-ufos (cons (make-ufo 15 90 -1) (cons (make-ufo 20 30 1) empty)) (cons (make-bullet 15 25) empty))
              (cons (make-ufo 15 90 -1) empty))
(check-expect (collide-ufos (cons (make-ufo 15 90 -1) (cons (make-ufo 20 30 1) empty))
                            (cons (make-bullet 20 85) (cons (make-bullet 15 25) empty)))
              empty)

;(define (collide-ufos lou lob) lou) ;stub

(define (collide-ufos lou lob)
  (cond [(empty? lou) empty]
        [else
         (if (collide-ufo? (first lou) lob)
             (collide-ufos (rest lou) lob)
             (cons (first lou) (collide-ufos (rest lou) lob)))]))


;; Ufo ListOfBullet -> Boolean
;; given an ufo and a list of bullets, test collision with ufo for each bullet, return true if any, false otherwise
(check-expect (collide-ufo? (make-ufo 30 50 1) empty) false)
(check-expect (collide-ufo? (make-ufo 30 50 1) (cons (make-bullet 10 20) (cons (make-bullet 90 200) empty))) false)
(check-expect (collide-ufo? (make-ufo 30 50 1) (cons (make-bullet 25 55) (cons (make-bullet 90 200) empty))) true)
(check-expect (collide-ufo? (make-ufo 30 50 1) (cons (make-bullet 10 20) (cons (make-bullet 35 45) empty))) true)

;(define (collide-ufo? u lob) false) ;stub

(define (collide-ufo? u lob)
  (cond [(empty? lob) false]
        [else
         (if (< (ufo-distance u (first lob)) HIT-RANGE)
             true
             (collide-ufo? u (rest lob)))]))


;; Ufo Bullet -> Number
;; given an ufo and a bullet, returns the distance between then (straight line)
(check-expect (ufo-distance (make-ufo 10 30 1) (make-bullet 40 50))
              (inexact->exact (sqrt (+ (sqr (- 10 40)) (sqr (- 30 (+ 50 BULLET-SPEED)))))))

;(define (ufo-distance u b) 0) ;stub

(define (ufo-distance u b)
  (inexact->exact (sqrt (+ (sqr (- (ufo-x u) (bullet-x b)))
                           (sqr (- (ufo-y u) (+ (bullet-y b) BULLET-SPEED)))))))


;; ListOfUfo -> ListOfUfo
;; given a list of ufos, generate a new one with random x position if RNG condition is met

(define (generate-ufos lou)
  (if (< (random INVADE-RATE) 1)
      (cons (make-ufo (random WIDTH) 0 1) lou)
      lou))


;; Game -> Image
;; render the current game state with all of the entities
(check-expect (render-game (make-game (cons (make-ufo 90 10 -1) (cons (make-ufo 100 350 1) empty))
                                      (cons (make-bullet 10 200) (cons (make-bullet 90 140) empty))
                                      (make-tank 40 1)))
              (overlay (place-image TANK-IMG 40 TANK-Y MTS)
                       (place-image BULLET-IMG 10 200 (place-image BULLET-IMG 90 140 MTS))
                       (place-image UFO-IMG 90 10 (place-image UFO-IMG 100 350 MTS))
                       (rectangle WIDTH HEIGHT "solid" "white")))
                       
              
;(define (render-game g) MTS) ;stub

(define (render-game g)
  (overlay (render-tank (game-tank g))
           (render-lob (game-bullets g))
           (render-lou (game-ufos g))
           (rectangle WIDTH HEIGHT "solid" "white")))


;; Tank -> Image
;; given a tank object, render it's image on MTS at position tank-x and TANK-Y
(check-expect (render-tank (make-tank 20 -1))
              (place-image TANK-IMG 20 TANK-Y MTS))

;(define (render-tank t) MTS) ;stub

(define (render-tank t)
  (place-image TANK-IMG (tank-x t) TANK-Y MTS))


;; ListOfBullet -> Image
;; given a list of bullets, render their image on MTS at appropriate bullet-x and bullet-y positions
(check-expect (render-lob empty) MTS)
(check-expect (render-lob (cons (make-bullet 230 300) (cons (make-bullet 10 30) empty)))
              (place-image BULLET-IMG 230 300 (place-image BULLET-IMG 10 30 MTS)))

;(define (render-lob lob) MTS) ;stub

(define (render-lob lob)
  (cond [(empty? lob) MTS]
        [else
         (place-image BULLET-IMG
                      (bullet-x (first lob))
                      (bullet-y (first lob))
                      (render-lob (rest lob)))]))


;; ListOfUfos -> Image
;; given a list of ufos, render their image on MTS at appropriate ufo-x and ufo-y positions
(check-expect (render-lou empty) MTS)
(check-expect (render-lou (cons (make-ufo 230 300 1) (cons (make-ufo 10 30 -1) empty)))
              (place-image UFO-IMG 230 300 (place-image UFO-IMG 10 30 MTS)))

;(define (render-lou lou) MTS) ;stub

(define (render-lou lou)
  (cond [(empty? lou) MTS]
        [else
         (place-image UFO-IMG
                      (ufo-x (first lou))
                      (ufo-y (first lou))
                      (render-lou (rest lou)))]))


;; Game -> Boolean
;; given a game state, returns true if the game should end, false otherwise
(check-expect (end-game? (make-game (cons (make-ufo 30 40 1) empty)
                                    empty
                                    (make-tank 10 1))) false)
(check-expect (end-game? (make-game (cons (make-ufo 30 401 1) empty)
                                    empty
                                    (make-tank 10 1))) true)
(check-expect (end-game? (make-game (cons (make-ufo 30 40 1) (cons (make-ufo 100 430 -1) empty))
                                    empty
                                    (make-tank 10 1))) true)

;(define (end-game? g) false) ;stub

(define (end-game? g)
  (ufos-landed? (game-ufos g)))


;; ListOfUfo -> Boolean
;; given a list of ufos, returns true if one of them lands (reaches ufo-y > HEIGHT)
(check-expect (ufos-landed? empty) false)
(check-expect (ufos-landed? (cons (make-ufo 30 40 1) empty)) false)
(check-expect (ufos-landed? (cons (make-ufo 30 40 1) (cons (make-ufo 90 401 -1) empty))) true)

;(define (ufos-landed? lou) false) ;stub

(define (ufos-landed? lou)
  (cond [(empty? lou) false]
        [else
         (if (ufo-landed? (first lou))
             true
             (ufos-landed? (rest lou)))]))


;; Ufo -> Boolean
;; given an ufo object, returns true if ufo-y > HEIGHT (ufo landed), false otherwise
(check-expect (ufo-landed? (make-ufo 10 20 1)) false)
(check-expect (ufo-landed? (make-ufo 40 430 1)) true)

;(define (ufo-landed? u) false) ;stub

(define (ufo-landed? u)
  (> (ufo-y u) HEIGHT))


;; Game KeyEvent -> Game
;; given a ke input, moves the tank left/right or shoots a bullet from the tank
(check-expect (handle-key (make-game (cons (make-ufo 20 30 -1) (cons (make-ufo 90 230 1) empty))
                                     (cons (make-bullet 50 60) (cons (make-bullet 150 20) empty))
                                     (make-tank 50 1)) "left")
              (make-game (cons (make-ufo 20 30 -1) (cons (make-ufo 90 230 1) empty))
                         (cons (make-bullet 50 60) (cons (make-bullet 150 20) empty))
                         (make-tank 50 -1)))
(check-expect (handle-key (make-game (cons (make-ufo 20 30 -1) (cons (make-ufo 90 230 1) empty))
                                     (cons (make-bullet 50 60) (cons (make-bullet 150 20) empty))
                                     (make-tank 50 1)) "right")
              (make-game (cons (make-ufo 20 30 -1) (cons (make-ufo 90 230 1) empty))
                         (cons (make-bullet 50 60) (cons (make-bullet 150 20) empty))
                         (make-tank 50 1)))
(check-expect (handle-key (make-game (cons (make-ufo 20 30 -1) (cons (make-ufo 90 230 1) empty))
                                     (cons (make-bullet 50 60) (cons (make-bullet 150 20) empty))
                                     (make-tank 50 -1)) "left")
              (make-game (cons (make-ufo 20 30 -1) (cons (make-ufo 90 230 1) empty))
                         (cons (make-bullet 50 60) (cons (make-bullet 150 20) empty))
                         (make-tank 50 -1)))
(check-expect (handle-key (make-game (cons (make-ufo 20 30 -1) (cons (make-ufo 90 230 1) empty))
                                     (cons (make-bullet 50 60) (cons (make-bullet 150 20) empty))
                                     (make-tank 50 -1)) "right")
              (make-game (cons (make-ufo 20 30 -1) (cons (make-ufo 90 230 1) empty))
                         (cons (make-bullet 50 60) (cons (make-bullet 150 20) empty))
                         (make-tank 50 1)))
(check-expect (handle-key (make-game (cons (make-ufo 20 30 -1) (cons (make-ufo 90 230 1) empty))
                                     (cons (make-bullet 50 60) (cons (make-bullet 150 20) empty))
                                     (make-tank 50 1)) " ")
              (make-game (cons (make-ufo 20 30 -1) (cons (make-ufo 90 230 1) empty))
                         (cons (make-bullet 50 (+ TANK-Y 5)) (cons (make-bullet 50 60) (cons (make-bullet 150 20) empty)))
                         (make-tank 50 1)))
(check-expect (handle-key (make-game (cons (make-ufo 20 30 -1) (cons (make-ufo 90 230 1) empty))
                                     (cons (make-bullet 50 60) (cons (make-bullet 150 20) empty))
                                     (make-tank 50 -1)) "a")
              (make-game (cons (make-ufo 20 30 -1) (cons (make-ufo 90 230 1) empty))
                         (cons (make-bullet 50 60) (cons (make-bullet 150 20) empty))
                         (make-tank 50 -1)))

;(define (handle-key g ke) g) ;stub

(define (handle-key g ke)
  (cond [(key=? ke " ") (generate-bullets g)]
        [(key=? ke "left") (make-game (game-ufos g)
                                      (game-bullets g)
                                      (make-tank (tank-x (game-tank g)) -1))]
        [(key=? ke "right") (make-game (game-ufos g)
                                       (game-bullets g)
                                       (make-tank (tank-x (game-tank g)) 1))]
        [else g]))


;; Game -> Game
;; given a game state, return a new one with an updated bullet list (new shot)
(check-expect (generate-bullets (make-game empty empty (make-tank 10 1)))
              (make-game empty (cons (make-bullet 10 (+ TANK-Y 5)) empty) (make-tank 10 1)))
(check-expect (generate-bullets (make-game empty (cons (make-bullet 50 150) empty) (make-tank 10 1)))
              (make-game empty (cons (make-bullet 10 (+ TANK-Y 5)) (cons (make-bullet 50 150) empty)) (make-tank 10 1)))

;(define (generate-bullets g) g) ;stub

(define (generate-bullets g)
  (make-game (game-ufos g)
             (generate-bullet (game-bullets g) (tank-x (game-tank g)))
             (game-tank g)))


;; ListOfBullet Integer -> ListOfBullet
;; given a list of bullets and a tank x position, returns a new one with an updated bullet (the one that was shot)
(check-expect (generate-bullet empty 50) (cons (make-bullet 50 (+ TANK-Y 5)) empty))

;(define (generate-bullet lob tx) empty) ;stub

(define (generate-bullet lob tx)
  (cons (make-bullet tx (+ TANK-Y 5)) lob))