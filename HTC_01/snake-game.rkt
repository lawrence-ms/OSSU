;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname snake-game) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; Snake game with BSL

;; =================
;; Constants:

(define WIDTH 1200)
(define HEIGHT 600)
(define MTS (rectangle WIDTH HEIGHT "outline" "black"))
(define SNAKE-SPEED 20)
(define SNAKE-IMG (square 20 "solid" "black"))
(define APPLE-IMG (circle 10 "solid" "red"))
(define HIT-RANGE 1)

;; =================
;; Data definitions:

(define-struct apple (x y))
;; Apple is (make-apple Integer Integer)
;; interp. an apple with x and y positions

(define A1 (make-apple 0 10)) ;apple at position (0, 10)
#;
(define (fn-for-apple a)
  (... (apple-x a)
       (apple-y a)))

;; Template rules used:
;;  - compound: 2 fields

(define-struct snake (x y dx dy))
;; Snake is (make-snake Integer Integer Integer[-1, 1] Integer[-1, 1])
;; interp. a snake body segment with x and y positions and dx and dy directions
;; dx -1 moving left, 1 moving right, 0 not moving horizontally
;; dy -1 moving down, 1 moving up, 0 not moving vertically

(define S1 (make-snake 20 40 1 0)) ;snake at position (20, 40) moving to the right
(define S2 (make-snake 20 40 -1 0)) ;snake at position (20, 40) moving to the left
(define S3 (make-snake 20 40 0 1)) ;snake at position (20, 40) moving up
(define S4 (make-snake 20 40 0 -1)) ;snake at position (20, 40) moving down
#;
(define (fn-for-snake s)
  (... (snake-x s)
       (snake-y s)
       (snake-dx s)
       (snake-dy s)))

;; Template rules used:
;;  - compound: 4 fields

;; ListOfSnake is one of:
;;  - empty
;;  - (cons Snake ListOfSnake)
;; interp. a list of snake body segments

(define LOS1 (cons (make-snake 20 40 1 0) empty)) ;snake with 1 body segment at (20, 40) going to the right
(define LOS2 (cons (make-snake 40 40 1 0) LOS1)) ;snake with 2 body segments, head at (40,40) going to the right
#;
(define (fn-for-los los)
  (cond [(empty? los) (...)]
        [else
         (... (fn-for-snake (first los))
              (fn-for-los (rest los)))]))

;; Template rules used:
;;  - one of: 2 cases
;;  - atomic distinct: empty
;;  - compound: (cons Snake ListOfSnake)
;;  - reference: (first los) is Snake
;;  - self-reference: (rest los) is ListOfSnake

(define-struct game (los apple))
;; Game is (make-game ListOfSnake Apple)
;; interp. a game state with a snake and all of its segments and an apple

(define G1 (make-game LOS2 A1)) ;game state with snake given as LOS2 and apple as A1
#;
(define (fn-for-game g)
  (... (fn-for-los (game-los g))
       (fn-for-apple (game-apple g))))

;; Template rules used:
;; compound: 2 fields
;; reference: (game-los g) is ListOfSnake
;; reference: (game-apple g) is Apple

;; =================
;; Functions:

;; Game -> Game
;; start the world with (main (make-game (list (make-snake 600 300 1 0) (make-snake 580 300 1 0) (make-snake 560 300 1 0)) (make-apple 900 140)))
;; 
(define (main g)
  (big-bang g                         ; Game
            (on-tick   next-game)     ; Game -> Game
            (to-draw   render-game)   ; Game -> Image
            (stop-when end-game?)     ; Game -> Boolean
            (on-key    handle-key)))  ; Game KeyEvent -> Game

;; Game -> Game
;; produce the next game state given the previous game state
(check-expect (next-game (make-game (list (make-snake 100 200 1 0) (make-snake 80 200 1 0)) (make-apple 10 20)))
              (make-game (list (make-snake (+ (* 1 SNAKE-SPEED) 100) (+ (* 0 SNAKE-SPEED) 200) 1 0)
                               (make-snake (+ (* 1 SNAKE-SPEED) 80) (+ (* 0 SNAKE-SPEED) 200) 1 0))
                         (make-apple 10 20))) ;default game state change, snake advances SNAKE-SPEED and nothing else happens
(check-random (next-game (make-game (list (make-snake 100 200 1 0) (make-snake 80 200 1 0)) (make-apple 100 200)))
              (make-game (list (make-snake (+ (* 1 SNAKE-SPEED) 100) (+ (* 0 SNAKE-SPEED) 200) 1 0)
                               (make-snake (+ (* 1 SNAKE-SPEED) 80) (+ (* 0 SNAKE-SPEED) 200) 1 0)
                               (make-snake (+ (* 1 SNAKE-SPEED) (- 80 (* 1 SNAKE-SPEED))) (+ (* 0 SNAKE-SPEED) (- 200 (* 0 SNAKE-SPEED))) 1 0))
                         (make-apple (* (random (/ (- WIDTH (* 2 SNAKE-SPEED)) SNAKE-SPEED)) SNAKE-SPEED)
                                     (* (random (/ (- HEIGHT (* 2 SNAKE-SPEED)) SNAKE-SPEED)) SNAKE-SPEED)))) ;snake eats apple, apple changes position (random), snake gains 1 segment after previous last segment

;(define (next-game g) g) ;stub

(define (next-game g)
  (make-game (next-los (game-los g) (game-apple g))
             (next-apple (game-apple g) (game-los g))))


;; ListOfSnake Apple -> ListOfSnake
;; produce the next list of snake given the previous one and the apple position
(check-expect (next-los (list (make-snake 100 200 1 0) (make-snake 80 200 1 0)) (make-apple 10 20))
              (list (make-snake (+ (* 1 SNAKE-SPEED) 100) (+ (* 0 SNAKE-SPEED) 200) 1 0)
                    (make-snake (+ (* 1 SNAKE-SPEED) 80) (+ (* 0 SNAKE-SPEED) 200) 1 0))) ;snake advances SNAKE-SPEED
(check-expect (next-los (list (make-snake 100 200 1 0) (make-snake 80 200 1 0)) (make-apple 100 200))
              (list (make-snake (+ (* 1 SNAKE-SPEED) 100) (+ (* 0 SNAKE-SPEED) 200) 1 0)
                    (make-snake (+ (* 1 SNAKE-SPEED) 80) (+ (* 0 SNAKE-SPEED) 200) 1 0)
                    (make-snake (+ (* 1 SNAKE-SPEED) (- 80 (* 1 SNAKE-SPEED))) (+ (* 0 SNAKE-SPEED) (- 200 (* 0 SNAKE-SPEED))) 1 0))) ;snake eats apple, gets bigger

;(define (next-los los a) los) ;stub

(define (next-los los a)
  (tick-los
   (collide-snake los a)))


;; ListOfSnake Apple -> ListOfSnake
;; given a list of snake and an apple, check for collisions bewteen the two and return a new updated list of snake
(check-expect (collide-snake (list (make-snake 100 200 1 0) (make-snake 80 200 1 0)) (make-apple 10 20))
              (list (make-snake 100 200 1 0)
                    (make-snake 80 200 1 0))) ;snake didn't collide, doesn't change
(check-expect (collide-snake (list (make-snake 100 200 1 0) (make-snake 80 200 1 0)) (make-apple 100 200))
              (list (make-snake 100 200 1 0)
                    (make-snake 80 200 1 0)
                    (make-snake (- 80 (* 1 SNAKE-SPEED)) (- 200 (* 0 SNAKE-SPEED)) 1 0))) ;snake eats apple, gets bigger

;(define (collide-snake los a) los) ;stub

(define (collide-snake los a)
  (if (collide-snake? (first los) a)
      (insert-segment los)
      los))


;; Snake Apple -> Boolean
;; given a snake (head) and an apple, return true if they colided, false otherwise
(check-expect (collide-snake? (make-snake 20 20 0 1) (make-apple 40 20)) false) ;snake head not touching apple
(check-expect (collide-snake? (make-snake 40 40 0 1) (make-apple 40 40)) true) ;snake head touching apple

;(define (collide-snake? s a) false) ;stub

(define (collide-snake? s a)
  (and (= (- (snake-x s) (apple-x a)) 0)
       (= (- (snake-y s) (apple-y a)) 0)))


;; ListOfSnake -> ListOfSnake
;; given a list of snake, insert an element after its last with position subtracted by SNAKE-SPEED
(check-expect (insert-segment (list (make-snake 20 40 0 1) (make-snake 20 20 0 1)))
              (list (make-snake 20 40 0 1) (make-snake 20 20 0 1) (make-snake (- 20 (* 0 SNAKE-SPEED)) (- 20 (* 1 SNAKE-SPEED)) 0 1)))
              
;(define (insert-segment los) los) ;stub

(define (insert-segment los)
  (cond [(empty? (rest los)) (cons (first los) (cons (calculate-tail (first los)) empty))]
        [else
         (cons (first los) (insert-segment (rest los)))]))


;; Snake -> Snake
;; given a snake segment, return a new segment subtracted SNAKE-SPEED from the first, according to dx and dy
(check-expect (calculate-tail (make-snake 40 60 -1 0)) (make-snake (- 40 (* -1 SNAKE-SPEED)) (- 60 (* 0 SNAKE-SPEED)) -1 0))

;(define (calculate-tail s) s) ;stub

(define (calculate-tail s)
  (make-snake (- (snake-x s) (* (snake-dx s) SNAKE-SPEED))
              (- (snake-y s) (* (snake-dy s) SNAKE-SPEED))
              (snake-dx s)
              (snake-dy s)))


;; ListOfSnake -> ListOfSnake
;; given a list of snake, advance it by SNAKE-SPEED and pass down the directions (dx, dy)
(check-expect (tick-los (list (make-snake 20 20 1 0)
                                   (make-snake 20 0 0 1)))
              (list (make-snake (+ (* 1 SNAKE-SPEED) 20) (+ (* 0 SNAKE-SPEED) 20) 1 0)
                    (make-snake (+ (* 0 SNAKE-SPEED) 20) (+ (* 1 SNAKE-SPEED) 0) 1 0)))

;(define (tick-los los) los) ;stub

(define (tick-los los)
  (direction-los
   (first los)
   (advance-los los)))


;; ListOfSnake -> ListOfSnake
;; given a list of snake, advance it by SNAKE-SPEED
(check-expect (advance-los (list (make-snake 20 20 0 1)
                                   (make-snake 20 0 0 1)))
              (list (make-snake (+ (* 0 SNAKE-SPEED) 20) (+ (* 1 SNAKE-SPEED) 20) 0 1)
                    (make-snake (+ (* 0 SNAKE-SPEED) 20) (+ (* 1 SNAKE-SPEED) 0) 0 1)))

;(define (advance-los los) los) ;stub

(define (advance-los los)
  (cond [(empty? los) empty]
        [else
         (cons (advance-snake (first los))
               (advance-los (rest los)))]))


;; Snake -> Snake
;; given a snake segment advance it's x and y position by SNAKE-SPEED according to directions dx and dy
(check-expect (advance-snake (make-snake 100 120 0 -1))
              (make-snake (+ (* 0 SNAKE-SPEED) 100) (+ (* -1 SNAKE-SPEED) 120) 0 -1))

;(define (advance-snake s) s) ;stub

(define (advance-snake s)
  (make-snake (+ (* (snake-dx s) SNAKE-SPEED) (snake-x s))
              (+ (* (snake-dy s) SNAKE-SPEED) (snake-y s))
              (snake-dx s)
              (snake-dy s)))


;; Snake ListOfSnake -> ListOfSnake
;; given a snake segment and the list of snake, update all segment directions according to segments ahead
(check-expect (direction-los (make-snake 200 100 1 0)
                             (list (make-snake 220 100 1 0) (make-snake 200 100 0 1) (make-snake 200 80 0 1)))
              (list (make-snake 220 100 1 0) (make-snake 200 100 1 0) (make-snake 200 80 0 1)))

;(define (direction-los s los) los) ;stub

(define (direction-los s los)
  (cond [(empty? los) empty]
        [else
         (cons (direction-snake s (first los))
               (direction-los (first los) (rest los)))]))


;; Snake Snake -> Snake
;; given first and second snake segment, copy directions from the first to the second
(check-expect (direction-snake (make-snake 40 40 0 1) (make-snake 40 20 1 0))
              (make-snake 40 20 0 1))

;(define (direction-snake h s) s) ;stub

(define (direction-snake h s)
  (make-snake (snake-x s)
              (snake-y s)
              (snake-dx h)
              (snake-dy h)))


;; Apple ListOfSnake -> Apple
;; produce the next apple state, change apple location if conditions with list of snake are met
(check-random (next-apple (make-apple 20 40) (list (make-snake 200 200 1 0) (make-snake 180 200 1 0)))
              (make-apple 20 40)) ;snake didn't eat apple, apple doesn't change position
(check-random (next-apple (make-apple 200 200) (list (make-snake 200 200 1 0) (make-snake 180 200 1 0)))
              (make-apple (* (random (/ (- WIDTH (* 2 SNAKE-SPEED)) SNAKE-SPEED)) SNAKE-SPEED)
                          (* (random (/ (- HEIGHT (* 2 SNAKE-SPEED)) SNAKE-SPEED)) SNAKE-SPEED))) ;snake eats apple, apple changes position (random)

;(define (next-apple a los) a) ;stub

(define (next-apple a los)
  (if (collide-apple? a (first los))
      (generate-apple los)
      a))


;; Apple Snake -> Boolean
;; given an apple and a snake head, produce true is snake head hits apple, false otherwise
(check-expect (collide-apple? (make-apple 20 20) (make-snake 40 40 0 1)) false)
(check-expect (collide-apple? (make-apple 20 20) (make-snake 20 20 0 1)) true)

;(define (collide-apple? a los) true) ;stub

(define (collide-apple? a s)
  (and (= (apple-x a) (snake-x s))
       (= (apple-y a) (snake-y s))))


;; ListOfSnake -> Apple
;; given a list of snake, produce the next apple randomly across the screen
;; can't be where snake is currently in
(check-random (generate-apple (list (make-snake 100 80 -1 0) (make-snake 120 80 -1 0)))
              (make-apple (* (random (/ (- WIDTH (* 2 SNAKE-SPEED)) SNAKE-SPEED)) SNAKE-SPEED)
                          (* (random (/ (- HEIGHT (* 2 SNAKE-SPEED)) SNAKE-SPEED)) SNAKE-SPEED)))

;(define (generate-apple los) a) ;stub

(define (generate-apple los)
  (let ([TEST-APPLE (make-apple (* (random (/ (- WIDTH (* 2 SNAKE-SPEED)) SNAKE-SPEED)) SNAKE-SPEED)
                                (* (random (/ (- HEIGHT (* 2 SNAKE-SPEED)) SNAKE-SPEED)) SNAKE-SPEED))])
    (if (empty-space? TEST-APPLE los)
        TEST-APPLE
        (generate-apple los))))


;; Apple ListOfSnake -> Boolean
;; given an apple and a list of snake, check if the apple position is on an empty space or not
(check-expect (empty-space? (make-apple 40 80) (list (make-snake 200 200 0 1) (make-snake 180 200 0 1))) true)
(check-expect (empty-space? (make-apple 40 80) (list (make-snake 60 80 0 1) (make-snake 40 80 0 1))) false)
(check-expect (empty-space? (make-apple 60 80) (list (make-snake 60 80 0 1) (make-snake 40 80 0 1))) false)

;(define (empty-space? a los) true) ;stub

(define (empty-space? a los)
  (cond [(empty? los) true]
        [else
         (if (collide-apple? a (first los))
             false
             (empty-space? a (rest los)))]))


;; Game -> Image
;; render the current state of the game with all entities
(check-expect (render-game (make-game (list (make-snake 20 40 -1 0) (make-snake 40 40 -1 0)) (make-apple 100 120)))
              (overlay (place-image SNAKE-IMG 20 40 (place-image SNAKE-IMG 40 40 MTS))
                       (place-image APPLE-IMG 100 120 MTS)
                       (rectangle WIDTH HEIGHT "solid" "white")))

;(define (render-game g) g) ;stub

(define (render-game g)
  (overlay (render-los (game-los g))
           (render-apple (game-apple g))
           (rectangle WIDTH HEIGHT "solid" "white")))


;; ListOfSnake -> Image
;; given a list of snake, render its image
(check-expect (render-los (list (make-snake 20 40 -1 0) (make-snake 40 40 -1 0)))
              (place-image SNAKE-IMG 20 40 (place-image SNAKE-IMG 40 40 MTS)))

;(define (render-los los) MTS) ;stub

(define (render-los los)
  (cond [(empty? los) MTS]
        [else
         (place-image SNAKE-IMG
                      (snake-x (first los))
                      (snake-y (first los))
                      (render-los (rest los)))]))


;; Apple -> Image
;; given an apple, render its image
(check-expect (render-apple (make-apple 20 40))
              (place-image APPLE-IMG 20 40 MTS))

;(define (render-apple a) MTS) ;stub

(define (render-apple a)
  (place-image APPLE-IMG
               (apple-x a)
               (apple-y a)
               MTS))


;; Game -> Boolean
;; given a game state, returns true if game should end, false otherwise 
;; game ends when snake hits wall or hits ifselt
(check-expect (end-game? (make-game (list (make-snake 20 40 -1 0)
                                          (make-snake 40 40 -1 0))
                                    (make-apple 100 120))) false) ;regular game state
(check-expect (end-game? (make-game (list (make-snake WIDTH 40 1 0)
                                          (make-snake (- WIDTH 20) 40 1 0))
                                    (make-apple 100 120))) true) ;snake hits right wall
(check-expect (end-game? (make-game (list (make-snake 120 20 0 -1)
                                          (make-snake 120 40 0 -1)
                                          (make-snake 140 40 -1 0)
                                          (make-snake 140 20 0 1)
                                          (make-snake 120 20 1 0))
                                    (make-apple 100 120))) true) ;snake hits itself
              
;(define (end-game? g) false) ;stub

(define (end-game? g)
  (or (wall-hit? (first (game-los g)))
      (snake-hit? (first (game-los g)) (rest (game-los g)))))


;; Snake -> Boolean
;; given a snake head, return true if wall hit, false otherwise
(check-expect (wall-hit? (make-snake 20 40 1 0)) false)
(check-expect (wall-hit? (make-snake WIDTH 40 1 0)) true)
(check-expect (wall-hit? (make-snake 0 40 1 0)) true)
(check-expect (wall-hit? (make-snake 40 HEIGHT 1 0)) true)
(check-expect (wall-hit? (make-snake 40 0 1 0)) true)

;(define (wall-hit? s) false) ;stub

(define (wall-hit? s)
  (cond [(= (snake-x s) 0) true]
        [(= (snake-x s) WIDTH) true]
        [(= (snake-y s) 0) true]
        [(= (snake-y s) HEIGHT) true]
        [else false]))


;; Snake ListOfSnake -> Boolean
;; given a snake head and its body, returns true if head hits body, false otherwise
(check-expect (snake-hit? (make-snake 120 20 0 -1)
                          (list (make-snake 120 40 0 -1)
                                (make-snake 140 40 -1 0)
                                (make-snake 140 20 0 1)
                                (make-snake 120 20 1 0))) true)
(check-expect (snake-hit? (make-snake 100 40 -1 0)
                          (list (make-snake 120 40 -1 0)
                                (make-snake 140 40 -1 0)
                                (make-snake 140 20 0 1)
                                (make-snake 120 20 1 0))) false)

;(define (snake-hit? s los) false) ;stub

(define (snake-hit? s los)
  (cond [(empty? los) false]
        [else
         (if (and (= (snake-x s) (snake-x (first los)))
             (= (snake-y s) (snake-y (first los))))
             true
             (snake-hit? s (rest los)))]))


;; Game KeyEvent -> Game
;; given a key event (left/right/up/down arrow) chance the directions of the snake
;; !!!

;(define (handle-key g ke) g) ;stub

(define (handle-key g ke)
  (cond [(key=? ke "left") (control-snake g -1 0)]
        [(key=? ke "right") (control-snake g 1 0)]
        [(key=? ke "up") (control-snake g 0 -1)]
        [(key=? ke "down") (control-snake g 0 1)]
        [else g]))


;; Game Integer[-1, 1] Integer[-1, 1] -> Game
;; given a game state and two integers [-1, 1], change de direction (dx, dy) of the snake head
;; direction remains unchanged if it's in the complete opposite direction
;; !!!

;(define (control-snake g dx dy) g) ;stub

(define (control-snake g dx dy)
  (if (or (and (not (zero? dx))
               (not (zero? (snake-dx (first (game-los g)))))
               (= dx (- (snake-dx (first (game-los g))))))
          (and (not (zero? dy))
               (not (zero? (snake-dy (first (game-los g)))))
               (= dy (- (snake-dy (first (game-los g)))))))
      g
      (make-game (cons (make-snake (snake-x (first (game-los g)))
                                   (snake-y (first (game-los g)))
                                   dx
                                   dy)
                       (rest (game-los g)))
                 (game-apple g))))