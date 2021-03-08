;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders


;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 10)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 50)

(define BACKGROUND (empty-scene WIDTH HEIGHT))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))
(define TANK-WIDTH/2 (/ (image-width TANK) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))



;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))



(define-struct tank (x dir shoot-missile))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1 false))   ;center going right
(define T1 (make-tank 50 1 false))            ;going right
(define T2 (make-tank 50 -1 false))           ;going left
 
#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))



(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))

(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))

;;Functions:
;===========
;;Game -> Game
;;the world starts with one tank shoting missiles on tick

(define (never x) false)

(define (main game)
  (big-bang game                   ; WS
            (on-tick   advance-state)     ; Game -> Game
            (to-draw   render)   ; WS -> Image
            (stop-when over?)      ; WS -> Boolean
            (on-key    control)))    ; WS KeyEvent -> WS

;--------------------------------------------------------------

;;Game -> Boolean
;;game over

(define (over? s)
  (invaders-reach? (game-invaders s)))

;;Invader -> Boolean

(define (invader-reach? i)
  (>= (invader-y i) HEIGHT))

;;ListOfInvaders -> Boolean

(define (invaders-reach? loi)
  (cond [(empty? loi) false]
        [else (or (invader-reach? (first loi))
         (invaders-reach? (rest loi)))]))

;;GameState KeyEvent -> GameState
;;move the tank with right and left key
(check-expect (control G0 "right")
              (make-game empty empty (make-tank (/ WIDTH 2) 1 false)))
(check-expect (control G1 "right")
              (make-game empty empty (make-tank 50 1 false)))
(check-expect (control G1 "left")
              (make-game empty empty (make-tank 50 -1 false)))
(check-expect (control G1 "d")
              (make-game empty empty T1))

;(define (control-tank s key) s)
;Template from keyevent

(define (control s key)
  (cond [(key=? key "right") (make-game (game-invaders s) (game-missiles s) (tank-right (game-tank s)))]
        [(key=? key "left") (make-game (game-invaders s) (game-missiles s) (tank-left (game-tank s)))]
        [(key=? key "s") (launch s)]
        [else s]))

;;Tank -> Tank
;;move the tank to the right

;;(define (tank-right t)
;;  (cond [(>= (+ (tank-x t) TANK-SPEED) 300) (make-tank (- 300 TANK-WIDTH/2) 1)]
;;       [else (make-tank (+ (tank-x t) TANK-SPEED) (tank-dir t))]))

;;move the tank to the left
;(check-expect (tank-left T1)
;              (make-tank (- 50 TANK-SPEED) 1))

;
;(define (tank-left t)
;  (cond [(<= (- (tank-x t) TANK-SPEED) 0) (make-tank (+ 0 TANK-WIDTH/2) 1)]
;        [else (make-tank (- (tank-x t) TANK-SPEED) (tank-dir t))]))

(define (tank-left t)
  (make-tank (tank-x t) -1 (tank-shoot-missile t)))

(define (tank-right t)
  (make-tank (tank-x t) 1 (tank-shoot-missile t)))

;;Game -> Game
;;produce missile


(define (launch s)
  (make-game (game-invaders s) (game-missiles s) (make-tank (tank-x (game-tank s)) (tank-dir (game-tank s)) true)))

;;(define (launch s)
;;  (make-game (game-invaders s) (cons (make-missile (tank-x (game-tank s)) (- 500 TANK-HEIGHT/2)) (game-missiles s)) (game-tank s)))

;; tank -> tank
(define (move-tank t)
  (cond [(= 0 (tank-dir t)) t]
        [(= -1 (tank-dir t))
         (make-tank (max TANK-WIDTH/2
                         (- (tank-x t) TANK-SPEED))
                    0
                    (tank-shoot-missile t))]
        [(= 1  (tank-dir t))
         (make-tank (min (- WIDTH TANK-WIDTH/2)
                         (+ (tank-x t) TANK-SPEED))
                    0
                    (tank-shoot-missile t))]))

;; game -> game
(define (advance-tank s)
  (make-game (game-invaders s) (game-missiles s) (move-tank (game-tank s))))

;; missile -> missile
(define (move-missile m)
  (make-missile (missile-x m) (- (missile-y m) MISSILE-SPEED)))

;;ListOfMissiles -> ListOfMissiles
;;move missiles

(define (move-missiles lom)
  (cond [(empty? lom) lom]
        [else (cons (move-missile (first lom)) (move-missiles (rest lom)))]))

(define (bye-missiles lom)
  (cond [(empty? lom) lom]
        [(<= (missile-y (first lom)) 0) (bye-missiles (rest lom))]
        [else (cons (first lom) (bye-missiles (rest lom)))]))


;; game -> game
(define (advance-missiles s)
  (make-game (game-invaders s) (bye-missiles (move-missiles (game-missiles s))) (game-tank s)))

(define (shoot-missile s)
  (if (tank-shoot-missile (game-tank s))
      (make-game (game-invaders s)
                 (cons (make-missile (tank-x (game-tank s)) (- 500 TANK-HEIGHT/2)) (game-missiles s))
                 (make-tank (tank-x (game-tank s)) (tank-dir (game-tank s)) false))
      s))

;;Game -> Game
;;move everything

(define (advance-state s)
   (random-invaders (advance-invaders (remove-hit (advance-tank (shoot-missile (advance-missiles s)))))))


;;Invader -> Invader
;;move invader (x, y) by dx, when hits the wall, change the x
(check-expect (tick-invader I1)
              (make-invader (+ 150 12) (+ 100 INVADER-Y-SPEED) 12))


(define (tick-invader i)
  (cond [(>= (invader-x i) WIDTH)
                       (make-invader (- (invader-x i) (invader-dx i)) (+ (invader-y i) INVADER-Y-SPEED) (* (invader-dx i) -1))]
                      [(<= (invader-x i) 0)
                       (make-invader (- (invader-x i) (invader-dx i)) (+ (invader-y i) INVADER-Y-SPEED) (* (invader-dx i) -1))]
                      [else (make-invader (+ (invader-x i) (invader-dx i)) (+ (invader-y i) INVADER-Y-SPEED) (invader-dx i))]))

;;LIstOfInvaders -> ListOfInvaders
;;produce a list of ticked invaders
(check-expect (tick-invaders empty) empty)
(check-expect (tick-invaders (list (make-invader 10 20 12) (make-invader 30 20 10)))
              (list (make-invader (+ 10 12) (+ 20 INVADER-Y-SPEED) 12)
                                   (make-invader (+ 30 10) (+ 20 INVADER-Y-SPEED) 10)))

(define (tick-invaders invaders)
  (cond [(empty? invaders) empty]
        [else (cons (tick-invader (first invaders))
                    (tick-invaders (rest invaders)))]))


;;Game -> Game

(define (advance-invaders s)
  (make-game (tick-invaders (game-invaders s)) (game-missiles s) (game-tank s)))

;;Missile Invader -> Boolean
;;produce true if the invader dodgede the missile
(check-expect (hit? (make-invader 80 20 10) (make-missile 100 50)) false)
(check-expect (hit? (make-invader 80 100 10) (make-missile 80 100)) true)

(define (hit? i m)
  (and (<= (abs (- (invader-x i) (missile-x m))) HIT-RANGE)
       (<= (abs (- (invader-y i) (missile-y m))) HIT-RANGE)))

;;ListOfInvaders LIstOfMissiles -> ListOfMissiles

(define (dodged-missiles loi lom)
  (cond [(empty? lom) empty]
        [else (if (hit-missile? (first lom) loi)
                  (dodged-missiles loi (rest lom))
                  (cons (first lom) (dodged-missiles loi (rest lom))))]))

;;ListOfInvaders ListOfMissiles -> ListOfInvaders

(define (dodged-invaders loi lom)
  (cond [(empty? loi) empty]
        [else (if (hit-invader? (first loi) lom)
                  (dodged-invaders (rest loi) lom)
                  (cons (first loi) (dodged-invaders (rest loi) lom)))]))

;;Invaders ListOfMissiles -> Boolean

(define (hit-invader? i lom)
  (cond [(empty? lom) false]
        [else (or (hit? i (first lom))
                  (hit-invader? i (rest lom)))]))

;;Missiles ListOfInvaders -> Boolean

(define (hit-missile? m loi)
  (cond [(empty? loi) false]
        [else (or (hit? (first loi) m)
                  (hit-missile? m (rest loi)))]))

;;Game -> Game
;;remove the hit missiles and invaders
(define (remove-hit s)
  (make-game (dodged-invaders (game-invaders s) (game-missiles s)) (dodged-missiles (game-invaders s) (game-missiles s)) (game-tank s)))

;;Tank -> Image
;;place a tank for the render-tank
(check-expect (place-tank (make-tank 50 1 1) BACKGROUND)
              (place-image TANK 50 (- HEIGHT TANK-HEIGHT/2) BACKGROUND))

(define (render s)
  (place-invaders (game-invaders s) (place-missiles (game-missiles s) (place-tank (game-tank s) BACKGROUND))))

(define (place-tank t img)
  (place-image TANK (tank-x t) (- HEIGHT TANK-HEIGHT/2) img))

(define (should-spawn-invader unused) (= (random INVADE-RATE) 1))

;;Nothing -> Invader
;;make an invader
(define (hi-invader unused)
  (make-invader (random WIDTH) 0 INVADER-X-SPEED))

;;Game-> Game
;;add invaders to the game
(define (random-invaders s)
  (make-game (if (should-spawn-invader 0)
                 (cons (hi-invader 0) (game-invaders s))
                 (game-invaders s))
             (game-missiles s)
             (game-tank s)))

;;Missile -> Image
;;show missile
(check-expect (place-missile (make-missile 80 1) BACKGROUND)
              (place-image MISSILE 80 1 BACKGROUND))

(define (place-missile m img)
  (place-image MISSILE (missile-x m) (missile-y m) img))

(define (place-missiles lom img)
  (cond [(empty? lom) img]
        [else (place-missile (first lom) (place-missiles (rest lom) img))]))

;;Invader -> Image
;;show invader

(define (place-invader i img)
  (place-image INVADER (invader-x i) (invader-y i) img))

(define (place-invaders loi img)
  (cond [(empty? loi) img]
        [else (place-invader (first loi) (place-invaders (rest loi) img))]))