;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname HW) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ())))
; Problem a
; Given: 2x - (3 + 4 + 5)
; Step 1: ((2x) - ((3 + 4) + 5))
; Step 2: ((2 * x) - ((3 + 4) + 5))
; Step 3: (- (* 2 x) (+ (+ 3 4) 5))
(define x 3)
(check-expect (- (* 2 x) (+ (+ 3 4) 5)) -6)

; Problem b
; Given: (5 + 4) * 3
; Step 1: ((5 + 4) * 3)
; Step 2: ((5 + 4) * 3)
; Step 3: (* (+ 5 4) 3)
(check-expect (* (+ 5 4) 3) 27)

; Problem c
; Given: (-7 / (8 + (9 * 5)))
; Step 1: (-7 / (8 + (9 * 5)))
; Step 2: (-7 / (8 + (9 * 5)))
; Step 3: (/ -7 (+ 8 (* 9 5)))
(check-expect (/ -7 (+ 8 (* 9 5))) -7/53)

; Problem d
; Given: (7 * 0.5) + (6 * 0.4) + (5 * 0.3)
; Step 1: (((7 * 0.5) + (6 * 0.4)) + (5 * 0.3))
; Step 2: (((7 * 0.5) + (6 * 0.4)) + (5 * 0.3))
; Step 3: (+ (+ (* 7 0.5) (* 6 0.4)) (* 5 0.3))
(check-expect (+ (+ (* 7 0.5) (* 6 0.4)) (* 5 0.3)) 7.4)

; Problem e
; Given: (100 + 94 + 96) / 3
; Step 1: (((100 + 94 + 96)) / 3)
; Step 2: (((100 + 94) + 96) / 3)
; Step 3: (/ (+ (+ 100 94) 96) 3)
(check-expect (/ (+ (+ 100 94) 96) 3) 290/3)

; Problem f
; Given: (11 - (((27 / 18) / 131) + 927))
; Step 1: ((11 - (((27 / 18) / 131)) + 927))
; Step 2: ((11 - (((27 / 18) / 131)) + 927))
; Step 3: (+ (- 11 (/ (/ 27 18) 131)) 927)
(check-expect (+ (- 11 (/ (/ 27 18) 131)) 927) 245753/262)

; Problem g
; Given: 5xy
; Step 1: ((5x)y)
; Step 2: ((5 * x) * y)
; Step 3: (* (* 5 x) y)
(define y 1)
(check-expect (* (* 5 x) y) 15)

; Problem h
; Given: 2x + 3y
; Step 1: ((2x) + (3y))
; Step 2: ((2 * x) + (3 * y))
; Step 3: (+ (* 2 x) (* 3 y))
(check-expect (+ (* 2 x) (* 3 y)) 9)

; Problem i
; Given: 2 (x + yz)
; Step 1: (2 (x + (yz)))
; Step 2: (2 * (x + (y * z)))
; Step 3: (* 2 (+ x (* y z)))
(define z 2)
(check-expect (* 2 (+ x (* y z))) 10)

; Problem j
; Given: 2 (x + 3) y
; Step 1: ((2 (x + 3)) y)
; Step 2: ((2 * (x + 3)) * y)
; Step 3: (* (* 2 (+ x 3)) y)
(check-expect (* (* 2 (+ x 3)) y) 12)

; Problem k
; Given: (x + y) / (x - y)
; Step 1: ((x + y) / (x - y))
; Step 2: ((x + y) / (x - y))
; Step 3: (/ (+ x y) (- x y))
(check-expect (/ (+ x y) (- x y)) 2)

; Problem l
; Given: (x + y) / x - y
; Step 1: (((x + y) / x) - y)
; Step 2: (((x + y) / x) - y)
; Step 3: (- (/ (+ x y) x) y)
(check-expect (- (/ (+ x y) x) y) 1/3)

; Problem m
; Given: v + w - x + y - z
; Step 1: ((((v + w) - x) + y) - z)
; Step 2: ((((v + w) - x) + y) - z)
; Step 3: (- (+ (- (+ v w) x) y) z)
(define v 4)
(define w 5)
(check-expect (- (+ (- (+ v w) x) y) z) 5)

; Problem n
; Given: v + (w - x) + y - z
; Step 1: (((v + (w - x)) + y) - z)
; Step 2: (((v + (w - x)) + y) - z)
; Step 3: (- (+ (+ v (- w x)) y) z)
(check-expect (- (+ (+ v (- w x)) y) z) 5)

; Problem o
; Given: v + (w - x) + (y - z)
; Step 1: ((v + (w - x)) + (y - z))
; Step 2: ((v + (w - x)) + (y - z))
; Step 3: (+ (+ v (- w x)) (- y z))
(check-expect (+ (+ v (- w x)) (- y z)) 5)

; Problem p
; Given: v + w - (x + y) - z
; Step 1: (((v + w) - (x + y)) - z)
; Step 2: (((v + w) - (x + y)) - z)
; Step 3: (- (- (+ v w) (+ x y)) z)
(check-expect (- (- (+ v w) (+ x y)) z) 3)

; Problem q
; Given: v + w - (x + y - z)
; Step 1: ((v + w) - ((x + y) - z))
; Step 2: ((v + w) - ((x + y) - z))
; Step 3: (- (+ v w) (- (+ x y) z))
(check-expect (- (+ v w) (- (+ x y) z)) 7)