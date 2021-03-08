;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname next-season) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;Season is one of:
;; -"spring"
;; -"summer"
;; -"fall"
;; -"winter"
;;interp. the four seasons of the year
;;<example are redundant for enumerations>
#;
(define (fn-for-season s)
   (cond [(string=? "spring" s) (...)]
         [(string=? "summer" s) (...)]
         [(string=? "fall" s) (...)]
         [(string=? "winter" s) (...)]))
;;Template rules used:
;; -one of: 4 cases
;; -atomic distinct: "spring"
;; -atomic distinct: "summer"
;; -atomic distinct: "fall"
;; -atomic distinct: "winter"

;;Functons:
;;Season ->season
;;produce the next season that follows
(check-expect (next-season "spring") "summer")
(check-expect (next-season "summer") "fall")
(check-expect (next-season "fall") "winter")
(check-expect (next-season "winter") "spring")
;stub (define (next-season s) "spring")
(define (next-season s)
  (cond [(string=? s "spring") "summer"]
        [(string=? s "summer") "fall"]
        [(string=? s "fall") "winter"]
        [(string=? s "winter") "spring"]))