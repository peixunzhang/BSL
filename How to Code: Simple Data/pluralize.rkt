;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname pluralize) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; string -> string
;; adds s to words
(check-expect (pluralize "cat") "cats")
(check-expect (pluralize "dog") "dogs")
;;(define (pluralize str) str) ;this is the stub
;;(define (pluralize str) (... str)) ;this is the template
(define (pluralize n) (string-append n "s"))