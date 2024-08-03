#lang racket

(require "parser.rkt")

(provide generate-structs-ffi)

(define (to-racket-cstruct to-parse)
  (let ([name (substring (last to-parse) 2 (- (string-length (last to-parse)) 1))]
        [fields (map (lambda (s) (substring s 0 (- (string-length s) 1))) (reverse (drop (reverse (rest to-parse)) 1)))])
    (map (lambda (s) (string-split s " ")) fields)))

(define (generate-structs-ffi)
  (let ([structs (raylib-h-structs raylib-h-source)])
    (for/list ([def structs]
               ; TODO: Excluding opaque struct definition (rAudio)
               #:when (not (equal? 1 (length def))))
      (to-racket-cstruct def))))