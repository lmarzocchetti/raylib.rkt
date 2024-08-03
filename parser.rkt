#lang racket

(define raylib-h-source 
  (let* ([raylib-source (port->string (open-input-file "raylib-5.0/src/raylib.h") #:close? #t)]
         [splitted (map string-trim (string-split raylib-source "\n"))])
    (cdr (filter 
      (lambda (elem) (not (or 
                            (equal? 0 (string-length elem)) 
                            (equal? #\# (string-ref elem 0)) 
                            (equal? #\/ (string-ref elem 0))
                            (equal? #\* (string-ref elem 0))
                            (equal? "extern" (first (string-split elem " "))))))
      splitted))))

(define (raylib-h-structs source) 
  (for/list ([line source]
             [index (in-naturals 0)]
             #:when (let ([splitted (string-split line " ")])
                      (and (equal? (first splitted) "typedef") (equal? (second splitted) "struct"))))
    (append
      (list line) 
      (for/list ([l (drop source (+ index 1))]
                 #:break (string-contains? l "typedef"))
        (string-trim (first (string-split l "//")))))))

(define (raylib-h-enums source) 
  (for/list ([line source]
             [index (in-naturals 0)]
             #:when (let ([splitted (string-split line " ")])
                      (and (equal? (first splitted) "typedef") (equal? (second splitted) "enum"))))
    (append
      (list line) 
      (for/list ([l (drop source (+ index 1))]
                 #:break (string-contains? l "typedef"))
        (string-trim (first (string-split l "//")))))))

(define (raylib-h-functions source)
  (for/list ([line source]
             #:when (let ([splitted (string-split line " ")])
                      (equal? (first splitted) "RLAPI")))
    (string-trim (string-trim (first (string-split line "//")) "RLAPI "))))

