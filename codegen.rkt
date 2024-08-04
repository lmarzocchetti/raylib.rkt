#lang racket

(require "parser.rkt")

(provide generate-structs-ffi)

(define (to-crkt-type old-type)
  (match old-type
    ["float" "_float"]
    ["double" "_double"]
    ["char" "_sbyte"]
    ["int" "_int"]
    ["unsigned char" "_ubyte"]
    ["unsigned short" "_ushort"]
    ["unsigned int" "_uint"]
    ["void" "_void"]
    ["bool" "_stdbool"]
    [_ (string-append "_" old-type)]))

(struct struct-decl (name [fields #:mutable]) #:transparent)
(struct struct-field (name type) #:transparent)

(define (parse-declaration decls)
  (for/list ([single decls])
    ; (print single)
    (cond
      [(string-contains? single ",")
       (let* ([splitted (map string-trim (string-split single ","))]
              [type (first (string-split (first splitted) " "))]
              [names (reverse (append (reverse (rest splitted)) (list (second (string-split (first splitted) " ")))))])
         (struct-field names (to-crkt-type type)))]
      ;`(,(to-crkt-type type) ,names))]
      [(string-contains? single "unsigned")
       (let* ([name (third (string-split single " "))]
              [type-s (take (string-split single " ") 2)]
              [type (string-append (first type-s) " " (second type-s))])
         (struct-field name (to-crkt-type type)))]
      ; `(,(to-crkt-type type) ,name))]
      [else
       (let* ([type (first (string-split single " "))]
              [name (second (string-split single " "))])
         (struct-field name (to-crkt-type type)))]
      ; `(,(to-crkt-type type) ,name))]
      )))

(define (to-racket-cstruct to-parse)
  (let ([name (substring (last to-parse) 2 (- (string-length (last to-parse)) 1))]
        [fields (map (lambda (s) (substring s 0 (- (string-length s) 1))) (reverse (drop (reverse (rest to-parse)) 1)))])
    ;(map (lambda (s) (string-split s " ")) fields)
    (struct-decl (string-append "_" name) (flatten (parse-declaration fields)))))

(define (generate-structs-ffi)
  (let ([structs (raylib-h-structs raylib-h-source)])
    (with-output-to-file "raylib.rkt"
      (lambda ()
        (printf "#lang racket~n")
        (printf "(require ffi/unsafe)~n")
        (for/list ([def structs]
                   ; TODO: Excluding opaque struct definition (rAudio)
                   #:when (not (equal? 1 (length def))))
          (let ([strct (to-racket-cstruct def)])
            (printf "(define-cstruct ~a (" (struct-decl-name strct))
            (for/list ([decl (struct-decl-fields strct)])
              ; TODO: Pointers!
              (let* ([name (struct-field-name decl)]
                     [flds (struct-field-type decl)])
                (if (not (list? name))
                    (printf "[~a ~a]~n" name flds)
                    (for/list ([sngl name])
                      (printf "[~a ~a]~n" sngl flds)))))
            (printf "))~n")))))))