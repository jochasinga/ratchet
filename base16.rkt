#lang racket

(provide (contract-out
          [hex->rgb (-> string? list?)]
          [base16->10 (-> char? (and/c exact-nonnegative-integer? u8?))]
          [base10->16 (-> (and/c exact-nonnegative-integer? u8?) char?)]
          [u8? (-> exact-nonnegative-integer? boolean?)]))

(define HEX_DIGITS
  '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\A #\B #\C #\D #\E #\F))

;; Check if an integer is within an 8-bit range.
(define (u8? n) (and (> n 0) (< n 256)))

;; Convert a single-digit, base-16 character to an 8-bit unsigned integer.
(define (base16->u8 c) (* (index-of HEX_DIGITS (char-upcase c)) 16))
;; Convert a single 8-bit unsigned integer in the range of 0-255.
(define (u8->base16 n) (floor (/ n 16)))

;; Convert a single-digit, base-16 character to an unsigned decimal integer.
(define (base16->10 c) (index-of HEX_DIGITS (char-upcase c)))

;; Convert a single base-10 integer in the range of 0-255
(define (base10->16 n) (list-ref HEX_DIGITS n))

(define (hexpair->u8 pair #:acc [acc 0])
  (if (not (pair? pair))
      acc
      (let* ([first (car pair)]
             [second (car (cdr pair))]
             [sum 
              (+ (base16->u8 first)
                 (base16->10 second))])
        (hexpair->u8 (cdr (cdr pair))
                      #:acc sum))))


;; Pad one or more zeroes to the remaining digits of the hex string.
(define (pad-zero hexstr #:bits [bits 512])
  (let* ([digits (inexact->exact (* 2 (log bits 8)))]
         [diff (- digits (string-length hexstr))])
    (if (< diff 0)
        hexstr
        (string-append hexstr (make-string diff #\0)))))

;; Check if the hex string has consecutive repeated byte char.
;; Unused for now.
(define (repeated-bytes? hexstr)
  (let* ([charlist (string->list hexstr)]
         [len (length charlist)]
         [first (car charlist)])
    (if (= len 0)
        false
        (let ([chars (filter (lambda (c) (char=? c first)) charlist)])
          (= (length chars) len)))))


;; Convert hex string to 64-bit RGB list.
;; It tries to do the right thing by padding zeroes
;; where the chars are missing.
(define (hex->rgb str)
  (let ([chars (string->list (pad-zero str))])
    (letrec ([aux (lambda (chars acc)
                    (if (null? chars)
                        acc
                        (aux (cddr chars)
                             (cons (hexpair->u8
                                    (list (car chars)
                                          (cadr chars)))
                                   acc))))])
      (reverse (aux chars '())))))