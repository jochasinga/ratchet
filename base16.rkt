#lang racket

(provide (contract-out
          [base16->10 (-> char? (and/c exact-nonnegative-integer? is-8-bit-int?))]
          [base10->16 (-> (and/c exact-nonnegative-integer? is-8-bit-int?) char?)]
          [is-8-bit-int? (-> exact-nonnegative-integer? bool)]))

(define HEX_DIGITS
  '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\A #\B #\C #\D #\E #\F))

;; Check if an integer is within an 8-bit range.
(define (is-8-bit-int? n) (and (> n 0) (< n 256)))

;; Convert a single-digit, base-16 character to an unsigned decimal integer.
(define (base16->10 c) (* (index-of HEX_DIGITS (char-upcase c)) 16))

;; Convert a single base-10 integer in the range of 0-255
(define (base10->16 n) (floor (/ n 16)))