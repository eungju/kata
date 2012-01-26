#! /usr/bin/env racket
#lang scheme

(require srfi/13)

(define (fit? w h lines size)
  (and (<= (* (length lines) size) h)
       (<= (* (apply max (map string-length lines)) size) w)))

(define (word-wrap s limit)
  (let ((words (regexp-split #rx" " s)))
    (word-wrap- (cdr words) limit (car words) '())))

(define (word-wrap- words limit line lines)
  (if (empty? words)
      (reverse (cons line lines))
      (let ((appended (string-append line " " (car words))))
        (if (<= (string-length appended) limit)
            (word-wrap- (cdr words) limit appended lines)
            (word-wrap- (cdr words) limit (car words) (cons line lines))))))

(define (solve w h s)
  (let loop ((guess h))
    (cond
      ((= guess 0) guess)
      ((fit? w h (word-wrap s (quotient w guess)) guess) guess)
      (true (loop (- guess 1))))))

(define (main)
  (let loop ((i 1) (t (string->number (read-line))))
    (if (> i t)
        (void)
        (begin
          (let* ((tokens (string-tokenize (read-line)))
                 (w (string->number (car tokens)))
                 (h (string->number (cadr tokens)))
                 (s (string-join (cddr tokens) " ")))
            (printf "Case #~a: ~a~n" i (solve w h s)))
          (loop (+ i 1) t)))))

(main)