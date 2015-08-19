#lang racket

(require redex/reduction-semantics)

(define-language iswim-lang
  [M X
     C
     (λ X M)
     (M M)
     (O1 M)
     (O2 M M)]
  [X variable-not-otherwise-mentioned]
  [C number boolean]
  [O O1 O2]
  [O1 zero?]
  [O2 + - * /])