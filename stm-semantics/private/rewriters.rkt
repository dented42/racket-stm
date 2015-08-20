#lang racket/base

(provide current-rewriter-stack
         call-with-rewriter-stack
         with-rewriter-stack
         with-rewriters
         call-with-rewriters
         gen:redex-rewriter redex-rewriter? redex-rewriter/c
         call-with-rewriter
         (struct-out atomic-rewriter)
         (struct-out compound-rewriter))

(require racket/generic
         redex/pict)

(define current-rewriter-stack
  (make-parameter '()))

(define (call-with-rewriter-stack thunk)
  (let loop ([stack (current-rewriter-stack)])
    (if (null? stack)
        (thunk)
        (call-with-rewriter (car stack)
                            (λ () (loop (cdr stack)))))))

(define-syntax-rule (with-rewriter-stack body ...)
  (call-with-rewriter-stack (λ () body ...)))

(define-syntax-rule (with-rewriters [rewriter ...] body ...)
  (paramaterize ([current-rewriter-stack (list* rewriter ... (current-rewriter-stack))])
    (with-rewriter-stack body ...)))

(define (call-with-rewriters thunk . rewriters)
  (parameterize ([current-rewriter-stack (append rewriters (current-rewriter-stack))])
    (call-with-rewriter-stack thunk)))

(define-generics redex-rewriter
  (call-with-rewriter redex-rewriter thunk))

(struct atomic-rewriter (trigger replacement)
  #:methods gen:redex-rewriter
  [(define (call-with-rewriter rewriter thunk)
     (with-atomic-rewriter (atomic-rewriter-trigger rewriter)
                           (atomic-rewriter-replacement rewriter)
                           (thunk)))])

(struct compound-rewriter (trigger replacement)
  #:methods gen:redex-rewriter
  [(define (call-with-rewriter rewriter thunk)
     (with-atomic-rewriter (atomic-rewriter-trigger rewriter)
                           (atomic-rewriter-replacement rewriter)
                           (thunk)))])