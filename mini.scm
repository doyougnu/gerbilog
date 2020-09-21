;;; -*- Gerbil -*-
(import :std/srfi/1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;; Primitives ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def (var v) (vector v))
(def (var? v) (vector? v))
(def (var?= x1 x2) (= (vector-ref 0 x1) (vector-ref 0 x2)))

(def (walk v s)
  (cond
   ((var? v)
    (cond
     ((assq v s) =>
      (lambda (a) (walk (cdr a) s)))
     (else v)))
   (else v)))

(define (ext-s x v s) `((,x . ,v) . ,s))

(define (== u v)
  (lambda (s/c)
    (let ((s (unify u v (car s/c))))
      (if s (unit `(,s . ,(cdr s/c))) mzero))))

(define (unit s/c) (cons s/c mzero))
(define mzero '())

(define (unify u v s)
  (let ((u (walk u s)) (v (walk v s)))
    (cond
     ((and (var? u) (var? v) (var=? u v)) s)
     ((var? u) (ext-s u v s))
     ((var? v) (ext-s v u s))
     ((and (pair? u) (pair? v))
      (let ((s (unify (car u) (car v) s)))
        (and s (unify (cdr u) (cdr v) s))))
     (else (and (eqv? u v) s)))))

(define (call/fresh f)
  (lambda (s/c)
    (let ((c (cdr s/c)))
      ((f (var c)) `(,(car s/c) . ,(+ c 1))))))

(define (disj g1 g2) (lambda (s/c) (mplus (g1 s/c) (g2 s/c))))
(define (conj g1 g2) (lambda (s/c) (bind (g1 s/c) g2)))

(define (mplus $1 $2)
  (cond
   ((null? $1) $2)
   ((procedure? $1) (lambda () (mplus $2 ($1))))
   (else (cons (car $1) (mplus (cdr $1) $2)))))

(define (bind $ g)
  (cond
   ((null? $) mzero)
   ((procedure? $) (lambda () (bind ($) g)))
   (else (mplus (g (car $)) (bind (cdr $) g)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;; Api ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define empty-state '(() . 0))
(def (call/empty-state g) (g empty-state))

(define-syntax Zzz
  (syntax-rules ()
    ((_ g) (lambda (s/c) (lambda () (g s/c))))))

(define-syntax conj+
  (syntax-rules ()
    ((_ g) (Zzz g))
    ((_ g0 g ...) (conj (Zzz g0) (conj+ g ...)))))

(define-syntax disj+
  (syntax-rules ()
    ((_ g) (Zzz g))
    ((_ g0 g ...) (disj (Zzz g0) (disj+ g ...)))))

(define-syntax conde
  (syntax-rules ()
    ((_ (g0 g ...) ...) (disj+ (conj+ g0 g ...) ...))))

(define-syntax fresh
  (syntax-rules ()
    ((_ () g0 g ...) (conj+ g0 g ...))
    ((_ (x0 x ...) g0 g ...)
     (call/fresh (lambda (x0) (fresh (x ...) g0 g ...))))))

(def (pull $) (if (procedure? $) (pull ($)) $))

(def (take-all $)
  (let (($ (pull $)))
    (if (null? $)
      '()
      (cons (car $) (take-all (cdr $))))))

(def (take n $)
  (if (zero? n) '()
      (cond
       ((null? $) '())
       (else (cons (car $) (take (- n 1) (cdr $)))))))

(def (mk-reify s/c*)
  (map (reify-state/1st-var s/c*)))

(def (reify-state/1st-var s/c)
  (let ((v (walk* (var 0) (car s/c))))
    (walk* v (reify-s v '()))))

(def (reify-s v s)
  (let ((v (walk v s)))
    (cond
     ((var? v)
      (let ((n (reify-name (length s))))
        (cons `(,v . ,n) s)))
     ((pair? v) (reify-s (cdr v) (reify-s (car v) s)))
     (else s))))

(def (reify-name n)
  (string->symbol
   (string-append "_" "." (number->string n))))

(def (walk* v s)
  (let ((v (walk v s)))
    (cond
     ((var? v) v)
     ((pair? v) (cons (walk* (car v) s)
                      (walk* (cdr v) s)))
     (else v))))

(define-syntax run
  (syntax-rules ()
    ((_ n (x ...) g0 g ...)
     (mk-reify (take n (call/empty-state
                        (fresh (x ...) g0 g ...)))))))

(define-syntax run*
  (syntax-rules ()
    ((_ (x ...) g0 g ...)
     (mk-reify (take-all (call/empty-state
                          (fresh (x ...) g0 g ...)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; tests ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def a-and-b
  (conj
   (call/fresh (lambda (a) (== a 7)))
   (call/fresh (lambda (b) (disj (== b 5) (== b 6))))))