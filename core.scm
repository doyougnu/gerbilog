;;; -*- Gerbil -*-

;;;;;;;;;;;;;;;;;;;;;;; Internal Primitives ;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax lambdaf
  (syntax-rules ()
    ((_ () e) (lambda () e))))

(defsyntax all-aux
  (syntax-rules ()
    ((_ bnd) succeed)
    ((_ bnd g)  g)
    ((_ bnd g0 g ...)
     (let ((g-hat g0))
       (lambda (s)
         (displayln "all-aux" s)
         (bnd (g-hat s)
              (lambda (s) ((all-aux bnd g ...) s))))))))

(defsyntax cond-aux
  (syntax-rules ()
    ((_ ifer) fail)
    ((_ ifer (else g ...)) (all g ...))
    ((_ ifer (g ...)) (all g ...))
    ((_ ifer (g0 g ...) c ...)
     (ifer g0
           (all g ...)
           (cond-aux ifer c ...)))))

(defsyntax case-inf
  (syntax-rules ()
    ((_ e on-zero ((a-hat) on-one) ((a f) on-choice))
     (let ((a-inf e))
       (cond
        ((not a-inf) on-zero)
        ((not (and
                (pair? a-inf)
                (procedure? (cdr a-inf))))
         (let ((a-hat a-inf))
           on-one))
        (else
         (let ((a (car a-inf)) (f (cdr a-inf)))
           on-choice)))))))

(def (map-inf n p a-inf)
  (case-inf a-inf
            '()
            ((a)
             (cons (p a) '()))
            ((a f)
             (cons (p a)
                   (cond
                    ((not n) (map-inf n p (f)))
                    ((> n 1) (map-inf (- n 1) p (f)))
                    (else '()))))))

(defsyntax if-e
  (syntax-rules ()
    ((_ g0 g1 g2)
     (lambda (s)
       (mplus ((all g0 g1) s) (lambdaf () (g2 s)))))))

(defsyntax if-i
  (syntax-rules ()
    ((_ g0 g1 g2)
     (lambda (s)
       (mplus-i ((all g0 g1) s) (lambdaf () (g2 s)))))))

(defsyntax if-a
  (syntax-rules ()
    ((_ g0 g1 g2)
     (lambda (s)
       (let ((s-inf (g0 s)))
         (case-inf s-inf
                   (g2 s)
                   ((s) (g1 s))
                   ((s f) (bind s-inf g1))))))))

(defsyntax if-u
  (syntax-rules ()
    ((_ g0 g1 g2)
     (lambda (s)
       (let ((s-inf (g0 s)))
         (case-inf s-inf
                   (g2 s)
                   ((s) (g1 s))
                   ((s f) (g1 s))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;; Api ;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-syntax mzero
  (syntax-rules ()
    ((_) #f)))

(defsyntax unit
  (syntax-rules ()
    ((_ a) a)))

(defsyntax choice
  (syntax-rules ()
    ((_ a f) (cons a f))))

(def (mplus a-inf f)
  (case-inf a-inf
            (f)
            ((a) (choice a f))
            ((a f0) (choice a
                            (lambdaf () (mplus (f0) f))))))

(def (mplus-i a-inf f)
  (case-inf a-inf
            (f)
            ((a) (choice a f))
            ((a f0) (choice a
                            (lambdaf () (mplus-i (f) f0))))))

(def (bind a-inf g)
  ;; (displayln "bind" a-inf g)
  (case-inf a-inf
            (mzero)
            ((a) (g a))
            ((a f) (mplus (g a)
                          (lambdaf () (bind (f) g))))))

(def (bind-i a-inf g)
  (case-inf a-inf
            (mzero)
            ((a) (g a))
            ((a f) (mplus-i (g a)
                            (lambdaf () (bind-i (f) g))))))

(defsyntax all
  (syntax-rules ()
    ((_ g ...) (begin
                 (displayln "in-all: " g ...)
                 (all-aux bind g ...)))))

(defsyntax all-i
  (syntax-rules ()
    ((_ g ...) (all-aux bind-i g ...))))

(defsyntax fresh
  (syntax-rules ()
    ((_ (x ...) g ...)
     (lambda (s)
       (let ((x (var x)) ...)
         ((all g ...) s))))))

(defsyntax cond-e
  (syntax-rules ()
    ((_ c ...) (cond-aux if-e c ...))))

(defsyntax cond-a
  (syntax-rules ()
    ((_ c ...) (cond-aux if-a c ...))))

(defsyntax cond-u
  (syntax-rules ()
    ((_ c ...) (cond-aux if-u c ...))))

(defsyntax cond-i
  (syntax-rules ()
    ((_ c ...) (cond-aux if-i c ...))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def (ext-s x v s)
  `((,x . ,v) . ,s))


(def (var v) (vector 'v))

(def (var? v) (vector? v))

(def (succeed s) (lambda (s) (unit s)))

(def (fail s) (lambda (s) (mzero)))

(def (walk v s)
  (displayln "walking: " v "  " s)
  (cond
   ((var? v)
    (cond
     ((assq v s) =>
        (lambda (a) (walk (cdr a) s)))
     (else v)))
   (else v)))

(def (walk* v s)
  (displayln "walking*: " v "  " s)
  (let ((v (walk v s)))
    (cond
     ((var? v) v)
     ((pair? v)
      (cons
       (walk* (car v) s)
       (walk* (cdr v) s)))
     (else v))))

(def (unify v w s)
  (displayln "unifying: " v "  " w " in " s)
  (let ((v (walk v s))
        (w (walk w s)))
    (cond
     ((eq? v w) s)
     ((var? v) (ext-s v w s))
     ((var? w) (ext-s w v s))
     ((and (pair? v) (pair? w))
      (cond
       ((unify (car v) (car w) s) =>
        (lambda (s)
          (unify (cdr v) (cdr w) s)))
       (else #f)))
     ((equal? v w) s)
     (else #f))))

(def (reify-name n)
  (string->symbol
   (string-append "_" "." (number->string n))))

(def (size-s s)
  (length s))

(def empty-s '())

(def (reify-s v s)
  (let ((v (walk v s)))
    (cond
     ((var? v) (ext-s v (reify-name (size-s s)) s))
     ((pair? v) (reify-s (cdr v)
                         (reify-s (car v) s))))))

(def (reify v)
  ;; (displayln "reify: " v)
  (walk* v (reify-s v empty-s)))

(def (== v w)
  (lambda (s)
      (cond
       ((unify v w s) => succeed)
       (else (fail s)))))

(defsyntax run
  (syntax-rules ()
    ((_ n-hat (x) g ...)
     (let ((n n-hat)
           (x (var x)))
       (if (or (not n) (> n 0))
         (map-inf n
                  (lambda (s)
                    (reify (walk* x s)))
                  ((all g ...) empty-s))
         '())))))

(defsyntax run*
  (syntax-rules ()
    ((_ (q0 q ...) g0 g ...) (run #f (q0 q ...) g0 g ...))))

;; (def test
;;   (run* (a)
;;         (== #t a)))

;; (def teacup
;;   (lambda (w)
;;     (cond-e
;;      ((== 'tea w) succeed)
;;      ((== 'cup w) succeed)
;;      (succeed fail))))
