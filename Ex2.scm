; David Lance
; Programming Exercise 2

; dotproduct-cps
(define dotproduct-cps
  (lambda (v1 v2 return)
    (cond
      ((null? v1) (return 0))
      ((null? v2) (return 0))
      (else (dotproduct-cps (cdr v1) (cdr v2) (lambda (v) (return (+ (* (car v1) (car v2)) v))))))))

; removesubsequence-cps
(define removesubsequence-cps
  (lambda (sub ls return)
    (cond
      ((null? sub) (return ls))
      ((null? ls) (return ()))
      ((eq? (car sub) (car ls)) (removesubsequence-cps (cdr sub) (cdr ls) return))
      (else (removesubsequence-cps sub (cdr ls) (lambda (v) (return (cons (car ls) v))))))))

; squareroot-cps
(define squareroot-cps
  (lambda (value i return)
    (cond
      ((= i 0) (return value))
      (else (squareroot-cps value (- i 1) (lambda (v)
                                        (return (- v
                                                   (/ (- (* v v) value) (* 2 v))))))))))

; replaceall*-cps
(define replaceall*-cps
  (lambda (e1 e2 ls return)
    (cond
      ((null? ls) (return ()))
      ((list? (car ls)) (replaceall*-cps e1 e2 (car ls)
                                         (lambda (v1) (replaceall*-cps e1 e2 (cdr ls)
                                                                       (lambda (v2) (return (cons v1 v2)))))))
      ((eq? (car ls) e1) (replaceall*-cps e1 e2 (cdr ls)
                                          (lambda (v) (return (cons e2 v)))))
      (else (replaceall*-cps e1 e2 (cdr ls)
                             (lambda (v) (return (cons (car ls) v))))))))

; reverse*-cps
(define reverse*-cps
  (lambda (ls return)
    (cond
      ((null? ls) (return ()))
      ((list? (car ls)) (reverse*-cps (car ls)
                                      (lambda (v1) (reverse*-cps (cdr ls)
                                                                 (lambda (v2) (return (append v2 (cons v1 ()))))))))
      (else (reverse*-cps (cdr ls) (lambda (v) (return (append v (cons (car ls) ())))))))))

; vectormult-cps
(define vectormult-cps
  (lambda (vec m return)
    (cond
      ((null? vec) (return ()))
      ((null? m) (return ()))
      ((null? (car m)) (return ()))
      (else (vectormult-cps vec (cdr-column-cps m (lambda (v) v))
                            (lambda (v1) (return (cons
                                                  (dotproduct-cps vec (car-column-cps m (lambda (v2) v2))
                                                                  (lambda (v3) v3))
                                                  v1 ))))))))

; helper
; car-column-cps
(define car-column-cps
  (lambda (ls return)
    (cond
      ((null? ls) (return ()))
      (else (car-column-cps (cdr ls) (lambda (v) (return (cons (caar ls) v))))))))
; helper
; cdr-column-cps
(define cdr-column-cps
  (lambda (ls return)
    (cond
      ((null? ls) (return ()))
      (else (cdr-column-cps (cdr ls) (lambda (v) (return (cons (cdar ls) v))))))))

; matrixmultiply-cps
(define matrixmultiply-cps
  (lambda (m1 m2 return)
    (cond
      ((null? m1) (return ()))
      ((null? m2) (return ()))
      (else (matrixmultiply-cps (cdr m1) m2 (lambda (v)
                                              (return (cons
                                                       (vectormult-cps (car m1) m2 (lambda (v) v))
                                                       v))))))))

; removesubsequence*-cps
(define removesubsequence*-cps 
  (lambda (sub ls return)
    (cond
      ((null? sub) (return sub ls))
      ((null? ls) (return sub ls))
      ((list? (car ls)) (removesubsequence*-cps sub (car ls)
                                                (lambda (vsub1 v1)
                                                  (removesubsequence*-cps vsub1 (cdr ls) (lambda (vsub2 v2) (return vsub1 (cons v1 v2)))))))
      ((eq? (car ls) (car sub)) (removesubsequence*-cps (cdr sub) (cdr ls)
                                                        (lambda (vsub v) (return (cdr sub) v))))
      (else (removesubsequence*-cps sub (cdr ls) (lambda (vsub v)
                                                   (return sub (cons (car ls) v)))))))) 

; suffix with letrec
(define suffix
  (lambda (e ls)
    (letrec ((suffix+ (lambda (ls return)
                       (cond
                         ((null? ls) return)
                         ((eq? e (car ls)) (suffix+ (cdr ls) ()))
                         (else (suffix+ (cdr ls) (append return (cons (car ls) ()))))))))
      (suffix+ ls ()))))

; suffix2 with call/cc
(define suffix2
  (lambda (e ls)
    (call/cc
     (lambda (continuation)
       (letrec ((go (lambda (l return)
                        (cond
                          ((null? l) (return ()))
                          ((eq? e (car l)) (continuation (go (cdr l) (lambda (v) v))))
                          (else (go (cdr l) (lambda (v) (return (cons (car l) v)))))))))
                (go ls (lambda (v) v)))))))

  
                          
                          