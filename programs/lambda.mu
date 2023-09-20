; MU language lambda calculus examples
; Nils M Holm, 2022
; Public domain / 0BSD license
; Mostly from "Compiling Lambda Calculus", Nils M Holm, 2016

lisp(1).

(def T (fun (x) (fun (y) x)))
(def F (fun (x) (fun (y) y)))

(def conj (fun (x) (fun (y) ((x y) F))))
(def disj (fun (x) (fun (y) ((x T) y))))
(def neg  (fun (x) ((x F) T)))

(def true  (fun (x) ((x 1) 0)))
(def false (fun (x) ((x 0) 1)))

(def pair   (fun (x) (fun (y) (fun (f) ((f x) y)))))
(def first  (fun (p) (p T)))
(def second (fun (p) (p F)))

(def nil  ((pair T) T))
(def null first)

(def cons (fun (x) (fun (y) ((pair F) ((pair x) y)))))
(def car  (fun (x) (first (second x))))
(def cdr  (fun (x) (second (second x))))

(def zero (fun (f) (fun (x) x)))

; convert church numeral to integer
(def val (fun (n) ((n (fun (x) x')) 0)))

; \n\f\x.f(nfx)
(def S
  (fun (n)
    (fun (f)
      (fun (x)
        (f ((n f) x))))))

; \n\f\x.n(\g\h.h(gf))(\ux)(\uu)
(def P
  (fun (n)
    (fun (f)
      (fun (x)
        (((n (fun (g)
               (fun (h)
                 (h (g f)))))
          (fun (u) x))
         (fun (x) x))))))

; \m\n\f\x.mf(nfx) 
(def plus
  (fun (m)
    (fun (n)
      (fun (f)
        (fun (x)
          ((m f) ((n f) x)))))))

; \m\n.(n P)m
(def minus
  (fun (m)
    (fun (n)
      (fun (f)
        (fun (x)
          ((((n P) m) f) x))))))

; \m\n\f.m(nf)
(def times
  (fun (m)
    (fun (n)
      (fun (f)
        (fun (x)
          ((m (n f)) x))))))

; \m\n.nm
(def exp
  (fun (m)
    (fun (n)
      (fun (f)
        ((n m) f)))))

; \n.n(\x.F)T
(def zerop
  (fun (n)
    ((n (fun (x) F)) T)))

; \m\n.zerop(minus mn)
(def leqp
  (fun (m)
    (fun (n)
      (zerop ((minus m) n)))))

; \m\n.conj(leq mn)(leq nm)
(def eqp
  (fun (m)
    (fun (n)
      ((conj ((leqp m) n)) ((leqp n) m)))))

(def Y
  (fun (f)
    ((fun (x) (x x))
     (fun (x) (f (fun (y) ((x x) y)))))))

; \m\n.Y(\r\d.zerop d 0 (S (r (minus d n))))(minus (S m) n)
(def div
  (fun (m)
    (fun (n)
      ((Y (fun (r)
            (fun (d)
              ((((zerop d)
                 (fun (u) zero))
                (fun (u) (S (r ((minus d) n)))))
               (fun (u) u)))))
       ((minus (S m)) n)))))

; \p\x.Y(\n.eqp x (p n) n (f (succ n)))0
(def kp
  (fun (p)
    (fun (x)
      ((Y (fun (f)
            (fun (n)
              (((((eqp x) (p n))
                  (fun (u) n))
                 (fun (u) (f (S n))))
                (fun (u) u)))))
       zero))))

; \f\m\n.(kp \xfxn)m
(def inv
  (fun (f)
    (fun (m)
      (fun (n)
        ((kp (fun (x) ((f x) n))) m)))))

(def quot (inv times))
(def diff (inv plus))
(def log  (inv exp))

(lisp 0)
