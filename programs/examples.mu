; MU language examples
; Nils M Holm, 2022
; Public domain / 0BSD license

;lexical(x) ->
;  (fun(x) ->
;    (fun(f) ->
;      (fun(x) -> f(2))(0))
;     (fun(y) -> x))
;   (1).

zero(x) -> 0.

id(x) -> x.

succ(x) -> x'.

pred(x') -> x.

sum(x,0) -> x
 | (x,y') -> sum(x,y)'.

diff(x,0) -> x
  | (x,y') -> pred(diff(x,y)).

diff(x,0) -> x
  | (x',y') -> diff(x,y).

prod(x,0) -> 0
  | (x,y') -> x + prod(x,y).

not(x) -> 1-x.

not(0) -> 1 | (x) -> 0.

sg(x) -> 1-(1-x).

sg(0) -> 0 | (x) -> 1.

lt(x,y) -> sg(y-x).

eq(x,y) -> not(x-y+(y-x)).

eq(0,0) -> 1
| (0,x') -> 0
| (x',0) -> 0
| (x',y') -> eq(x,y).

lteq(x,y) -> not(y<x).

and(x,y) -> sq(x*y).

or(x,y) -> sq(x+y).

quot(x,y,n,z,0) -> n
  | (x,y,n,z,k') -> q(x,y,(1-(x<z+y))+n,z+y,k)
  | (x,y) -> q(x,y,0,0,x).

quot(x,0) -> 0
  | (x y) -> [x<y -> 0 : quot(x-y,y)'].

rem(x,0) -> x
 | (x,y) -> [x<y -> x : rem(x-y,y)].

exp(x,0) -> 1
 | (x,y') -> x * exp(x,y).

mu(x,f) -> [f(x)=0 -> x : mu(x',f)]
| (x,w,f) -> [w<x' -> w :
              f(x)=0 -> x :
              mu(x'w,f)]
| (f) -> mu(0,f).

min(w,f) -> mu(0,w,fun(x) -> not(f(x))).

max(x,w,z,f) -> [x=w -> z :
                 f(x)=0 -> max(x',w,z,f) :
                 max(x',w,x,f)]
 | (w,f) -> max(0,w,0,f).

sqrt(x) -> max(x,fun(y) -> not(x<y*y)).

sqrt(x) -> pred(min(x,fun(y) -> x<y*y)).

nrt(x,y) -> pred(min(y,fun(z) -> y<exp(z,x))).

lo(x,y) -> max(y,fun(z) -> not(y mod exp(x,z))).

lo(x,y) -> pred(mu(fun(z) -> not(y mod exp(x,z)))).

lg(x,y) -> max(y,fun(z) -> not(y < exp(x,z))).

lg(x,y) -> pred(mu(fun(z) -> not(y < exp(x,z)))).

lg(x,y,z) -> [y < exp(x,z) -> 0 :
              lg(x,y,z')']
| (x,y) -> pred(lg(x,y,0)).

lg(x,y,z) -> [y<z -> 0 :
              lg(x,y,x*z)']
| (x,y) -> pred(lg(x,y,1)).

lg(x,y,z,n) -> [y<z -> n :
                lg(x,y,x*z,n')]
| (x,y) -> [x<2 -> 0:
            y=0 -> 0:
            pred(lg(x,y,1,0))].

hyper(0,x,y) -> y'
   | (1,x,0) -> x
   | (2,x,0) -> 0
   | (n,x,0) -> 1
   | (n',x,y') -> hyper(n, x, hyper(n',x,y)).

hyper(0,x,y) -> y'
   | (1,x,y) -> x+y
   | (2,x,y) -> x*y
   | (n,x,0) -> 1
   | (n',x,y') -> hyper(n, x, hyper(n',x,y)).

ack(0,y) -> y'
 | (x',0) -> ack(x,1)
 | (x',y') -> ack(x,ack(x',y)).

fac(0) -> 1
 | (x') -> x' * fac(x).

fib(x,y,0) -> x
 | (x,y,z') -> fib(x+y,x,z)
 | (x') -> fib(1,1,x).

gcd(x,0) -> 0
 | (x,y) -> [x<y -> gcd(y,x) :
             not(x mod y) -> y :
             gcd(x, x mod y)].

lcm(x,y) -> x*y / gcd(x,y).

phi(r,x,0) -> r(x,0)
 | (r,x,y') -> [r(x,y') -> phi(r,x,y)' :
                phi(r,x,y)]
 | (r,x) -> phi(r,x,pred(x)).

prime(x,0) -> 0
   | (x,2) -> 1
   | (x,y') -> [x mod y = 0 -> 0 :
                prime(x,y)]
   | (x) -> prime(x,x).

prime(x) -> (fun(w) ->
              w = mu(2,w,fun(z) -> x mod z))
             (sqrt(x)').

pi(x,y') -> [prime(x) -> [y-> pi(x'',y) : x] :
             pi(x'',y')]
| (0) -> 2
| (x) -> pi(3,x).

foreach(x,y,z,f) -> [x=y -> y :
                    foreach(x',y,print(x,f(x)),f)]
     | (x,y,f) -> foreach(x,y,0,f).

%%%%%%%%%%

; MU language examples in LISP notation
; Nils M Holm, 2022
; Public domain / 0BSD license

; Delete all up to (and including) the %%%'s if you prefer LISP notation.

lisp(1).

;(def lexical
;  (fun (x)
;    ((fun (x)
;      ((fun (f)
;        ((fun (x)
;          (f 2))
;         0))
;       (fun (y) x)))
;     1)))

(def zero (fun (x) 0))

(def id (fun (x) x))

(def succ (fun (x) x'))

(def pred (fun (x') x))

(def sum
  (fun ((x 0) x)
       ((x y') (sum x y)')))

(def diff
  (fun ((x 0) x)
       ((x y') (pred (diff x y)))))

(def diff
  (fun ((x 0) x)
       ((x' y') (diff x y))))

(def prod
  (fun ((x 0) 0)
       ((x y') (+ x (prod x y)))))

(def not (fun (x) (- 1 x)))

(def not (fun ((0) 1) ((x) 0)))

(def sg (fun (x) (- 1 (- 1 x))))

(def sg (fun ((0) 0) ((x) 1)))

(def lt (fun (x y) (sg (diff y x))))

(def eq (fun (x y) (not (+ (diff x y) (diff y x)))))

(def eq (fun ((0 0) 1)
             ((0 x) 0)
             ((x 0) 0)
             ((x' y') (eq x y))))

(def lteq (fun (x y) (not (lt (y z)))))

(def and *)

(def or (fun (x y) (sg (+ x y))))

(def quot
  (fun ((x 0) 0)
       ((x y) (if ((< x y) 0)
                  (1 (quot (- x y) y)')))))

(def rem
  (fun ((x 0) x)
       ((x y) (if ((< x y) x)
                  (1 (rem (- x y) y))))))

(def exp
  (fun ((x 0) 1)
       ((x y') (* x (exp x y)))))

(def mu
  (fun ((f x) (if ((= 0 (f x)) x)
                  (1 (mu f x'))))
       ((f x w) (if ((< w x') w)
                    ((= 0 (f x)) x)
                    (1 (mu f x' w))))
       ((f) (mu f 0))))

(def min
  (fun (f w) (mu (fun (x) (not (f x))) 0 w)))

(def max
  (fun ((f x w z) (if ((= x w) z)
                      ((= 0 (f x)) (max f x' w z))
                      (1 (max f x' w x))))
       ((f w) (max f 0 w 0))))

(def sqrt
  (fun (x)
    (max (fun (y) (not (< x (* y y)))) x)))

(def sqrt
  (fun (x)
    (pred (min (fun (y) (< x (* y y))) x))))

(def nrt
  (fun (x y)
    (pred (min (fun (z) (< y (exp z x))) y))))

(def lo
  (fun (x y)
    (max (fun (z) (not (mod y (exp x z)))) y)))

(def lo
  (fun (x y)
    (pred (mu (fun (z) (not (mod y (exp x z)))) 0))))

(def lg
  (fun (x y)
    (max (fun (z) (not (< y (exp x z)))) y)))

(def lg
  (fun (x y)
    (pred (mu (fun (z) (not (< y (exp x z)))) 0))))

(def lg
  (fun ((x y) (pred (lg x 0 y)))
       ((x z y) (if ((< y (exp x z)) 0)
                    (1 (lg x z' y)')))))

(def lg
  (fun ((x y) (pred (lg x 1 y)))
       ((x z y) (if ((< y z) 0)
                    (1 (lg x (* x z) y)')))))

(def lg
  (fun ((x y) (if ((< x 2) 0)
                  ((= y 0) 0)
                  (1 (pred (lg x 1 y 0)))))
       ((x z y n) (if ((< y z) n)
                      (1 (lg x (* x z) y n'))))))

(def hyper
  (fun ((0  x y)  y')
       ((1  x 0)  x)
       ((2  x 0)  0)
       ((n  x 0)  1)
       ((n' x y') (hyper n x (hyper n' x y)))))

(def hyper
  (fun ((0  x y)  y')
       ((1  x y)  (+ x y))
       ((2  x y)  (* x y))
       ((n  x 0)  1)
       ((n' x y') (hyper n x (hyper n' x y)))))

(def ack
  (fun ((0 y)   y')
       ((x' 0)  (ack x 1))
       ((x' y') (ack x (ack x' y)))))

(def fac
  (fun ((0) 1)
       ((x') (* x' (fac x)))))

(def fib
  (fun ((x y 0) x)
       ((x y z') (fib (+ x y) x z))
       ((z') (fib 1 1 z))))

(def gcd
  (fun ((x 0) 0)
       ((x y) (if ((< x y) (gcd y x))
                  ((not (mod x y)) y)
                  (1 (gcd x (mod x y)))))))

(def lcm (fun (x y) (div (* x y) (gcd x y))))

(def phi
  (fun ((r x) (phi r x (pred x)))
       ((r x 0) (r x 0))
       ((r x y') (if ((r x y') (phi r x y)')
                     (1 (phi r x y))))))

(def prime
  (fun ((x 0) 0)
       ((x 2) 1)
       ((x y') (if ((= 0 (mod x y)) 0)
                   (1 (prime x y))))
       ((x) (prime x x))))

(def prime
  (fun (x)
    ((fun (w)
       (= w (mu (fun (z) (mod x z)) 2 w)))
     (sqrt x)')))

(def pi
  (fun ((x y') (if ((prime x) (if (y (pi x'' y))
                                  (1 x)))
                   (1 (pi x'' y'))))
       ((0) 2)
       ((x) (pi 3 x))))

(def foreach
  (fun ((x y f) (foreach x y 0 f))
       ((x y z f) (if ((= x y) y)
                      (1 (foreach x' y (print x (f x)) f))))))

; (succ (lg 10 (fac 100)))
