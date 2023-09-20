; Godelization in the Mu language
; Nils M Holm, 2022
; Public Domain / 0BSD

g -> exp(2,5) *
     exp(3,1) *
     exp(5,2) *
     exp(7,3) *
     exp(11,4) *
     exp(13,5).

lh(x) -> lo(2,x).

ent(x,y) -> lo(pi(y'),x).

fst(x) -> lo(3,x).

lst(x) -> lo(pi(lh(x)),x).

ext(x,y) -> 2*x * exp(pi(lh(x)'),y).

conc(x,y,0) -> x
  | (x,y,i') -> ext(conc(x,y,i),ent(y,i))
  | (x,y) -> conc(x,y,lh(y)).

rpl(x,i,y) -> x / exp(pi(i'),ent(x,i)) * exp(pi(i'),y).

trunc(x,n,i') -> [n=i -> x / exp(2,lh(x)-n') :
                  trunc(rpl(x,i,0),n,i)]
   | (x,n) -> [lh(x) < n -> x :
               n=0 -> 1 :
               trunc(x,pred(n),lh(x))].

subst(x,y,z,0) -> x
   | (x,y,z,i') -> [ent(x,i)=y -> subst(rpl(x,i,z),y,z,i) :
                    subst(x,y,z,i)]
   | (x,y,z) -> subst(x,y,z,lh(x)).

hist(f,x,0) -> 1
  | (f,x,y') -> exp(pi(y), f(x,y)+1) * hist(f,x,y).

dh(x,y,h) -> foreach(x,y,fun(i) -> lo(pi(i),h)-1).

J(x,y) -> exp(2,x) * exp(3,y).
K(x) -> lo(2,x).
L(x) -> lo(3,x).

