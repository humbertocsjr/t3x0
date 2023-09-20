! MU language
! Nils M Holm, 2022
! public domain / 0BSD license

use t3x: t;

! Use    8,000 on DOS,
!        4,000 on CP/M,
!      100,000 on 32-bit Unix

const	NNODES	= 8000;

const	SYMLEN	= 64;
const	BUFLEN	= 128;
const	PRDEPTH	= 64;

const	SPCL	= NNODES;
const	NIL	= NNODES+1;
const	EOT	= NNODES+2;
const	RPAREN	= NNODES+3;
const	UNDEF	= NNODES+4;
const	ARROW	= NNODES+5;

strlen(s) return t.memscan(s, 0, 32767);

strequ(a, b) return t.memcomp(a, b, strlen(a)+1) = 0;

var Ntoab::20;

ntoa(n) do var q, p;
	p := @Ntoab::19;
	q := p;
	p::0 := 0;
	while (n \/ p = q) do
		p := p-1;
		p::0 := n mod 10 + '0';
		n := n ./ 10;
	end
	return p;
end

pr(s) t.write(T3X.SYSOUT, s, strlen(s));

nl() do var b::3;
	pr(t.newline(b));
end

wrch(c) do var s::1;
	s::0 := c;
	t.write(T3X.SYSOUT, s, 1);
end

var	Input, Inp, Ink;
var	Exhausted;
var	Inbuf, Buffer::BUFLEN;
var	Rejected;
var	Line;

flushinp() do
	Input := T3X.SYSIN;
	Inbuf := Buffer;
	Inp := 0;
	Ink := 0;
	Rejected := %1;
end

decl error(2);

var	Errflag;

rdch() do var c;
	if (Exhausted) return EOT;
	if (Errflag) return EOT;
	if (Rejected \= %1) do
		c := Rejected;
		Rejected := %1;
		return c;
	end
	if (Inp >= Ink) do
		Ink := t.read(Input, Inbuf, BUFLEN);
		if (Ink < 1) do
			Exhausted := 1;
			return EOT;
		end
		Inp := 0;
	end
	c := Inbuf::Inp;
	Inp := Inp+1;
	if (c = 4 \/ c = 26 \/ c = '%') do
		Exhausted := 1;
		return EOT;
	end
	if (Line > 0 /\ c = '\n') Line := Line+1;
	return c;
end

fini(x) ie (x) halt 1; else halt 0;

decl print(1);

error(m, n) do
	if (Errflag) return;
	pr("? ");
	if (Line) do
		pr(ntoa(Line));
		pr(": ");
	end
	pr(m);
	if (n \= UNDEF) do
		pr(": ");
		print(n);
	end
	nl();
	flushinp();
	Errflag := %1;
	return NIL;
end

fatal(m) do
	error(m, UNDEF);
	pr("? aborting");
	nl();
	fini(1);
end

var	S_def, S_if, S_fun, S_funstar;

var 	S_cn, S_count, S_diff, S_div, S_eq, S_it, S_lisp, S_lt, S_mod,
	S_one, S_prime, S_print, S_prod, S_rec, S_sum, S_tco, S_trace,
	S_zero;

var	Car[NNODES],
	Cdr[NNODES];

var	Tag::NNODES;

const	ATOMB	= 0x01;
const	MARKB	= 0x02;
const	SWAPB	= 0x04;
const	NUMBB	= 0x08;

var	Freelist;

atomp(n) return n >= SPCL \/ Car[n] < SPCL /\ Tag::Car[n] & ATOMB;

symbolp(n) return n < SPCL /\ Car[n] < SPCL /\ Tag::Car[n] & ATOMB;

functionp(n) return n < SPCL /\ Car[n] < SPCL /\ \Tag::Car[n] & ATOMB /\
			Car[n] = S_funstar;

numberp(n) return n < SPCL /\ Car[n] < SPCL /\ Tag::n & NUMBB;

! Deutsch/Schorr/Waite graph marker

mark(n) do var p, x;
	p := NIL;
	while (1) do
		ie (n >= SPCL \/ Tag::n & MARKB) do
			if (p = NIL) leave;
			ie (Tag::p & SWAPB) do
				x := Cdr[p];
				Cdr[p] := Car[p];
				Car[p] := n;
				Tag::p := Tag::p & ~SWAPB;
				n := x;
			end
			else do
				x := p;
				p := Cdr[x];
				Cdr[x] := n;
				n := x;
			end
		end
		else ie (Tag::n & ATOMB) do
			x := Cdr[n];
			Cdr[n] := p;
			p := n;
			n := x;
			Tag::p := Tag::p | MARKB;
		end
		else do
			x := Car[n];
			Car[n] := p;
			Tag::n := Tag::n | MARKB;
			p := n;
			n := x;
			Tag::p := Tag::p | SWAPB;
		end
	end
end

var	Symbols;
var	Acc, Env;
var	Stack, Mstack;
var	Tmpcar, Tmpcdr;
var	Znode;
var	Ntrace;
var	Ncount, Count;

var	Verbose_GC;

gc() do var i, k;
	if (Verbose_GC) pr("GC: ");
	mark(Znode);
	mark(S_zero);
	mark(S_one);
	mark(Acc);
	mark(Env);
	mark(Symbols);
	mark(Stack);
	mark(Mstack);
	mark(Tmpcar);
	mark(Tmpcdr);
	mark(Ntrace);
	mark(Ncount);
	mark(Count);
	k := 0;
	Freelist := NIL;
	for (i=0, NNODES) do
		ie (Tag::i & MARKB) do
			Tag::i := Tag::i & ~MARKB;
		end
		else do
			Cdr[i] := Freelist;
			Freelist := i;
			k := k+1;
		end
	end
	if (Verbose_GC) do
		pr(ntoa(k));
		pr(" NODES");
		nl();
	end
	return k;
end

cons3(a, d, ta) do var n;
	if (Freelist = NIL) do
		Tmpcdr := d;
		if (ta \= ATOMB) Tmpcar := a;
		gc();
		Tmpcar := NIL;
		Tmpcdr := NIL;
		if (Freelist = NIL) do
			error("out of nodes", UNDEF);
			return Znode;
		end
	end
	n := Freelist;
	Freelist := Cdr[Freelist];
	Car[n] := a;
	Cdr[n] := d;
	Tag::n := ta;
	return n;
end

cons(a, d) return cons3(a, d, 0);

caar(x) return Car[Car[x]];
cadr(x) return Car[Cdr[x]];
cdar(x) return Cdr[Car[x]];
cddr(x) return Cdr[Cdr[x]];

caaar(x) return Car[Car[Car[x]]];
caadr(x) return Car[Car[Cdr[x]]];
cadar(x) return Car[Cdr[Car[x]]];
caddr(x) return Car[Cdr[Cdr[x]]];
cdadr(x) return Cdr[Car[Cdr[x]]];
cddar(x) return Cdr[Cdr[Car[x]]];

caddar(x) return Car[Cdr[Cdr[Car[x]]]];
cdddar(x) return Cdr[Cdr[Cdr[Car[x]]]];

save(n) Stack := cons(n, Stack);

unsave(k) do var n;
	while (k) do
		if (Stack = NIL) fatal("stack empty");
		n := Car[Stack];
		Stack := Cdr[Stack];
		k := k-1;
	end
	return n;
end

msave(n) Mstack := cons3(n, Mstack, ATOMB);

munsave() do var n;
	if (Mstack = NIL) fatal("mstack empty");
	n := Car[Mstack];
	Mstack := Cdr[Mstack];
	return n;
end

nrev(n) do var m, h;
	m := NIL;
	while (n \= NIL) do
		h := Cdr[n];
		Cdr[n] := m;
		m := n;
		n := h;
	end
	return m;
end

rev(n) do var m;
	save(n);
	m := NIL;
	save(m);
	while (n \= NIL) do
		m := cons3(Car[n], m, Tag::n);
		Car[Stack] := m;
		n := Cdr[n];
	end
	unsave(2);
	return m;
end

copy(n) return nrev(rev(n));

copynum(n) return cons3(copy(Car[n]), NIL, NUMBB);

length(x) do var i;
	i := 0;
	while (x \= NIL) do
		x := Cdr[x];
		i := i+1;
	end
	return i;
end

memq(x, a) do
	while (x \= NIL) do
		if (Car[x] = a) return %1;
		x := Cdr[x];
	end
	return 0;
end

strsym(s) do var i, n;
        i := 0;
        if (s::i = 0) return NIL;
	n := cons3(NIL, NIL, ATOMB);
        save(n);
        while (1) do
                Car[n] := s::i << 8 | s::(i+1);
                if (s::(i+1) = 0 \/ s::(i+2) = 0) leave;
                Cdr[n] := cons3(NIL, NIL, ATOMB);
                n := Cdr[n];
                i := i + 2;
        end
        n := unsave(1);
        return cons(n, UNDEF);
end

var Symb::SYMLEN+2;

symstr(n) do var i;
        i := 0;
        n := Car[n];
        while (n \= NIL) do
                Symb::i := Car[n] >> 8;
                Symb::(i+1) := Car[n] & 0xff;
                i := i + 2;
                n := Cdr[n];
        end
        Symb::i := 0;
        return Symb;
end

findsym(s) do var p;
	p := Symbols;
	while (p \= NIL) do
		if (strequ(s, symstr(Car[p])))
			return Car[p];
		p := Cdr[p];
	end
	return NIL;
end

addsym(s, v) do var n;
	n := findsym(s);
	if (n \= NIL) return n;
	n := strsym(s);
	Symbols := cons(n, Symbols);
	ie (v = SPCL)
		Cdr[n] := cons(n, NIL);
	else
		Cdr[n] := cons(v, NIL);
	return n;
end

lookup(x) do var a ,e;
	e := Env;
	while (e \= NIL) do
		a := Car[e];
		while (a \= NIL) do
			if (caar(a) = x) return cdar(a);
			a := Cdr[a];
		end
		e := Cdr[e];
	end
	return Cdr[x];
end

var	Parens;

decl lisp_read(0);

rdlist() do var n, lst, a;
	Parens := Parens+1;
	lst := cons(NIL, NIL);
	save(lst);
	a := NIL;
	while (1) do
		if (Errflag) return NIL;
		n := lisp_read();
		if (n = EOT) return error("missing ')'", UNDEF);
		if (n = RPAREN) leave;
		if (n = S_prime /\ a \= NIL) do
			Car[a] := cons(S_prime, cons(Car[a], NIL));
			loop;
		end
		ie (a = NIL) 
			a := lst;
		else
			a := Cdr[a];
		Car[a] := n;
		Cdr[a] := cons(NIL, NIL);
	end
	Parens := Parens-1;
	if (a \= NIL) Cdr[a] := NIL;
	unsave(1);
	return a = NIL-> NIL: lst;
end

numeric(c) return '0' <= c /\ c <= '9';

var	Lispmode;

symbolic(c) return c >= 'a' /\ c <= 'z' \/ c >= 'A' /\ c <= 'Z' \/
			Lispmode /\ t.memscan("+-*/<>=^", c, 8) \= %1;

rdsym(c) do var s::SYMLEN+1, i;
	i := 0;
	while (symbolic(c)) do
		ie (SYMLEN = i)
			error("long symbol", UNDEF);
		else if (i < SYMLEN) do
			s::i := c;
			i := i+1;
		end
		c := rdch();
	end
	s::i := 0;
	Rejected := c;
	return addsym(s, UNDEF);
end

normalize(n) do
	while (Car[n] = 0 /\ Cdr[n] \= NIL)
		n := Cdr[n];
	return n;
end

rdnum(c) do var n;
	n := NIL;
	save(n);
	while (numeric(c)) do
		n := cons3(c-'0', n, ATOMB);
		Car[Stack] := n;
		c := rdch();
	end
	Rejected := c;
	return cons3(normalize(rev(unsave(1))), NIL, NUMBB);
end

syntax(x) return error("syntax", x);

skip() do var c;
	c := rdch();
	while (1) do
		while (c = ' ' \/ c = '\t' \/ c = '\n' \/ c = '\r') do
			if (Errflag) return NIL;
			c := rdch();
		end
		if (c \= ';') leave;
		while (c \= '\n' /\ c \= '\r') c := rdch();
	end
	return c;
end

lisp_read() do var c;
	c := skip();
	if (Errflag) return NIL;
	if (c = EOT) return EOT;
	if (c = '(') do
		return rdlist();
	end
	if (c = ')') do
		if (\Parens) error("extra paren", UNDEF);
		return RPAREN;
	end
	if (c = ''') do
		return S_prime;
	end
	if (symbolic(c)) do
		return rdsym(c);
	end
	if (numeric(c)) do
		return rdnum(c);
	end
	syntax(UNDEF);
	return UNDEF;
end

print2(n, d) do
	if (d > PRDEPTH) error("print depth", UNDEF);
	if (Errflag) return error("stop", UNDEF);
	ie (n = NIL) do
		pr("()");
	end
	else ie (n = EOT) do
		pr("[eot]");
	end
	else ie (numberp(n)) do
		n := Car[n];
		while (n \= NIL) do
			wrch(Car[n]+'0');
			n := Cdr[n];
		end
	end
	else ie (n >= SPCL \/ Tag::n & ATOMB) do
		pr("[unprintable]");
	end
	else ie (symbolp(n)) do
		pr(symstr(n));
	end
	else do	! List
		if (\atomp(n) /\ Car[n] = S_funstar) do
			pr("fun:");
			print(caddr(n));
			return;
		end
		pr("(");
		while (n \= NIL) do
			print2(Car[n], d+1);
			n := Cdr[n];
			ie (symbolp(n)) do
				pr(" . ");
				print2(n, d+1);
				n := NIL;
			end
			else if (n \= NIL) do
				pr(" ");
			end
		end
		if (\Errflag) pr(")");
	end
end

print(n) print2(n, 0);

decl expr(0);

mksucc(x) return cons(S_prime, cons(x, NIL));

optsucc(n) do var c;
	c := skip();
	while (c = ''') do
		n := mksucc(n);
		c := skip();
	end
	Rejected := c;
	return n;
end

rdargs(x) do var c, n;
	x := cons(x, NIL);
	save(x);
	c := skip();
	while (c \= ')' /\ c \= EOT) do
		if (Errflag) return NIL;
		if (c = ',') do
			c := skip();
			loop;
		end
		ie (n = ''' /\ Cdr[x] \= NIL) do
			Car[x] := mksucc(Car[x]);
		end
		else do
			Rejected := c;
			n := expr();
			x := cons(n, x);
		end
		Car[Stack] := x;
		c := skip();
	end
	return nrev(unsave(1));
end

condexpr() do var n, m, q, c;
	n := expr();
	c := skip();
	q := NIL;
	save(q);
	while (c = ARROW) do
		save(n);
		m := expr();
		save(m);
		c := skip();
		if (c \= ':') do
			return syntax(UNDEF);
		end
		n := cons(n, cons(m, NIL));
		q := cons(n, q);
		Car[Stack] := q;
		unsave(2);
		n := expr();
		c := skip();
	end
	if (c \= ']') return syntax(UNDEF);
	n := cons(cons(S_one, cons(n, NIL)), q);
	return cons(S_if, nrev(n));
end

newfun(n) do var m, p, c;
	save(n);
	p := NIL;
	save(p);
	while (1) do
		n := Cdr[n];
		m := expr();
		p := cons(cons(n, cons(m, NIL)), p);
		Car[Stack] := p;
		c := skip();
		if (c \= '|') leave;
		Rejected := 'f';
		n := expr();
		Car[Cdr[Stack]] := n;
		c := skip();
		if (c \= ARROW) do
			unsave(2);
			return syntax(UNDEF);
		end
	end
	Rejected := c;
	p := nrev(p);
	Car[Stack] := p;
	if (Cdr[p] = NIL) p := Car[p];
	unsave(2);
	return cons(S_fun, p);
end

fundef(n) do
	if (atomp(n)) return cons(S_def, cons(n, cons(expr(), NIL)));
	return cons(S_def, cons(Car[n], cons(newfun(n), NIL)));
end

factor() do var c, n, fn;
	c := skip();
	if (c = EOT) return EOT;
	if (Errflag) return NIL;
	if (numeric(c)) do
		return optsucc(rdnum(c));
	end
	if (symbolic(c)) do
		n := rdsym(c);
		fn := n = S_fun;
		c := skip();
		ie (c = '(') do
			n := rdargs(n);
			if (fn) do
				if (skip() \= '-' \/ rdch() \= '>')
					return syntax(UNDEF);
				return newfun(n);
			end
		end
		else do
			Rejected := c;
		end
		return optsucc(n);
	end
	if (c = '(') do
		n := expr();
		c := skip();
		if (c \= ')') return error("missing ')'", UNDEF);
		return n;
	end
	if (c = '[') do
		return optsucc(condexpr());
	end
	return syntax(UNDEF);
end

funcall() do var n, c;
	n := factor();
	save(n);
	c := skip();
	while (c = '(') do
		n := rdargs(n);
		Car[Stack] := n;
		c := skip();
	end
	Rejected := c;
	return unsave(1);
end

termop() do var n, m, c;
	n := funcall();
	c := skip();
	while (c = '*' \/ c = '/' \/ c = 'm') do
		save(n);
		ie (c = '*') do
			m := funcall();
			save(m);
			n := cons(S_prod, cons(n, cons(m, NIL)));
		end
		else ie (c = '/') do
			m := funcall();
			save(m);
			n := cons(S_div, cons(n, cons(m, NIL)));
		end
		else if (c = 'm') do
			if (rdch() \= 'o' \/ rdch() \= 'd') do
				unsave(1);
				return syntax(UNDEF);
			end
			m := funcall();
			save(m);
			n := cons(S_mod, cons(n, cons(m, NIL)));
		end
		unsave(2);
		c := skip();
	end
	Rejected := c;
	return n;
end

sumop() do var n, m, c;
	n := termop();
	c := skip();
	while (c = '+' \/ c = '-') do
		save(n);
		ie (c = '+') do
			m := termop();
			save(m);
			n := cons(S_sum, cons(n, cons(m, NIL)));
		end
		else if (c = '-') do
			c := rdch();
			if (c = '>') do
				c := ARROW;
				unsave(1);
				leave;
			end
			Rejected := c;
			m := termop();
			save(m);
			n := cons(S_diff, cons(n, cons(m, NIL)));
		end
		unsave(2);
		c := skip();
	end
	Rejected := c;
	return n;
end

expr() do var n, m, c;
	n := sumop();
	c := skip();
	while (c = '=' \/ c = '<') do
		save(n);
		m := sumop();
		save(m);
		ie (c = '=') do
			n := cons(S_eq, cons(n, cons(m, NIL)));
		end
		else if (c = '<') do
			n := cons(S_lt, cons(n, cons(m, NIL)));
		end
		unsave(2);
		c := skip();
	end
	Rejected := c;
	return n;
end

mu_parse() do var n, c;
	n := expr();
	c := skip();
	if (c = ARROW) do
		n := fundef(n);
		c := skip();
	end
	if (c \= '.') Rejected := c;
	return n;
end

read() return Lispmode=1-> lisp_read(): mu_parse();

check(x, k0, kn) do var i, a;
	i := 0;
	a := x;
	while (\atomp(a)) do
		i := i+1;
		a := Cdr[a];
	end;
	if (a \= NIL \/ i < k0 \/ (kn \= %1 /\ i > kn))
		return syntax(x);
	return 0;
end

var	Tmpbuf::BUFLEN;

decl eval(1);
decl reset(0);

load(s) do var fd, ib, ip, ik, in, re, ex;
	fd := t.open(s, T3X.OREAD);
	if (fd < 0) return error("load", strsym(s));
	flushinp();
	reset();
	Input := fd;
	Inbuf := Tmpbuf;
	Line := 1;
	while (\Errflag) do
		Acc := read();
		if (Acc = EOT) leave;
		eval(Acc);
	end
	Line := 0;
	t.close(fd);
	flushinp();
end

sum(x, y) do var n, s, c, a, b;
	x := rev(Car[x]);
	save(x);
	y := rev(Car[y]);
	save(y);
	n := NIL;
	save(n);
	c := 0;
	while (x \= NIL \/ y \= NIL \/ c) do
		a := x = NIL-> 0: Car[x];
		b := y = NIL-> 0: Car[y];
		s := a + b + c;
		c := 0;
		if (s > 9) do
			s := s - 10;
			c := 1;
		end
		n := cons3(s, n, ATOMB);
		Car[Stack] := n;
		if (x \= NIL) x := Cdr[x];
		if (y \= NIL) y := Cdr[y];
	end
	unsave(3);
	return cons3(n, NIL, NUMBB);
end

succ(x) return sum(x, S_one);

lt(a, b) do var ka, kb;
	a := Car[a];
	b := Car[b];
	ka := length(a);
	kb := length(b);
	if (ka < kb) return %1;
	if (ka > kb) return 0;
	while (a \= NIL) do
		if (Car[a] < Car[b]) return %1;
		if (Car[a] > Car[b]) return 0;
		a := Cdr[a];
		b := Cdr[b];
	end
	return 0;
end

diff(x, y) do var n, s, c, a, b;
	if (lt(x, y)) return S_zero;
	x := rev(Car[x]);
	save(x);
	y := rev(Car[y]);
	save(y);
	n := NIL;
	save(n);
	c := 0;
	while (x \= NIL) do
		a := Car[x];
		b := y = NIL-> 0: Car[y];
		s := a - b - c;
		c := 0;
		if (s < 0) do
			s := s + 10;
			c := 1;
		end
		n := cons3(s, n, ATOMB);
		Car[Stack] := n;
		x := Cdr[x];
		if (y \= NIL) y := Cdr[y];
	end
	if (c) n := Car[S_zero];
	n := normalize(n);
	unsave(3);
	return cons3(n, NIL, NUMBB);
end

pred(x) return diff(x, S_one);

eq(a, b) do
	a := Car[a];
	b := Car[b];
	while (a \= NIL) do
		if (b = NIL) return 0;
		if (Car[a] \= Car[b]) return 0;
		a := Cdr[a];
		b := Cdr[b];
	end
	return b = NIL;
end

zerop(x) return Tag::x & NUMBB /\ caar(x) = 0 /\ cdar(x) = NIL;

x10(x) do var p;
	p := Car[x];
	while (Cdr[p] \= NIL) p := Cdr[p];
	Cdr[p] := cons3(0, NIL, ATOMB);
	return x;
end

d10(x) do var p;
	p := Car[x];
	if (Cdr[p] = NIL) return x;
	while (cddr(p) \= NIL) p := Cdr[p];
	Cdr[p] := NIL;
	return x;
end

prod(x, y) do var n, i, k, a, b;
	x := rev(Car[x]);
	save(x);
	y := copynum(y);
	save(y);
	n := S_zero;
	save(n);
	while (x \= NIL) do
		k := Car[x];
		for (i=0, k) do
			n := sum(n, y);
			Car[Stack] := n;
		end
		x := Cdr[x];
		if (x \= NIL) y := x10(y);
	end
	unsave(3);
	return cons3(normalize(Car[n]), NIL, NUMBB);
end

divmod(x, y) do var n, m, k, i, d;
	if (zerop(y)) return cons(S_zero, x);
	y := copynum(y);
	save(y);
	x := copynum(x);
	save(x);
	n := NIL;
	save(n);
	k := length(Car[x])-length(Car[y]);
	if (k < 0) do
		unsave(3);
		return cons(S_zero, x);
	end
	for (i=0, k) y := x10(y);
	for (i=0, k+1) do
		m := S_zero;
		save(m);
		d := 0;
		while (\lt(x, m)) do
			m := sum(m, y);
			d := d+1;
			Car[Stack] := m;
		end
		m := diff(m, y);
		Car[Stack] := m;
		d := d-1;
		x := diff(x, m);
		unsave(1);
		Car[Cdr[Stack]] := x;
		n := cons3(d, n, ATOMB);
		Car[Stack] := n;
		y := d10(y);
	end
	n := cons(cons3(normalize(nrev(n)), NIL, NUMBB), x);
	unsave(3);
	return n;
end

quot(x, y) return Car[divmod(x, y)];

rem(x, y) return Cdr[divmod(x, y)];

mkfun(v, tr) return cons(S_fun,
			cons(cons(v, NIL),
				cons(tr, NIL)));

church(c) do var x, n, f;
	x := addsym("x", UNDEF);
	f := addsym("f", UNDEF);
	n := x;
	while (\Errflag /\ \zerop(c)) do
		n := cons(f, cons(n, NIL));
		c := pred(c);
	end
	n := mkfun(x, n);
	n := mkfun(f, n);
	return eval(n);
end

type(x) return error("type", x);

numcheck(x) do var a, b;
	if (check(x, 3, 3)) return NIL;
	a := cadr(x);
	b := caddr(x);
	if (\numberp(a)) return type(x);
	if (\numberp(b)) return type(x);
	return 0;
end

printall(x) do var y;
	while (x \= NIL) do
		y := Car[x];
		print(y);
		pr("\s");
		x := Cdr[x];
	end
	nl();
	return y;
end

xlate(x, o, n) do var p;
	p := x;
	while (p \= NIL) do
		if (Car[p] = o) Car[p] := n;
		p := Cdr[p];
	end
	return x;
end

var	Trace;
var	Tco;

builtin(x) do var a;
	ie (S_prime = Car[x]) do
		if (check(x, 2, 2)) return NIL;
		a := cadr(x);
		if (\numberp(a)) return type(x);
		return succ(a);
	end
	else ie (S_sum = Car[x]) do
		if (numcheck(x)) return NIL;
		return sum(cadr(x), caddr(x));
	end
	else ie (S_diff = Car[x]) do
		if (numcheck(x)) return NIL;
		return diff(cadr(x), caddr(x));
	end
	else ie (S_eq = Car[x]) do
		if (numcheck(x)) return NIL;
		return eq(cadr(x), caddr(x))-> S_one: S_zero;
	end
	else ie (S_lt = Car[x]) do
		if (numcheck(x)) return NIL;
		return lt(cadr(x), caddr(x))-> S_one: S_zero;
	end
	else ie (S_prod = Car[x]) do
		if (numcheck(x)) return NIL;
		return prod(cadr(x), caddr(x));
	end
	else ie (S_div = Car[x]) do
		if (numcheck(x)) return NIL;
		a := quot(cadr(x), caddr(x));
		return a;
	end
	else ie (S_mod = Car[x]) do
		if (numcheck(x)) return NIL;
		return rem(cadr(x), caddr(x));
	end
	else ie (S_print = Car[x]) do
		if (check(x, 2, %1)) return NIL;
		return printall(Cdr[x]);
	end
	else ie (S_cn = Car[x]) do
		if (check(x, 2, 2)) return NIL;
		a := cadr(x);
		if (\numberp(a)) return type(x);
		return church(a);
	end
	else ie (S_tco = Car[x]) do
		if (check(x, 2, 2)) return NIL;
		a := cadr(x);
		if (\numberp(a)) return type(x);
		Tco := caar(a);
		return a;
	end
	else ie (S_trace = Car[x]) do
		if (check(x, 2, %1)) return NIL;
		a := cadr(x);
		ie (numberp(a)) do
			Trace := caar(a);
			Ntrace := NIL;
			return cons3(cons3(Trace, NIL, ATOMB), NIL, NUMBB);
		end
		else do
			Ntrace := xlate(Cdr[x], NIL, S_prime);
			Trace := 0;
			return Cdr[x];
		end
	end
	else ie (S_lisp = Car[x]) do
		if (check(x, 2, 2)) return NIL;
		a := cadr(x);
		if (\numberp(a)) return type(x);
		Lispmode := caar(a);
		return a;
	end
	else ie (S_count = Car[x]) do
		if (check(x, 1, %1)) return NIL;
		Ncount := xlate(Cdr[x], NIL, S_prime);
		Trace := 0;
		return Cdr[x];
	end
	else do
		syntax(x);
		return UNDEF;
	end
end

ckfun(x) do var p, q;
	check(x, 2, %1);
	if (atomp(cadr(x))) return syntax(x);
	p := Cdr[x];
	if (	\atomp(p) /\ \atomp(Car[p]) /\
		(atomp(caar(p)) \/ caaar(p) = S_prime)
	) do
		if (Cdr[p] = NIL \/ cddr(p) \= NIL) syntax(x);
		if (atomp(Car[p])) syntax(x);
		return;
	end
	while (\atomp(p)) do
		if (atomp(p) \/ atomp(Car[p])) return syntax(x);
		if (cdar(p) = NIL \/ cddar(p) \= NIL) return syntax(x);
		if (atomp(caar(p))) syntax(x);
		p := Cdr[p];
	end
	if (p \= NIL) syntax(x);
end

specialp(n)
	return	n = S_if \/
		n = S_fun \/
		n = S_funstar \/
		n = S_rec \/
		n = S_def;

predicate(x) do
	if (atomp(x) \/ atomp(Car[x]) \/ atomp(cdar(x)))
		return syntax(cons(S_if, x));
	return caar(x);
end

struct MODES = MHALT, MEXPR, MLIST, MBETA, MRETN, MPRED, MDEFN, MRECF;

special(x, pm) do
	if (S_if = Car[x]) do
		check(x, 2, %1);
		msave(MPRED);
		pm[0] := MEXPR;
		save(Cdr[x]);
		return predicate(Cdr[x]);
	end
	if (S_fun = Car[x]) do
		ckfun(x);
		pm[0] := munsave();
		return cons(S_funstar, cons(Env, cons(NIL, Cdr[x])));
	end
	if (S_funstar = Car[x]) do
		! check(x, 3, %1);
		pm[0] := munsave();
		return x;
	end
	if (S_rec = Car[x]) do
		check(x, 3, 3);
		if (\symbolp(cadr(x))) syntax(x);
		msave(MRECF);
		pm[0] := MEXPR;
		save(cadr(x));
		return caddr(x);
	end
	if (S_def = Car[x]) do
		check(x, 3, 3);
		if (\symbolp(cadr(x))) syntax(x);
		msave(MDEFN);
		pm[0] := MEXPR;
		save(cadr(x));
		return caddr(x);
	end
	syntax(x);
	return UNDEF;
end

match(v, a) do var e, x, aa, vv;
	e := cons(NIL, NIL);
	save(e);
	x := 0;
	while (v \= NIL) do
		aa := Car[a];
		vv := Car[v];
		if (a = NIL) x := %1;
		ie (numberp(vv)) do
			ie (numberp(aa))
				x := \eq(vv, aa);
			else
				x := %1;
		end
		else ie (atomp(vv)) do
			Car[e] := cons(cons(vv, cons(aa, NIL)), Car[e]);
		end
		else if (Car[vv] = S_prime /\ numberp(aa)) do
			aa := pred(aa);
			vv := cadr(vv);
			Car[e] := cons(cons(vv, cons(aa, NIL)), Car[e]);
		end
		if (x) do
			unsave(1);
			return NIL;
		end
		a := Cdr[a];
		v := Cdr[v];
	end
	if (a \= NIL) Car[Stack] := NIL;
	return unsave(1);
end

bindargs(v, a) do var e, p;
	p := v;
	ie (atomp(caar(v)) \/ caaar(v) = S_prime) do
		e := match(Car[p], a);
		if (e = NIL) return error("no match", v);
		Env := cons(Car[e], Env);
		return cadr(p);
	end
	else do
		while (p \= NIL) do
			e := match(caar(p), a);
			if (e \= NIL) leave;
			p := Cdr[p];
		end
		if (p = NIL) return error("no match", v);
		Env := cons(Car[e], Env);
		return cadar(p);
	end
end

funapp(x) do var b;
	Acc := x;
	if (atomp(Car[x]) \/ caar(x) \= S_funstar)
		return syntax(x);
	ie (Tco /\ Mstack \= NIL /\ Car[Mstack] = MRETN) do
		Env := cadar(Acc);
		b := bindargs(cdddar(Acc), Cdr[Acc]);
	end
	else do
		save(Env);
		Env := cadar(Acc);
		b := bindargs(cdddar(Acc), Cdr[Acc]);
		msave(MRETN);
	end
	return b;
end

prtrace() do var p;
	p := Mstack;
	pr("+ ");
	while (p \= NIL) do
		if (Car[p] = MRETN) pr(" ");
		p := Cdr[p];
	end
	print(Acc);
	nl();
end

eval(x) do var n, m, e;
	Count := S_zero;
	Acc := x;
	msave(MHALT);
	m := MEXPR;
	while (\Errflag) do
		ie (m = MEXPR) do
			ie (numberp(Acc)) do
				m := munsave();
			end
			else ie (symbolp(Acc)) do
				n := Car[lookup(Acc)];
				if (n = UNDEF)
					return error("undefined", Acc);
				Acc := n;
				m := munsave();
			end
			else ie (atomp(Acc)) do
				m := munsave();
			end
			else ie (specialp(Car[Acc])) do
				m := MBETA;
			end
			else do
				save(Cdr[Acc]);
				Acc := Car[Acc];
				save(NIL);
				msave(MLIST);
			end
		end
		else ie (m = MLIST) do
			ie (cadr(Stack) = NIL) do
				Acc := nrev(cons(Acc, unsave(1)));
				unsave(1);
				m := MBETA;
			end
			else do
				Car[Stack] := cons(Acc, Car[Stack]);
				Acc := caadr(Stack);
				Car[Cdr[Stack]] := cdadr(Stack);
				msave(MLIST);
				m := MEXPR;
			end
		end
		else ie (m = MBETA) do
			ie (specialp(Car[Acc])) do
				Acc := special(Acc, @m);
			end
			else ie (symbolp(Car[Acc])) do
				if (Trace > 1) prtrace();
				if (Ntrace \= NIL /\ memq(Ntrace, Car[Acc]))
					prtrace();
				if (Ncount \= NIL /\ memq(Ncount, Car[Acc]))
					Count := succ(Count);
				Acc := builtin(Acc);
				m := munsave();
			end
			else do
				if (Trace) prtrace();
				if (Ntrace \= NIL /\ memq(Ntrace, Car[Acc]))
					prtrace();
				if (Ncount \= NIL /\ memq(Ncount, Car[Acc]))
					Count := succ(Count);
				Acc := funapp(Acc);
				m := MEXPR;
			end
		end
		else ie (m = MRETN) do
			Env := unsave(1);
			m := munsave();
		end
		else ie (m = MPRED) do
			n := unsave(1);
			ie (zerop(Acc)) do
				n := Cdr[n];
				if (n = NIL)
					return error("if: exhausted", UNDEF);
				save(n);
				Acc := predicate(n);
				msave(MPRED);
			end
			else do
				Acc := cadar(n);
			end
			m := MEXPR;
		end
		else ie (m = MRECF) do
			n := unsave(1);
			if (atomp(Acc) \/ Car[Acc] \= S_funstar)
				return error("rec: function expected", Acc);
			e := cons(n, cons(Acc, NIL));
			ie (cadr(Acc) = NIL) do
				e := cons(e, NIL);
				e := cons(e, NIL);
			end
			else do
				e := cons(e, caadr(Acc));
				e := cons(e, cdadr(Acc));
			end
			Car[Cdr[Acc]] := e;
			m := munsave();
		end
		else ie (m = MDEFN) do
			n := unsave(1);
			Car[lookup(n)] := Acc;
			if (functionp(Acc))
				Car[Cdr[Cdr[Acc]]] := n;
			Acc := n;
			m := munsave();
		end
		else if (m = MHALT) do
			return Acc;
		end
	end
	return NIL;
end

reset() do
	Parens := 0;
	Errflag := 0;
	Exhausted := 0;
	Rejected := %1;
	Acc := NIL;
	Env := NIL;
	Stack := NIL;
	Mstack := NIL;
	Tmpcar := NIL;
	Tmpcdr := NIL;
	Car[Znode] := NIL;
	Cdr[Znode] := NIL;
end

init() do
	Symbols := NIL;
	Freelist := NIL;
	Stack := NIL;
	Mstack := NIL;
	Znode := NIL;
	Tmpcar := NIL;
	Tmpcdr := NIL;
	S_zero := NIL;
	S_one := NIL;
	Acc := NIL;
	Env := NIL;
	Trace := 0;
	Tco := %1;
	Ntrace := NIL;
	Ncount := NIL;
	Count := NIL;
	Lispmode := 0;
	Line := 0;
	flushinp();
	Verbose_GC := 0;
	t.memfill(Tag, 0, NNODES);
	Znode := cons(NIL, NIL);
	S_zero := cons3(cons3(0, NIL, ATOMB), NIL, NUMBB);
	S_one := cons3(cons3(1, NIL, ATOMB), NIL, NUMBB);
	S_it := addsym("it", UNDEF);
	S_prime := addsym("'", SPCL);
	S_def := addsym("def", UNDEF);
	S_fun := addsym("fun", UNDEF);
	S_funstar := addsym("fun:", UNDEF);
	S_if := addsym("if", UNDEF);
	S_rec := addsym("rec", UNDEF);
	S_cn := addsym("cn", SPCL);
	S_count := addsym("count", SPCL);
	S_diff := addsym("-", SPCL);
	S_eq := addsym("=", SPCL);
	S_lisp := addsym("lisp", SPCL);
	S_lt := addsym("<", SPCL);
	S_prod := addsym("*", SPCL);
	S_div := addsym("div", SPCL);
	S_mod := addsym("mod", SPCL);
	S_print := addsym("print", SPCL);
	S_sum := addsym("+", SPCL);
	S_tco := addsym("tco", SPCL);
	S_trace := addsym("trace", SPCL);
	reset();
end

! Remove these when compiling to binary (CP/M, DOS).

extern signal(2);

interrupt() do
	pr("interrupt");
	nl();
	Errflag := 1;
end

do var a::15, i;
	init();
	i := 1;
	while (t.getarg(i, a, 15) > 0) do
		load(a);
		i := i+1;
	end
	pr("Mu - ");
	pr(ntoa(NNODES));
	pr(" nodes");
	nl();
	! Not supported on CP/M and DOS
	signal(2, @interrupt);
	while (1) do
		reset();
		pr("*\s");
		Acc := read();
		if (Lispmode = 2) do
			print(Acc);
			nl();
		end
		if (Errflag) loop;
		if (Acc = EOT) leave;
		Acc := eval(Acc);
		if (Errflag) loop;
		print(Acc);
		nl();
		Car[Cdr[S_it]] := Acc;
		if (Ncount \= NIL) do
			pr("+ count = ");
			print(Count);
			nl();
		end
	end
	fini(0);
end
