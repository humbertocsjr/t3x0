#!/bin/sh

usage() {
	echo "Usage: $0 host target"
	echo "       available hosts/targets: tcvm cpm dos fbsd unix"
	exit 1
}

unsupported() {
	echo "$0: host=$1, target=$2 is not supported (too big)"
	exit 1
}

reset() {
	ln -fs txemtbin.t txemit.t
	ln -fs txtcvm.t t3x.t
	ln -fs cgtcvm.t cg.t
}

if [ $# != 2 ]; then usage; fi

case "$1$2" in
tcvmtcvm) eh=bin; et=bin; h=tcvm;   t=tcvm;   s=.tc;  dh=.;       dt=. ;;
tcvmcpm)  eh=bin; et=bin; h=tcvm;   t=xtocpm; s=.tc;  dh=.;       dt=targets ;;
tcvmdos)  eh=bin; et=bin; h=tcvm;   t=dos86c; s=.tc;  dh=.;       dt=targets ;;
tcvmfbsd) eh=src; et=bin; h=tcvm;   t=fbd386; s=.tc;  dh=.;       dt=targets ;;
tcvmunix) eh=src; et=bin; h=tcvm;   t=unx386; s=.tc;  dh=.;       dt=targets ;;
cpmtcvm)  eh=bin; et=bin; h=cpmz80; t=cpmtvm; s=.com; dh=targets; dt=targets ;;
cpmcpm)   eh=bin; et=bin; h=cpmz80; t=cpmz80; s=.com; dh=targets; dt=targets ;;
cpmdos)   eh=bin; et=bin; h=cpmz80; t=cpmdos; s=.com; dh=targets; dt=targets ;;
cpmfbsd)  unsupported $1 $2 ;;
cpmunix)  unsupported $1 $2 ;;
dostcvm)  eh=bin; et=bin; h=dos86c; t=tcvm;   s=.com; dh=targets; dt=. ;;
doscpm)   eh=bin; et=bin; h=dos86c; t=xtocpm; s=.com; dh=targets; dt=targets ;;
dosdos)   eh=bin; et=bin; h=dos86c; t=dos86c; s=.com; dh=targets; dt=targets ;;
dosfbsd)  unsupported $1 $2 ;;
dosunix)  unsupported $1 $2 ;;
fbsdtcvm) eh=src; et=bin; h=fbd386; t=tcvm;   s="";   dh=targets; dt=. ;;
fbsdcpm)  eh=src; et=bin; h=fbd386; t=xtocpm; s="";   dh=targets; dt=targets ;;
fbsddos)  eh=src; et=bin; h=fbd386; t=dos86c; s="";   dh=targets; dt=targets ;;
fbsdfbsd) eh=src; et=src; h=fbd386; t=fbd386; s="";   dh=targets; dt=targets ;;
unixtcvm) eh=src; et=bin; h=unx386; t=tcvm;   s="";   dh=targets; dt=. ;;
unixcpm)  eh=src; et=bin; h=unx386; t=xtocpm; s="";   dh=targets; dt=targets ;;
unixdos)  eh=src; et=bin; h=unx386; t=dos86c; s="";   dh=targets; dt=targets ;;
unixunix) eh=src; et=src; h=unx386; t=unx386; s="";   dh=targets; dt=targets ;;
*)        echo "$0: unknown host/target combination"; exit 1 ;;
esac

if [ ! -e tcvm ]; then
	echo "Tcode/0 VM (./tcvm) not found"
	exit 1
fi

if [ ! -f txtrn.tc ]; then
	echo "T3X translator (txtrn.tc) not found"
	exit 1
fi

if [ $h = tcvm ]; then
	echo "building tx-$2.tc"
else
	echo "building tx-$2.tc (as a side effect)"
fi

ln -fs txemt$eh.t txemit.t
ln -fs txtcvm.t t3x.t
ln -fs $dh/cg$h.t cg.t
./tcvm bin/txtrn0.tc txtrn tx-$2

if [ $h != tcvm ]; then
	if [ $h = fbd386 -o $h = unx386 ]; then
		echo "building tx-$2.s (intermediate)"
	else
		echo "building tx-$2$s"
	fi
	ln -fs txemt$et.t txemit.t
	ln -fs $dt/cg$t.t cg.t
	ln -fs $dh/tx$h.t t3x.t
	./tcvm tx-$2.tc txtrn tx-$2
	rm -f tx-$2.tc
fi

if [ $h = fbd386 ]; then
	echo "building tx-$2$s"
	cc -m32 -s -o tx-$2$s tx-$2.s
	rm -f tx-$2.s
fi

if [ $h = unx386 ]; then
	echo "building tx-$2$s"
	cc -m32 -s -o tx-$2$s tx-$2.s targets/ux386lib.c
	rm -f tx-$2.s
fi

reset
