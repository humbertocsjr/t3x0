#!/bin/sh

T3XDIR=/usr/local/t3x/0

tcvm=`which tcvm`
target='unix'
cc=cc

externs=""

usage() {
	echo "Usage: `basename $0` [-t target] [-e extern] infile [outfile]"
	echo "       targets: tcvm unix fbsd dos cpm boot"
	exit 1
}

if [ "$tcvm" = "" ]; then
	echo "$0: error: TCVM not installed"
	exit 1
fi

if [ "$1" = "" ]; then usage; fi

while true; do
	case $1 in
	-t)	target=$2; shift 2 ;;
	-e)	externs="$externs $2"; shift 2 ;;
	-*)	usage ;;
	*)	break ;;
	esac
done

case $target in
	boot)
		;;
	dos|cpm|fbsd|unix|tcvm)
		if [ ! -e "$T3XDIR/tx-$target" ]; then
			echo "$0: $target target not installed"
			exit 1
		fi
		;;
	*)	echo "$0: no such target: $target"
		exit 1
esac

out=${2:-$1}

if [ -x $T3XDIR/tx-$target ]; then
	$T3XDIR/tx-$target $1 $out
	if [ $? != 0 ]; then exit 1; fi
	if [ "$target" = "fbsd" ]; then
		$cc -s -m32 -o $out $out.s $externs
		rm -f $out.s
	fi
	if [ "$target" = "unix" ]; then
		$cc -s -m32 -o $out $out.s $T3XDIR/unx386/ux386lib.c $externs
		rm -f $out.s
	fi
else
	$tcvm $T3XDIR/txtrn.tc $1 $out
fi
