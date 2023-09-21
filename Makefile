# T3X/0 Makefile
# Nils M Holm, 2022
# Public domain / 0BSD license

V=6ee

# Where to keep the T3X libraries
#
T3XDIR=	/usr/local/t3x/0

# Where the binaries go
#
BINDIR=	/usr/bin

HOST=	unix

all:	txtrn.tc tcvm

# When something goes wrong during cross-compilation, this
# target will reset the code generator and runtime links.
#
reset:
	ln -fs cgtcvm.t cg.t
	ln -fs txtcvm.t t3x.t
	ln -fs txemtbin.t txemit.t

# Regular bootstrap using a pre-compiled Tcode/0 image
#
txtrn.tc:	txtrn.t cg.t txemit.t tcvm bin/txtrn0.tc
	./tcvm bin/txtrn0.tc txtrn

# Alternative bootstrap using an existing T3Xr7 compiler
#
alt: txtrn.t cg.t txemit.t txtrn0
	txx txtrn0 txtrn

txtrn0:	txtrn0.t
	tx txtrn0.t

# The Tcode/0 VM
#
tcvm:	tcvm.c
	cc -m32 -O2 -g -o tcvm tcvm.c

# Triple test of the Tcode/0 compiler
#
triple: txtrn.tc
	./tcvm txtrn.tc txtrn txtrn1
	./tcvm txtrn1.tc txtrn txtrn2
	cmp txtrn1.tc txtrn2.tc
	rm -f txtrn1.tc txtrn2.tc

# Run the test suite under the Tcode/0 VM
#
test:	programs/test.t tcvm txtrn.tc
	./tcvm txtrn.tc programs/test test
	./tcvm test.tc

# Native CP/M-Z80 compiler
#
tx-cpm.com:
	bin/build.sh cpm cpm

# Native DOS-8086 compiler
#
tx-dos.com:
	bin/build.sh dos dos

# HOST -> DOS-8086 cross compiler
#
tx-dos:
	bin/build.sh $(HOST) dos

# HOST -> CP/M-Z80 cross compiler
#
tx-cpm:
	bin/build.sh $(HOST) cpm

# HOST -> TCVM cross compiler
#
tx-tcvm:
	bin/build.sh $(HOST) tcvm

# Native HOST compiler
#
tx-$(HOST):
	bin/build.sh $(HOST) $(HOST)

# Triple test ofthe FreeBSD-386 compiler
#
fbsd-triple: tx-fbsd
	ln -fs txemtsrc.t txemit.t
	ln -fs targets/cgfdb386.t cg.t
	ln -fs targets/txfbd386.t t3x.t
	./tx-fbsd txtrn tx-fbsd2
	make reset
	cc -s -m32 -o tx-fbsd2 tx-fbsd2.s && rm tx-fbsd2.s
	cmp tx-fbsd tx-fbsd2 && rm tx-fbsd2

# System-wide installation
#
install:	tcvm txtrn.tc tx-$(HOST) tx-dos tx-cpm tx-tcvm
	install -d $(T3XDIR)
	install -d $(T3XDIR)/cpmz80
	install -d $(T3XDIR)/dos86c
	install -d $(T3XDIR)/fbd386
	install -d $(T3XDIR)/unx386
	install -c -m 0644 txtrn.tc $(T3XDIR)
	install -c -m 0644 txtcvm.t $(T3XDIR)/t3x.t
	install -c -m 0644 targets/txunx386.t $(T3XDIR)/unx386/t3x.t
	install -c -m 0644 targets/ux386lib.c $(T3XDIR)/unx386
	install -c -m 0644 targets/txfbd386.t $(T3XDIR)/fbd386/t3x.t
	install -c -m 0644 targets/txcpmz80.t $(T3XDIR)/cpmz80/t3x.t
	install -c -m 0644 targets/txdos86c.t $(T3XDIR)/dos86c/t3x.t
	install -c -m 0644 library/* $(T3XDIR)
	install -c tx-$(HOST) $(T3XDIR)
	install -c tx-dos $(T3XDIR)
	install -c tx-cpm $(T3XDIR)
	install -c tx-tcvm $(T3XDIR)
	install -c tcvm $(BINDIR)
	install -c bin/tx0.sh $(BINDIR)/tx0

csums:
	txsum -u <_checksums >_checksums.new
	mv -f _checksums.new _checksums

mksums:	clean
	find . -type f | grep -v t3x0-$V.zip | grep -v _checksums \
		| txsum -m >_checksums

arc:	clean
	(cd ..; zip -9r t3x0-$V.zip t3x0)
	mv ../t3x0-$V.zip .

clean:
	rm -f ftx.tc ftx.s
	rm -f txtrn0 txtrn1.tc txtrn.tc \
		tx-tcvm.tc tx-tcvm.com tx-tcvm \
		tx-cpm.tc tx-cpm.com tx-cpm \
		tx-dos.tc tx-dos.com tx-dos \
		tx-fbsd.tc tx-fbsd \
		tx-unix.tc tx-unix \
		cpmfile.tc dosfile.tc \
		tcvm test.tc *.core core t3x0-$V.zip

