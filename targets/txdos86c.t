! T3X core module for DOS on the 8086
! Nils M Holm, 2022, 2023
! Public Domain / 0BSD license

module t3x;

 public const	SYSIN = 0,
		SYSOUT = 1,
		SYSERR = 2;

 public const	OREAD = 0,
		OWRITE = 1,
		ORDWR = 2,
		OAPPND = 3;

 public const	SEEK_SET = 0,
		SEEK_FWD = 1,
		SEEK_END = 2,
		SEEK_BCK = 3;

 public struct	REG86 = REG_AX,
			REG_BX,
			REG_CX,
			REG_DX,
			REG_SI,
			REG_DI;

 public const	REG86_CF = 0x01,
		REG86_ZF = 0x40;

 ! mov bx,$01xx; jmp bx
 public inline	bpw(0) =	[ 0xbb, 0x08, 0x01, 0xff, 0xe3 ],
		newline(1) =	[ 0xbb, 0x0b, 0x01, 0xff, 0xe3 ],
		memcomp(3) =	[ 0xbb, 0x0e, 0x01, 0xff, 0xe3 ],
		memcopy(3) =	[ 0xbb, 0x11, 0x01, 0xff, 0xe3 ],
		memfill(3) =	[ 0xbb, 0x14, 0x01, 0xff, 0xe3 ],
		memscan(3) =	[ 0xbb, 0x17, 0x01, 0xff, 0xe3 ],
		getarg(3) =	[ 0xbb, 0x1a, 0x01, 0xff, 0xe3 ],
		open(2) =	[ 0xbb, 0x1d, 0x01, 0xff, 0xe3 ],
		close(1) =	[ 0xbb, 0x20, 0x01, 0xff, 0xe3 ],
		read(3) =	[ 0xbb, 0x23, 0x01, 0xff, 0xe3 ],
		write(3) =	[ 0xbb, 0x26, 0x01, 0xff, 0xe3 ],
		seek(3) =	[ 0xbb, 0x41, 0x01, 0xff, 0xe3 ],
		rename(2) =	[ 0xbb, 0x29, 0x01, 0xff, 0xe3 ],
		remove(1) =	[ 0xbb, 0x2c, 0x01, 0xff, 0xe3 ],
		trunc(1) =	[ 0xbb, 0x50, 0x01, 0xff, 0xe3 ],
		break(1) =	[ 0xbb, 0x53, 0x01, 0xff, 0xe3 ],
		intr86(2) =	[ 0xbb, 0x56, 0x01, 0xff, 0xe3 ];

 public create(s) return open(s, OWRITE);

end
