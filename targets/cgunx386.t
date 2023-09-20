! T3X/0 back-end for generic Unix on the 386
! Basically the same as CGFBD386, but without the library
! Nils M Holm, 2019,2022
! In the Public Domain / 0BSD License

module cg;

 public const	BPW = 4;

 public const	GPOOL_SIZE = 0;

 public const	BUFLEN = 512;

 public const	SYMTBL_SIZE = 2048;
 public const	LABEL_SIZE = 4096;
 public const	NLIST_SIZE = 6144;
 public const	FWDCL_SIZE = 128;

 public const	LOCAL_LIMIT = 32760;

 public binary() return 0;

 public suffix() return ".s";

 public modpath() return [
	"/usr/local/t3x/0/unx386/",
	"",
	"library/",
	"/usr/local/t3x/0/",
	0 ];

 public header() do
	return 0;
 end

 public textseg() return ".text\n";
 public dataseg() return ".data\n";
 public defbyte() return ".byte ";
 public defword() return ".long ";
 public defaddr() return ".long L";
 public defglob() return ".long 0\n";

 public codefrags() return [
	[ CG_NULL,	[0]			],
	[ CG_PUSH,	["push %eax",0]		],
	[ CG_CLEAR,	["xor %eax,%eax",0]	],
	[ CG_DROP,	["pop %ebx",0]		],
	[ CG_LDVAL,	["mov $!w,%eax",0]	],
	[ CG_LDADDR,	["mov $L!w,%eax",0]	],
	[ CG_LDLREF,	["lea !w(%ebp),%eax",0]	],
	[ CG_LDGLOB,	["mov L!w,%eax",0]	],
	[ CG_LDLOCL,	["mov !w(%ebp),%eax",0]	],
	[ CG_STGLOB,	["mov %eax,L!w",0]	],
	[ CG_STLOCL,	["mov %eax,!w(%ebp)",0]	],
	[ CG_STINDR,	["pop %ebx",
			 "mov %eax,(%ebx)",0]	],
	[ CG_STINDB,	["pop %ebx",
			 "mov %al,(%ebx)",0]	],
	[ CG_INCGLOB,	["incl L!w",0]		],
	[ CG_INCLOCL,	["incl !w(%ebp)",0]	],
	[ CG_INCR,	["add $!w,%eax",0]	],
	[ CG_STACK,	["add $!w,%esp",0]	],
	[ CG_UNSTACK,	["add $!w,%esp",0]	],
	[ CG_LOCLVEC,	["mov %esp,%eax",
			 "push %eax",0]		],
	[ CG_GLOBVEC,	["mov %esp,L!w",0]	],
	[ CG_INDEX,	["shl $2,%eax",
			 "pop %ebx",
			 "add %ebx,%eax",0]	],
	[ CG_DEREF,	["mov (%eax),%eax",0]	],
	[ CG_INDXB,	["pop %ebx",
			 "add %ebx,%eax",0]	],
	[ CG_DREFB,	["mov %eax,%ebx",
			 "xor %eax,%eax",
			 "mov (%ebx),%al",0]	],
	[ CG_CALL,	["call L!w",0]		],
	[ CG_CALR,	["call *%eax",0]	],
	[ CG_JUMP,	["jmp L!w",0]		],
	[ CG_RJUMP,	["jmp L!w",0]		],
	[ CG_JMPFALSE,	["or %eax,%eax",
			 "jz L!w",0]		],
	[ CG_JMPTRUE,	["or %eax,%eax",
			 "jnz L!w",0]		],
	[ CG_FOR,	["pop %ebx",
			 "cmp %eax,%ebx",
			 "jge L!w",0]		],
	[ CG_FORDOWN,	["pop %ebx",
			 "cmp %eax,%ebx",
			 "jle L!w",0]		],
	[ CG_MKFRAME,	["push %ebp",
			 "mov %esp,%ebp",0]	],
	[ CG_DELFRAME,	["pop %ebp",0]		],
	[ CG_RET,	["ret",0]		],
	[ CG_HALT,	["push $!w",
			 "call _exit",0]	],
	[ CG_NEG,	["neg %eax",0]		],
	[ CG_INV,	["not %eax",0]		],
	[ CG_LOGNOT,	["neg %eax",
			 "sbb %eax,%eax",
			 "not %eax",0]		],
	[ CG_ADD,	["pop %ebx",
			 "add %ebx,%eax",0]	],
	[ CG_SUB,	["mov %eax,%ebx",
			 "pop %eax",
			 "sub %ebx,%eax",0]	],
	[ CG_MUL,	["pop %ecx",
			 "imul %ecx",0]		],
	[ CG_DIV,	["mov %eax,%ecx",
			 "pop %eax",
			 "cltd",
			 "idiv %ecx",0]		],
	[ CG_MOD,	["mov %eax,%ecx",
			 "pop %eax",
			 "xor %edx,%edx",
			 "div %ecx",
			 "mov %edx,%eax",0]	],
	[ CG_AND,	["pop %ebx",
			 "and %ebx,%eax",0]	],
	[ CG_OR,	["pop %ebx",
			 "or %ebx,%eax",0]	],
	[ CG_XOR,	["pop %ebx",
			 "xor %ebx,%eax",0]	],
	[ CG_SHL,	["mov %eax,%ecx",
			 "pop %eax",
			 "shl %cl,%eax",0]	],
	[ CG_SHR,	["mov %eax,%ecx",
			 "pop %eax",
			 "shr %cl,%eax",0]	],
	[ CG_EQ,	["pop %ebx",
			 "cmp %ebx,%eax",
			 "setne %dl",
			 "movzbl %dl,%eax",
			 "dec %eax",0]		],
	[ CG_NE,	["pop %ebx",
			 "cmp %ebx,%eax",
			 "sete %dl",
			 "movzbl %dl,%eax",
			 "dec %eax",0]		],
	[ CG_LT,	["pop %ebx",
			 "cmp %eax,%ebx",
			 "setge %dl",
			 "movzbl %dl,%eax",
			 "dec %eax",0]		],
	[ CG_GT,	["pop %ebx",
			 "cmp %eax,%ebx",
			 "setle %dl",
			 "movzbl %dl,%eax",
			 "dec %eax",0]		],
	[ CG_LE,	["pop %ebx",
			 "cmp %eax,%ebx",
			 "setg %dl",
			 "movzbl %dl,%eax",
			 "dec %eax",0]		],
	[ CG_GE,	["pop %ebx",
			 "cmp %eax,%ebx",
			 "setl %dl",
			 "movzbl %dl,%eax",
			 "dec %eax",0]		],
	[ CG_UMUL,	["pop %ecx",
			 "mul %ecx",0]		],
	[ CG_UDIV,	["mov %eax,%ecx",
			 "pop %eax",
			 "xor %edx,%edx",
			 "div %ecx",0]		],
	[ CG_ULT,	["pop %ebx",
			 "cmp %eax,%ebx",
			 "setae %dl",
			 "movzbl %dl,%eax",
			 "dec %eax",0]		],
	[ CG_UGT,	["pop %ebx",
			 "cmp %eax,%ebx",
			 "setbe %dl",
			 "movzbl %dl,%eax",
			 "dec %eax",0]		],
	[ CG_ULE,	["pop %ebx",
			 "cmp %eax,%ebx",
			 "seta %dl",
			 "movzbl %dl,%eax",
			 "dec %eax",0]		],
	[ CG_UGE,	["pop %ebx",
			 "cmp %eax,%ebx",
			 "setb %dl",
			 "movzbl %dl,%eax",
			 "dec %eax",0]		],
	[ CG_JMPEQ,	["pop %ebx",
			 "cmp %eax,%ebx",
			 "je L!w",0]		],
	[ CG_JMPNE,	["pop %ebx",
			 "cmp %eax,%ebx",
			 "jne L!w",0]		],
	[ CG_JMPLT,	["pop %ebx",
			 "cmp %eax,%ebx",
			 "jl L!w",0]		],
	[ CG_JMPGT,	["pop %ebx",
			 "cmp %eax,%ebx",
			 "jg L!w",0]		],
	[ CG_JMPLE,	["pop %ebx",
			 "cmp %eax,%ebx",
			 "jle L!w",0]		],
	[ CG_JMPGE,	["pop %ebx",
			 "cmp %eax,%ebx",
			 "jge L!w",0]		],
	[ CG_JMPULT,	["pop %ebx",
			 "cmp %eax,%ebx",
			 "jb L!w",0]		],
	[ CG_JMPUGT,	["pop %ebx",
			 "cmp %eax,%ebx",
			 "ja L!w",0]		],
	[ CG_JMPULE,	["pop %ebx",
			 "cmp %eax,%ebx",
			 "jbe L!w",0]		],
	[ CG_JMPUGE,	["pop %ebx",
			 "cmp %eax,%ebx",
			 "jae L!w",0]		],
	[ CG_SKIP,	["jmp L!w",0]		],
	[ CG_CALN,	["call t3x_!n",0]	],
	[ CG_LDNAM,	["mov $t3x_!n,%eax",0]	],
	[ %1,		[0]			] ];

 public optimizations() return [
	[ CG_EQ,	0,	CG_JMPFALSE,	CG_JMPNE	],
	[ CG_NE,	0,	CG_JMPFALSE,	CG_JMPEQ	],
	[ CG_LT,	0,	CG_JMPFALSE,	CG_JMPGE	],
	[ CG_GT,	0,	CG_JMPFALSE,	CG_JMPLE	],
	[ CG_LE,	0,	CG_JMPFALSE,	CG_JMPGT	],
	[ CG_GE,	0,	CG_JMPFALSE,	CG_JMPLT	],
	[ CG_EQ,	0,	CG_JMPTRUE,	CG_JMPEQ	],
	[ CG_NE,	0,	CG_JMPTRUE,	CG_JMPNE	],
	[ CG_LT,	0,	CG_JMPTRUE,	CG_JMPLT	],
	[ CG_GT,	0,	CG_JMPTRUE,	CG_JMPGT	],
	[ CG_LE,	0,	CG_JMPTRUE,	CG_JMPLE	],
	[ CG_GE,	0,	CG_JMPTRUE,	CG_JMPGE	],
	[ CG_LOGNOT,	0,	CG_JMPFALSE,	CG_JMPTRUE	],
	[ %1,		%1,	%1,		%1		],
	[ CG_LDVAL,	0,	CG_ADD,		CG_DROP		],
	%1 ];

 public library() return [
 	[ "\t.text",
	  "\t.globl\tstart",
 	  "start:", 0],
 	0 ];

end