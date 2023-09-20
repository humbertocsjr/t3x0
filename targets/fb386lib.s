# T3X/0 core module for FreeBSD on the 386
# Nils M Holm, 2022
# Public Domain / 0BSD License

	.data
args:	.long	0

	.text
	.globl	main
main:	lea	8(%esp),%eax
	mov	(%eax),%eax
	mov	%eax,args
	jmp	start

# t3x.bpw()

t3x_bpw:
	mov	$4,%eax
	ret

# t3x.newline(buf)

t3x_newline:
	mov	4(%esp),%esi
	mov	$10,%al
	mov	%al,(%esi)
	inc	%esi
	xor	%eax,%eax
	mov	%al,(%esi)
	mov	4(%esp),%eax
	ret

# t3x.memcopy(d, s, n)

t3x_memcopy:
	push	%ebp
	mov	%esp,%ebp
	mov	16(%ebp),%edi
	mov	12(%ebp),%esi
	mov	8(%ebp),%ecx
	cld
	cmp	%esi,%edi
	jz	endmcp
	jb	domc
	std
	add	%ecx,%esi
	add	%ecx,%edi
	dec	%esi
	dec	%edi
domc:	rep movsb
endmcp:	pop	%ebp
	ret

# t3x.memcomp

t3x_memcomp:
	push	%ebp
	mov	%esp,%ebp
	mov	16(%ebp),%edi
	mov	12(%ebp),%esi
	mov	8(%ebp),%ecx
	cmp	%esi,%edi
	jnz	domcm
mc0:	xor	%eax,%eax
	jmp	endmcm
domcm:	inc	%ecx
	cld
	repz cmpsb
	or	%ecx,%ecx
	jz	mc0
	xor	%eax,%eax
	mov	-1(%edi),%al
	xor	%ebx,%ebx
	mov	-1(%esi),%bl
	sub	%ebx,%eax
endmcm:	pop	%ebp
	ret

# t3x.memfill(d, c, n)

t3x_memfill:
	push	%ebp
	mov	%esp,%ebp
	mov	16(%ebp),%edi
	mov	12(%ebp),%eax
	mov	8(%ebp),%ecx
	cld
	rep stosb
	xor	%eax,%eax
	pop	%ebp
	ret

# t3x.memscan(s, c, n)

t3x_memscan:
	push	%ebp
	mov	%esp,%ebp
	mov	16(%ebp),%edi
	mov	12(%ebp),%eax
	mov	8(%ebp),%ecx
	mov	%edi,%edx
	inc	%ecx
	cld
	repnz scasb
	or	%ecx,%ecx
	jz	msc0
	mov	%edi,%eax
	sub	%edx,%eax
	jmp	endmsc
msc0:	xor	%eax,%eax
endmsc:	dec	%eax
	pop	%ebp
	ret

# t3x.getarg(n, b, k)

t3x_getarg:
	push	%ebp
	mov	%esp,%ebp
	mov	16(%ebp),%ecx
	mov	args,%esi
nxtarg:	mov	(%esi),%eax
	or	%eax,%eax
	jz	noarg
	or	%ecx,%ecx
	jz	getarg
	dec	%ecx
	add	$4,%esi
	jmp	nxtarg
getarg:	mov	(%esi),%esi
	mov	12(%ebp),%edi
	mov	8(%ebp),%ecx
	xor	%ebx,%ebx
getnc:	dec	%ecx
	jz	endma
	mov	(%esi),%al
	or	%al,%al
	jz	endma
	mov	%al,(%edi)
	inc	%esi
	inc	%edi
	inc	%ebx
	jmp	getnc
endma:	xor	%eax,%eax
	mov	%al,(%edi)
	mov	%ebx,%eax
	jmp	endarg
noarg:	xor	%eax,%eax
	dec	%eax
endarg:	pop	%ebp
	ret

# t3x.create(path)

t3x_create:
	push	$0644
	push	$0x601		# O_CREAT | O_TRUNC | O_WRONLY
	push	12(%esp)	# path
	call	open
	add	$12,%esp
	ret

# t3x.open(path, flags);

t3x_open:
	mov	4(%esp),%eax	# flags
	cmp	$1,%eax
	jnz	open1
	push	8(%esp)		# path
	call	t3x_create
	add	$4,%esp
	ret
open1:	cmp	$3,%eax
	jnz	open2
	push	$1
	push	12(%esp)
	call	open
	add	$8,%esp
	push	$2
	xor	%ebx,%ebx
	push	%ebx
	push	%ebx
	push	%eax
	call	lseek
	pop	%eax
	add	$12,%esp
	ret
open2:	push	4(%esp)		# flags
	push	12(%esp)	# path
	call	open
	add	$8,%esp
	ret

# t3x.read(fd, buf, len);

t3x_read:
	push	4(%esp)		# len
	push	12(%esp)	# buf
	push	20(%esp)	# fd
	call	read
	add	$12,%esp
	ret

# t3x.write(fd, buf, len);

t3x_write:
	push	4(%esp)		# len
	push	12(%esp)	# buf
	push	20(%esp)	# fd
	call	write
	add	$12,%esp
	ret

# t3x.seek(int fd, int pos, int how);

t3x_seek:
	mov	4(%esp),%ebx	# how
	mov	8(%esp),%eax	# pos
	cdq
	or	%ebx,%ebx
	jz	seek1
	cmp	$1,%ebx
	jz	seek1
	or	%eax,%eax
	jz	noneg
	neg	%eax
	dec	%edx
noneg:	cmp	$2,%ebx
	jz	seek1
	cmp	$3,%ebx
	jz	seek2
	xor	%eax,%eax
	dec	%eax
	ret
seek2:	mov	$1,%ebx
seek1: push	%ebx
	push	%edx		# offset, high word
	push	%eax		# offset, low word
	push	24(%esp)	# fd
	call	lseek
	add	$16,%esp
	ret

# t3x.close(fd);

t3x_close:
	push	4(%esp)		# fd
	call	close
	add	$4,%esp
	ret

# t3x.remove(path);

t3x_remove:
	push	4(%esp)		# path
	call	unlink
	add	$4,%esp
	ret

# t3x.rename(old, new);

t3x_rename:
	push	4(%esp)		# new
	push	12(%esp)	# old
	call	rename
	add	$8,%esp
	ret

# t3x.trunc(fd);

t3x_trunc:
	mov	4(%esp),%ebx	# fd
	push	$1
	xor	%eax,%eax
	push	%eax
	push	%eax
	push	%ebx
	call	lseek
	add	$16,%esp
	mov	4(%esp),%ebx	# fd
	push	%edx
	push	%eax
	push	%ebx
	call	ftruncate
	add	$12,%esp
	ret

start:

