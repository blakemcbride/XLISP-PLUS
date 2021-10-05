/* Assembly language suport GCC and XLISP */
	.data
	.align 4
NaN:	.word	0,0,1,0x7ff0
FPCW:
	.word	0,0	
/* real memory selector */
	.globl _cmem
_cmem:
	.word	0
/* floating point control word */
	.globl _bytesperline
_bytesperline:
	.word	-1,0	
/* maximum x value -- graphics */
savemode:
	.byte	0
	.text
.align 4
.globl _floor	
/* missing from mathlib */
_floor:
	pushl %ebp
	movl %esp,%ebp
	subl $8,%esp	
/* room for two temps */
	fnstcw -4(%ebp)
	movl -4(%ebp),%eax
	andl $-3073,%eax
	orl $1024,%eax
	movl %eax,-8(%ebp)
	fldcw -8(%ebp)
	fldl 8(%ebp)
	frndint
	fldcw -4(%ebp)
	leave
	ret
.align 4
.globl _ceil
_ceil:
	pushl %ebp
	movl %esp,%ebp
	subl $8,%esp
	fnstcw -4(%ebp)
	movl -4(%ebp),%eax
	andl $-3073,%eax
	orl $2048,%eax
	movl %eax,-8(%ebp)
	fldcw -8(%ebp)
	fldl 8(%ebp)
	frndint
	fldcw -4(%ebp)
	leave
	ret
.align 4
.globl _fabs
_fabs:
	fldl 4(%esp)
	fabs
	ret
	.align 4
	.globl _setfpcw
_setfpcw:
/* sets the floating point control word, masking off all fp interupts */
	fstcww	FPCW
	fwait
	orl	$0x3b, FPCW
	fldcww	FPCW
	ret

/* Set pixel (EGA/VGA modes) Derived from Richard Wilton's book */
	.align 4
	.globl _setdrawmode
_setdrawmode:
	movl	$0x3ce, %edx
	movl	$0x205, %eax   
/* write mode 2, read mode 0 */
	outw	%ax, %dx
	movl	4(%esp), %eax
	movb	%al,savemode
	movb	$0, %ah
	testb	$0x80, %al
	jz	overmode
	movb	$0x18, %ah
overmode:
	movb	$3, %al
	outw	%ax, %dx
	ret
	
	.align 4
	.globl _unsetdrawmode
_unsetdrawmode:
	movl	$0x3ce, %edx
	movl	$0xff08, %eax	/* restore defaults */
	outw	%ax, %dx
	movl	$5, %eax
	outw	%ax, %dx
	movl	$3, %eax
	outw	%ax, %dx
	ret

	.align 4
	.globl _setpixel
_setpixel:
	push	%ebp
	movl	%esp, %ebp		/* set up new stack base */
	push	%ebx
	movw	_cmem, %fs
	movl	12(%ebp), %eax		/* Arg Y */
	movl	8(%ebp), %ebx		/* Arg X */
	movb	%bl, %cl		/* low order byte of X */
	imull	_bytesperline, %eax	/* bytes per line * y address */
	shrl	$3, %ebx		/* x/8 */
	addl	%eax, %ebx		/* byte offset */
	addl	$0xa0000, %ebx		/* address of display */

	andb	$7, %cl			/* x & 7 */
	xorb	$7, %cl			/* number of bits to shift left */
	movb	$1, %ah			/* shift mask */

	shlb	%cl, %ah		/* bit mask is generated */
	movw	$0x3ce, %dx
	movb	$8, %al			/* set bit mask */
	outw	%ax, %dx

	.byte 0x64
	movb	0(%ebx), %al
	movb	savemode, %al
	.byte 0x64
	movb	%al, 0(%ebx)
	
	pop	%ebx
	pop	%ebp
	ret

	.align 4
	.globl _doscall
_doscall:
/* usage: 	int doscall(int EAX, int EBX, int ECX, int EDX) 
	returns EAX contents */
	pushl	%ebx
	pushl	%esi
	pushl	%edi
	movl	16(%esp), %eax
	movl	20(%esp), %ebx
	movl	24(%esp), %ecx
	movl	28(%esp), %edx
	int	$0x21
	popl	%edi
	popl	%esi
	popl	%ebx
	ret
	.align 4
	.globl _calldisp
_calldisp:
/* usage: 	int calldisp(int EAX, int EBX, int ECX, int EDX) 
	returns EAX contents */
	pushl	%ebx
	pushl	%esi
	pushl	%edi
	movl	16(%esp), %eax
	movl	20(%esp), %ebx
	movl	24(%esp), %ecx
	movl	28(%esp), %edx
	pushl	%ebp    
/* this is a kludge, hopefully will be fixed! */
	int	$0x10	
/* display */
	popl	%ebp	
/* finish of kludge */
	popl	%edi
	popl	%esi
	popl	%ebx
	ret
	.align 4
	.globl _calldispdx
_calldispdx:
/* usage: 	int calldisp(int EAX, int EBX, int ECX, int EDX) 
	returns EDX contents */
	pushl	%ebx
	pushl	%esi
	pushl	%edi
	movl	16(%esp), %eax
	movl	20(%esp), %ebx
	movl	24(%esp), %ecx
	movl	28(%esp), %edx
	pushl	%ebp	
/* this is a kludge, hopefully will be fixed! */
	int	$0x10	
/* display */
	popl	%ebp	
/* finish of kludge */
	mov	%edx, %eax
	popl	%edi
	popl	%esi
	popl	%ebx
	ret
	.align 4
	.globl	_doscalledx
_doscalledx:
/* usage: 	int doscall(int EAX, int EBX, int ECX, int EDX) 
	returns EDX contents */
	pushl	%ebx
	pushl	%esi
	pushl	%edi
	movl	16(%esp), %eax
	movl	20(%esp), %ebx
	movl	24(%esp), %ecx
	movl	28(%esp), %edx
	int	$0x21
	movl	%edx, %eax
	popl	%edi
	popl	%esi
	popl	%ebx
	ret
