.data
.balign	8
.text
interpret.39:
	movl	(%eax,%ebx,4), %edx
	cmpl	$0, %edx
	jne	je_else.111
	addl	$1, %ebx
	addl	$1, %ecx
	jmp	interpret.39
je_else.111:
	cmpl	$1, %edx
	jne	je_else.112
	addl	$1, %ebx
	subl	$1, %ecx
	jmp	interpret.39
je_else.112:
	cmpl	$2, %edx
	jne	je_else.113
	movl	%ebx, %edx
	addl	$1, %edx
	movl	(%eax,%edx,4), %edx
	cmpl	$0, %ecx
	jg	jle_else.114
	addl	$2, %ebx
	jmp	interpret.39
jle_else.114:
	movl	%edx, %ebx
	jmp	interpret.39
je_else.113:
	cmpl	$3, %edx
	jne	je_else.115
	addl	$1, %ebx
	movl	(%eax,%ebx,4), %ebx
	jmp	interpret.39
je_else.115:
	cmpl	$4, %edx
	jne	je_else.116
	movl	%ecx, %eax
	ret
je_else.116:
	movl	$-1, %eax
	ret
test_trace:
	subl	$1, %eax
	cmpl	$0, %eax
	jg	jle_else.117
	jmp	test_trace
jle_else.117:
	movl	$0, %ebx
	movl	$0, %ecx
	movl	%ecx, 0(%ebp)
	movl	%eax, %ecx
	movl	%ebx, %eax
	movl	0(%ebp), %ebx
	jmp	interpret.39
.globl	min_caml_start
min_caml_start:
.globl	_min_caml_start
_min_caml_start: # for cygwin
	pushl	%eax
	pushl	%ebx
	pushl	%ecx
	pushl	%edx
	pushl	%esi
	pushl	%edi
	pushl	%ebp
	movl	32(%esp),%ebp
	movl	36(%esp),%eax
	movl	%eax,min_caml_hp
	movl	$100, %eax
	movl	$0, %ebx
	call	min_caml_create_array
	movl	$1, %ebx
	movl	%ebx, 0(%eax)
	movl	$2, %ebx
	movl	%ebx, 4(%eax)
	movl	$0, %ebx
	movl	%ebx, 8(%eax)
	movl	$4, %ebx
	movl	%ebx, 12(%eax)
	movl	$0, %ebx
	movl	$100, %ecx
	call	interpret.39
	call	min_caml_print_int
	popl	%ebp
	popl	%edi
	popl	%esi
	popl	%edx
	popl	%ecx
	popl	%ebx
	popl	%eax
	ret
