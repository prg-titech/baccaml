.data
.balign	8
.text
interpret.39:
	movl	(bytecode.40,pc.41,4), opcode.61
	cmpl	$0, opcode.61
	jne	je_else.84
	movl	pc.41, Ti20.81
	addl	$1, Ti20.81
	movl	a.42, Ti22.83
	addl	$1, Ti22.83
	movl	Ti22.83, %ecx
	movl	Ti20.81, %ebx
	movl	bytecode.40, %eax
	jmp	interpret.39
je_else.84:
	cmpl	$1, opcode.61
	jne	je_else.85
	movl	pc.41, Ti25.77
	addl	$1, Ti25.77
	movl	a.42, Ti27.79
	subl	$1, Ti27.79
	movl	Ti27.79, %ecx
	movl	Ti25.77, %ebx
	movl	bytecode.40, %eax
	jmp	interpret.39
je_else.85:
	cmpl	$2, opcode.61
	jne	je_else.86
	movl	pc.41, Ti30.71
	addl	$1, Ti30.71
	movl	(bytecode.40,Ti30.71,4), target.72
	cmpl	$0, a.42
	jg	jle_else.87
	movl	pc.41, Ti33.75
	addl	$2, Ti33.75
	movl	a.42, %ecx
	movl	Ti33.75, %ebx
	movl	bytecode.40, %eax
	jmp	interpret.39
jle_else.87:
	movl	a.42, %ecx
	movl	target.72, %ebx
	movl	bytecode.40, %eax
	jmp	interpret.39
je_else.86:
	cmpl	$3, opcode.61
	jne	je_else.88
	movl	pc.41, Ti36.68
	addl	$1, Ti36.68
	movl	(bytecode.40,Ti36.68,4), target.69
	movl	a.42, %ecx
	movl	target.69, %ebx
	movl	bytecode.40, %eax
	jmp	interpret.39
je_else.88:
	cmpl	$4, opcode.61
	jne	je_else.89
	movl	a.42, %eax
	ret
je_else.89:
	movl	$-1, %eax
	ret
test_trace.1000:
	movl	a.42, Ti27.79
	subl	$1, Ti27.79
	movl	Ti27.79, a.42
	cmpl	Ti31.73, a.42
	jg	jle_else.90
	movl	pc.41, Ti33.75
	addl	$2, Ti33.75
	movl	a.42, %ecx
	movl	Ti33.75, %ebx
	movl	bytecode.40, %eax
	jmp	interpret.39
jle_else.90:
	movl	a.42, Ti27.79
	subl	$1, Ti27.79
	movl	a.42, %eax
	jmp	test_trace.1000
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
	movl	$100, Ti5.43
	movl	$0, Ti6.44
	movl	Ti6.44, %ebx
	movl	Ti5.43, %eax
	call	min_caml_create_array
	movl	$1, Ti8.47
	movl	Ti8.47, 0(input.45)
	movl	$2, Ti10.50
	movl	Ti10.50, 4(input.45)
	movl	$0, Ti12.53
	movl	Ti12.53, 8(input.45)
	movl	$4, Ti14.56
	movl	Ti14.56, 12(input.45)
	movl	$0, Ti15.58
	movl	$100, Ti16.59
	movl	Ti16.59, %ecx
	movl	Ti15.58, %ebx
	movl	input.45, %eax
	call	interpret.39
	movl	Ti17.60, %eax
	call	min_caml_print_int
	popl	%ebp
	popl	%edi
	popl	%esi
	popl	%edx
	popl	%ecx
	popl	%ebx
	popl	%eax
	ret
