# x86-64 runtime for MinCaml test programs
# System V AMD64 ABI for external C calls
# MinCaml internal convention: args in %rax, %rbx, %rcx, %rdx, ...

.text
.globl min_caml_print_newline
min_caml_print_newline:
	pushq	%rbp
	movq	%rsp, %rbp
	andq	$-16, %rsp
	movl	$10, %edi
	call	putchar@PLT
	movq	%rbp, %rsp
	popq	%rbp
	ret
.globl min_caml_print_int
min_caml_print_int:
	pushq	%rbp
	movq	%rsp, %rbp
	andq	$-16, %rsp
	movq	%rax, %rsi
	leaq	format_int(%rip), %rdi
	xorl	%eax, %eax
	call	printf@PLT
	movq	%rbp, %rsp
	popq	%rbp
	ret
.globl min_caml_print_string
min_caml_print_string:
	pushq	%rbp
	movq	%rsp, %rbp
	andq	$-16, %rsp
	movq	%rax, %rdi
	xorl	%eax, %eax
	call	printf@PLT
	movq	%rbp, %rsp
	popq	%rbp
	ret
.globl min_caml_print_byte
min_caml_print_byte:
	pushq	%rbp
	movq	%rsp, %rbp
	andq	$-16, %rsp
	movl	%eax, %edi
	call	putchar@PLT
	movq	%rbp, %rsp
	popq	%rbp
	ret
.globl min_caml_prerr_int
min_caml_prerr_int:
	pushq	%rbp
	movq	%rsp, %rbp
	andq	$-16, %rsp
	movq	min_caml_stderr(%rip), %rdi
	leaq	format_int(%rip), %rsi
	movq	%rax, %rdx
	xorl	%eax, %eax
	call	fprintf@PLT
	movq	%rbp, %rsp
	popq	%rbp
	ret
.globl min_caml_prerr_byte
min_caml_prerr_byte:
	pushq	%rbp
	movq	%rsp, %rbp
	andq	$-16, %rsp
	movl	%eax, %edi
	movq	min_caml_stderr(%rip), %rsi
	call	fputc@PLT
	movq	%rbp, %rsp
	popq	%rbp
	ret
.globl min_caml_prerr_float
min_caml_prerr_float:
	pushq	%rbp
	movq	%rsp, %rbp
	andq	$-16, %rsp
	movq	min_caml_stderr(%rip), %rdi
	leaq	format_float(%rip), %rsi
	movl	$1, %eax
	call	fprintf@PLT
	movq	%rbp, %rsp
	popq	%rbp
	ret
.globl min_caml_read_int
min_caml_read_int:
	pushq	%rbp
	movq	%rsp, %rbp
	andq	$-16, %rsp
	subq	$16, %rsp
	leaq	-8(%rbp), %rsi
	leaq	format_long(%rip), %rdi
	xorl	%eax, %eax
	call	scanf@PLT
	movq	-8(%rbp), %rax
	movq	%rbp, %rsp
	popq	%rbp
	ret
.globl min_caml_read_float
min_caml_read_float:
	pushq	%rbp
	movq	%rsp, %rbp
	andq	$-16, %rsp
	subq	$16, %rsp
	leaq	-8(%rbp), %rsi
	leaq	format_float(%rip), %rdi
	xorl	%eax, %eax
	call	scanf@PLT
	movsd	-8(%rbp), %xmm0
	movq	%rbp, %rsp
	popq	%rbp
	ret
.globl min_caml_create_array
min_caml_create_array:
	pushq	%rbp
	movq	%rsp, %rbp
	pushq	%rcx
	pushq	%rdx
	movq	%rax, %rcx
	movq	min_caml_hp(%rip), %rax
	movq	%rcx, %rdx
	shlq	$3, %rdx
	addq	%rdx, min_caml_hp(%rip)
create_array_loop:
	cmpq	$0, %rcx
	jne	create_array_cont
create_array_exit:
	popq	%rdx
	popq	%rcx
	movq	%rbp, %rsp
	popq	%rbp
	ret
create_array_cont:
	decq	%rcx
	movq	%rbx, (%rax,%rcx,8)
	jmp	create_array_loop
.globl min_caml_create_float_array
min_caml_create_float_array:
	pushq	%rbp
	movq	%rsp, %rbp
	pushq	%rcx
	pushq	%rdx
	movq	%rax, %rcx
	movq	min_caml_hp(%rip), %rax
	movq	%rcx, %rdx
	shlq	$3, %rdx
	addq	%rdx, min_caml_hp(%rip)
create_float_array_loop:
	cmpq	$0, %rcx
	jne	create_float_array_cont
create_float_array_exit:
	popq	%rdx
	popq	%rcx
	movq	%rbp, %rsp
	popq	%rbp
	ret
create_float_array_cont:
	decq	%rcx
	movsd	%xmm0, (%rax,%rcx,8)
	jmp	create_float_array_loop
.globl min_caml_abs_float
min_caml_abs_float:
	pushq	%rbp
	movq	%rsp, %rbp
	andq	$-16, %rsp
	call	fabs@PLT
	movq	%rbp, %rsp
	popq	%rbp
	ret
.globl min_caml_sqrt
min_caml_sqrt:
	sqrtsd	%xmm0, %xmm0
	ret
.globl min_caml_floor
min_caml_floor:
	pushq	%rbp
	movq	%rsp, %rbp
	andq	$-16, %rsp
	call	floor@PLT
	movq	%rbp, %rsp
	popq	%rbp
	ret
.globl min_caml_int_of_float
min_caml_int_of_float:
.globl min_caml_truncate
min_caml_truncate:
	cvttsd2si	%xmm0, %rax
	ret
.globl min_caml_float_of_int
min_caml_float_of_int:
	cvtsi2sdq	%rax, %xmm0
	ret
.globl min_caml_cos
min_caml_cos:
	pushq	%rbp
	movq	%rsp, %rbp
	andq	$-16, %rsp
	call	cos@PLT
	movq	%rbp, %rsp
	popq	%rbp
	ret
.globl min_caml_sin
min_caml_sin:
	pushq	%rbp
	movq	%rsp, %rbp
	andq	$-16, %rsp
	call	sin@PLT
	movq	%rbp, %rsp
	popq	%rbp
	ret
.globl min_caml_atan
min_caml_atan:
	pushq	%rbp
	movq	%rsp, %rbp
	andq	$-16, %rsp
	call	atan@PLT
	movq	%rbp, %rsp
	popq	%rbp
	ret
.globl min_caml_print_debug
min_caml_print_debug:
	pushq	%rbp
	movq	%rsp, %rbp
	andq	$-16, %rsp
	movq	%rax, %rdi
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	call	interp_debug
	movq	%rbp, %rsp
	popq	%rbp
	ret
.globl min_caml_modulo
min_caml_modulo:
	pushq	%rbp
	movq	%rsp, %rbp
	andq	$-16, %rsp
	movq	%rax, %rdi
	movq	%rbx, %rsi
	call	modulo
	movq	%rbp, %rsp
	popq	%rbp
	ret
.globl min_caml_divide
min_caml_divide:
	pushq	%rbp
	movq	%rsp, %rbp
	andq	$-16, %rsp
	movq	%rax, %rdi
	movq	%rbx, %rsi
	call	divide
	movq	%rbp, %rsp
	popq	%rbp
	ret
.data
format_int:
	.asciz	"%ld"
format_long:
	.asciz	"%ld"
format_float:
	.asciz	"%lf"
.balign 16
float_0:
	.quad	0x0
float_1:
	.quad	0x3ff0000000000000
.balign 16
.globl min_caml_fnegd
min_caml_fnegd:
	.quad	0x8000000000000000
	.quad	0
.globl min_caml_hp
min_caml_hp:
	.quad	0x0
