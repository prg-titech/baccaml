# x86-64 runtime for MinCaml / BacCaml
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
	# xmm0 already has the float (System V ABI float arg)
	movq	min_caml_stderr(%rip), %rdi
	leaq	format_float(%rip), %rsi
	movl	$1, %eax	# 1 float arg in xmm
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
	# %rax = size, %rbx = initial value
	pushq	%rbp
	movq	%rsp, %rbp
	pushq	%rcx
	pushq	%rdx
	movq	%rax, %rcx
	movq	min_caml_hp(%rip), %rax
	movq	%rcx, %rdx
	shlq	$3, %rdx		# multiply by 8 (word size)
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
	# %rax = size, %xmm0 = initial value
	pushq	%rbp
	movq	%rsp, %rbp
	pushq	%rcx
	pushq	%rdx
	movq	%rax, %rcx
	movq	min_caml_hp(%rip), %rax
	movq	%rcx, %rdx
	shlq	$3, %rdx		# multiply by 8
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
	# xmm0 already has the value
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
.globl min_caml_divide
min_caml_divide:
	# %rax = dividend, %rbx = divisor
	pushq	%rbp
	movq	%rsp, %rbp
	andq	$-16, %rsp
	movq	%rax, %rdi
	movq	%rbx, %rsi
	call	divide
	movq	%rbp, %rsp
	popq	%rbp
	ret
.globl min_caml_modulo
min_caml_modulo:
	# %rax = dividend, %rbx = divisor
	pushq	%rbp
	movq	%rsp, %rbp
	andq	$-16, %rsp
	movq	%rax, %rdi
	movq	%rbx, %rsi
	call	modulo
	movq	%rbp, %rsp
	popq	%rbp
	ret
.globl min_caml_is_mj
min_caml_is_mj:
	ret
.globl min_caml_can_enter_jit
min_caml_can_enter_jit:
	# MinCaml args: %rax=stack, %rbx=sp, %rcx=bytecode, %rdx=pc
	# Bridge to C: %rdi=stack, %rsi=sp, %rdx=bytecode, %rcx=pc
	pushq	%rbp
	movq	%rsp, %rbp
	andq	$-16, %rsp
	movq	%rax, %rdi
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	# %rdx already has bytecode, need pc in %rcx
	# Actually: rdi=stack, rsi=sp, rdx=code, rcx=pc
	pushq	%rax
	pushq	%rbx
	movq	%rax, %rdi	# stack
	movq	%rbx, %rsi	# sp
	movq	%rcx, %rdx	# bytecode
	# %rdx = pc was in the 4th MinCaml reg
	# For c_can_enter_jit(stack, sp, code, pc):
	# We have stack=%rax, sp=%rbx, bytecode=%rcx, pc=%rdx in MinCaml convention
	movq	%rax, %rdi
	movq	%rbx, %rsi
	# swap rcx <-> rdx for C convention
	movq	%rcx, %r8
	movq	%rdx, %rcx
	movq	%r8, %rdx
	call	c_can_enter_jit
	popq	%rbx
	popq	%rax
	movq	%rbp, %rsp
	popq	%rbp
	ret
.globl min_caml_jit_merge_point
min_caml_jit_merge_point:
	# MinCaml args: %rax=stack, %rbx=sp, %rcx=bytecode, %rdx=pc
	pushq	%rbp
	movq	%rsp, %rbp
	andq	$-16, %rsp
	pushq	%rax
	pushq	%rbx
	pushq	%rcx
	pushq	%rdx
	# C convention: rdi=stack, rsi=sp, rdx=code, rcx=pc
	movq	%rax, %rdi
	movq	%rbx, %rsi
	movq	%rcx, %r8
	movq	%rdx, %rcx
	movq	%r8, %rdx
	call	c_jit_merge_point
	popq	%rdx
	popq	%rcx
	popq	%rbx
	popq	%rax
	movq	%rbp, %rsp
	popq	%rbp
	# Check guard failure
	movq	min_caml_guard_fail_flg(%rip), %r8
	cmpq	$0, %r8
	jg	min_caml_guard_restore
	jmp	min_caml_jit_merge_point_cont
min_caml_guard_restore:
	movq	min_caml_guard_stack(%rip), %rax
	movq	min_caml_guard_sp(%rip), %rbx
	movq	min_caml_guard_bytecode(%rip), %rcx
	movq	min_caml_guard_pc(%rip), %rdx
	movq	$0, min_caml_guard_fail_flg(%rip)
	jmp	interp
.globl min_caml_mj_call
min_caml_mj_call:
	# MinCaml args: %rax=stack, %rbx=sp, %rcx=bytecode, %rdx=pc
	pushq	%rbp
	movq	%rsp, %rbp
	andq	$-16, %rsp
	pushq	%rax
	pushq	%rbx
	# C convention: rdi=stack, rsi=sp, rdx=code, rcx=pc
	movq	%rax, %rdi
	movq	%rbx, %rsi
	movq	%rcx, %r8
	movq	%rdx, %rcx
	movq	%r8, %rdx
	call	c_mj_call
	popq	%rbx
	popq	%rax
	movq	%rbp, %rsp
	popq	%rbp
	ret
.globl min_caml_jit_setup
min_caml_jit_setup:
	# MinCaml args: %rax=stack, %rbx=sp, %rcx=bytecode, %rdx=pc
	pushq	%rbp
	movq	%rsp, %rbp
	andq	$-16, %rsp
	pushq	%rax
	pushq	%rbx
	# C convention: rdi=stack, rsi=sp, rdx=code, rcx=pc
	movq	%rax, %rdi
	movq	%rbx, %rsi
	movq	%rcx, %r8
	movq	%rdx, %rcx
	movq	%r8, %rdx
	call	c_jit_setup
	popq	%rbx
	popq	%rax
	movq	%rbp, %rsp
	popq	%rbp
	ret
.globl min_caml_guard_occur_at
min_caml_guard_occur_at:
	pushq	%rbp
	movq	%rsp, %rbp
	andq	$-16, %rsp
	movq	%rax, %rdi
	movq	%rbx, %rsi
	movq	%rcx, %r8
	movq	%rdx, %rcx
	movq	%r8, %rdx
	call	call_caml_guard_occur_at
	movq	%rbp, %rsp
	popq	%rbp
	ret
.globl min_caml_loop_start
min_caml_loop_start:
	ret
.globl min_caml_loop_end
min_caml_loop_end:
	ret
.globl min_caml_method_entry
min_caml_method_entry:
	ret
.globl min_caml_guard_promote
min_caml_guard_promote:
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
.globl min_caml_bp
min_caml_bp:
	.quad	0x0
.globl min_caml_sp
min_caml_sp:
	.quad	0x0
.globl min_caml_guard_stack
min_caml_guard_stack:
	.quad 0
.globl min_caml_guard_sp
min_caml_guard_sp:
	.quad 0
.globl min_caml_guard_bytecode
min_caml_guard_bytecode:
	.quad 0
.globl min_caml_guard_pc
min_caml_guard_pc:
	.quad 0
.globl	min_caml_guard_fail_flg
min_caml_guard_fail_flg:
	.quad 0
