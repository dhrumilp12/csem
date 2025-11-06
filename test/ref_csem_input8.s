	.text
	.file	"<stdin>"
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	3, 0x0                          # -- Begin function main
.LCPI0_0:
	.quad	0x403e000000000000              # double 30
	.text
	.globl	main
	.p2align	4, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# %bb.0:
	pushq	%rax
	.cfi_def_cfa_offset 16
	movl	$1, 4(%rsp)
	movl	$8, (%rsp)
	movq	m@GOTPCREL(%rip), %rax
	movsd	.LCPI0_0(%rip), %xmm0           # xmm0 = [3.0E+1,0.0E+0]
	jmp	.LBB0_1
	.p2align	4, 0x90
.LBB0_5:                                # %L1
                                        #   in Loop: Header=BB0_1 Depth=1
	decl	(%rsp)
.LBB0_1:                                # %userlbl_L1
                                        # =>This Inner Loop Header: Depth=1
	movslq	4(%rsp), %rcx
	movl	(%rsp), %edx
	imull	%ecx, %edx
	xorps	%xmm1, %xmm1
	cvtsi2sd	%edx, %xmm1
	addsd	(%rax,%rcx,8), %xmm1
	movsd	%xmm1, (%rax,%rcx,8)
	ucomisd	%xmm0, %xmm1
	jbe	.LBB0_5
# %bb.2:                                # %L0
                                        #   in Loop: Header=BB0_1 Depth=1
	movl	4(%rsp), %ecx
	incl	%ecx
	movl	%ecx, 4(%rsp)
	cmpl	$4, %ecx
	jge	.LBB0_4
# %bb.3:                                # %L2
                                        #   in Loop: Header=BB0_1 Depth=1
	movl	$8, (%rsp)
	jmp	.LBB0_1
.LBB0_4:                                # %L3
	movsd	(%rax), %xmm0                   # xmm0 = mem[0],zero
	movsd	8(%rax), %xmm1                  # xmm1 = mem[0],zero
	movsd	16(%rax), %xmm2                 # xmm2 = mem[0],zero
	movsd	24(%rax), %xmm3                 # xmm3 = mem[0],zero
	movsd	32(%rax), %xmm4                 # xmm4 = mem[0],zero
	movsd	40(%rax), %xmm5                 # xmm5 = mem[0],zero
	leaq	.L__unnamed_1(%rip), %rdi
	movb	$6, %al
	callq	print@PLT
	xorl	%eax, %eax
	popq	%rcx
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end0:
	.size	main, .Lfunc_end0-main
	.cfi_endproc
                                        # -- End function
	.type	m,@object                       # @m
	.bss
	.globl	m
	.p2align	4, 0x0
m:
	.zero	48
	.size	m, 48

	.type	.L__unnamed_1,@object           # @0
	.section	.rodata.str1.1,"aMS",@progbits,1
.L__unnamed_1:
	.asciz	"%5.1f %5.1f %5.1f %5.1f %5.1f %5.1f\n"
	.size	.L__unnamed_1, 37

	.section	".note.GNU-stack","",@progbits
