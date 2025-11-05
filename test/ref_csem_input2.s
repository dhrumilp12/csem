	.text
	.file	"<stdin>"
	.section	.rodata.cst8,"aM",@progbits,8
	.p2align	3, 0x0                          # -- Begin function main
.LCPI0_0:
	.quad	0x4010000000000000              # double 4
.LCPI0_1:
	.quad	0x4018000000000000              # double 6
	.text
	.globl	main
	.p2align	4, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# %bb.0:
	subq	$24, %rsp
	.cfi_def_cfa_offset 32
	movl	$3, 4(%rsp)
	movabsq	$4616189618054758400, %rax      # imm = 0x4010000000000000
	movq	%rax, 16(%rsp)
	movl	$5, (%rsp)
	movabsq	$4618441417868443648, %rax      # imm = 0x4018000000000000
	movq	%rax, 8(%rsp)
	leaq	.L__unnamed_1(%rip), %rdi
	movsd	.LCPI0_0(%rip), %xmm0           # xmm0 = [4.0E+0,0.0E+0]
	movsd	.LCPI0_1(%rip), %xmm1           # xmm1 = [6.0E+0,0.0E+0]
	movl	$3, %esi
	movl	$5, %edx
	movb	$2, %al
	callq	print@PLT
	xorl	%eax, %eax
	addq	$24, %rsp
	.cfi_def_cfa_offset 8
	retq
.Lfunc_end0:
	.size	main, .Lfunc_end0-main
	.cfi_endproc
                                        # -- End function
	.type	.L__unnamed_1,@object           # @0
	.section	.rodata.str1.1,"aMS",@progbits,1
.L__unnamed_1:
	.asciz	"%d %3.2f %d %3.2f\n"
	.size	.L__unnamed_1, 19

	.section	".note.GNU-stack","",@progbits
