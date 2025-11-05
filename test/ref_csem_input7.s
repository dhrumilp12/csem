	.text
	.file	"<stdin>"
	.globl	main                            # -- Begin function main
	.p2align	4, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# %bb.0:
	pushq	%rax
	.cfi_def_cfa_offset 16
	movl	%esi, (%rsp)
	movl	$0, 4(%rsp)
	movq	m@GOTPCREL(%rip), %rax
	jmp	.LBB0_1
	.p2align	4, 0x90
.LBB0_5:                                # %L6
                                        #   in Loop: Header=BB0_1 Depth=1
	incl	4(%rsp)
.LBB0_1:                                # %L0
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_3 Depth 2
	cmpl	$5, 4(%rsp)
	jg	.LBB0_6
# %bb.2:                                # %L1
                                        #   in Loop: Header=BB0_1 Depth=1
	movl	$5, (%rsp)
	.p2align	4, 0x90
.LBB0_3:                                # %L2
                                        #   Parent Loop BB0_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movslq	4(%rsp), %rcx
	movl	(%rsp), %edx
	imull	%ecx, %edx
	xorps	%xmm0, %xmm0
	cvtsi2sd	%edx, %xmm0
	addsd	(%rax,%rcx,8), %xmm0
	movsd	%xmm0, (%rax,%rcx,8)
	imull	$-1431655765, %edx, %ecx        # imm = 0xAAAAAAAB
	addl	$715827882, %ecx                # imm = 0x2AAAAAAA
	cmpl	$1431655765, %ecx               # imm = 0x55555555
	jb	.LBB0_5
# %bb.4:                                # %L4
                                        #   in Loop: Header=BB0_3 Depth=2
	movl	(%rsp), %ecx
	decl	%ecx
	movl	%ecx, (%rsp)
	testl	%ecx, %ecx
	jg	.LBB0_3
	jmp	.LBB0_5
.LBB0_6:                                # %L7
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
