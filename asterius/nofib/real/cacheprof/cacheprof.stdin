	.file	"heimdall.c"
	.version	"01.01"
gcc2_compiled.:
.data
	.align 4
	.type	 num_insns_done,@object
	.size	 num_insns_done,4
num_insns_done:
	.long 0
.section	.rodata
	.align 32
.LC46:
	.string	"heimdall: fatal error near insn %d:\n\t%s\n"
.text
	.align 4
	.type	 panic,@function
panic:
	subl $12,%esp
	movl 16(%esp),%eax
	pushl %eax
	movl num_insns_done,%eax
	pushl %eax
	pushl $.LC46
	movl stderr,%eax
	pushl %eax
	call fprintf
	addl $-12,%esp
	pushl $1
	call exit
.Lfe1:
	.size	 panic,.Lfe1-panic
.section	.rodata
.LC47:
	.string	"getIMem"
.text
	.align 4
	.type	 getIMem,@function
getIMem:
	subl $12,%esp
	movl 16(%esp),%edx
	movl 20(%esp),%eax
	cmpl $4,%edx
	jne .L341
	movl (%eax),%eax
	jmp .L345
	.p2align 4,,7
.L341:
	cmpl $1,%edx
	jne .L342
	movzbl (%eax),%eax
	jmp .L345
	.p2align 4,,7
.L342:
	cmpl $2,%edx
	je .L343
	addl $-12,%esp
	pushl $.LC47
	call panic
	xorl %eax,%eax
	addl $16,%esp
	jmp .L340
	.p2align 4,,7
.L343:
	movzwl (%eax),%eax
.L345:
.L340:
	addl $12,%esp
	ret
.Lfe2:
	.size	 getIMem,.Lfe2-getIMem
.section	.rodata
.LC48:
	.string	"setIMem"
.text
	.align 4
	.type	 setIMem,@function
setIMem:
	subl $12,%esp
	movl 16(%esp),%eax
	movl 20(%esp),%ecx
	movl 24(%esp),%edx
	cmpl $4,%eax
	jne .L347
	movl %edx,(%ecx)
	jmp .L346
	.p2align 4,,7
.L347:
	cmpl $1,%eax
	jne .L348
	movb %dl,(%ecx)
	jmp .L346
	.p2align 4,,7
.L348:
	cmpl $2,%eax
	jne .L349
	movw %dx,(%ecx)
	jmp .L346
	.p2align 4,,7
.L349:
	addl $-12,%esp
	pushl $.LC48
	call panic
	addl $16,%esp
.L346:
	addl $12,%esp
	ret
.Lfe3:
	.size	 setIMem,.Lfe3-setIMem
	.align 4
	.type	 getIRoM,@function
getIRoM:
	subl $12,%esp
	movl 16(%esp),%edx
	movl 20(%esp),%eax
	cmpb $0,24(%esp)
	je .L373
	cmpl $4,%edx
	jne .L374
	movl m_regs(,%eax,4),%eax
	jmp .L372
	.p2align 4,,7
.L374:
	cmpl $1,%edx
	jne .L376
	cmpl $3,%eax
	ja .L377
	movzbl m_regs(,%eax,4),%eax
	jmp .L372
	.p2align 4,,7
.L377:
	movzbl m_regs-15(,%eax,4),%eax
	jmp .L372
	.p2align 4,,7
.L376:
	cmpl $2,%edx
	jne .L379
	movzwl m_regs(,%eax,4),%eax
	jmp .L372
	.p2align 4,,7
.L379:
	addl $-12,%esp
	pushl $.LC47
	call panic
	xorl %eax,%eax
	jmp .L383
	.p2align 4,,7
.L373:
	addl $-8,%esp
	pushl %eax
	pushl %edx
	call getIMem
.L383:
	addl $16,%esp
.L372:
	addl $12,%esp
	ret
.Lfe4:
	.size	 getIRoM,.Lfe4-getIRoM
.section	.rodata
.LC49:
	.string	"calcBSF(1)"
.LC50:
	.string	"calcBSF(2)"
.text
	.align 4
	.type	 calcBSF,@function
calcBSF:
	subl $24,%esp
	pushl %ebx
	movl 32(%esp),%eax
	movl 36(%esp),%ebx
	cmpl $4,%eax
	je .L385
	cmpl $2,%eax
	je .L385
	cmpl $1,%eax
	je .L385
	addl $-12,%esp
	pushl $.LC49
	call panic
	addl $16,%esp
.L385:
	testl %ebx,%ebx
	jne .L386
	addl $-12,%esp
	pushl $.LC50
	call panic
	addl $16,%esp
.L386:
	xorl %eax,%eax
	testb $1,%bl
	jne .L388
	.p2align 4,,7
.L389:
	incl %eax
	btl %eax,%ebx
	jnc .L389
.L388:
	popl %ebx
	addl $24,%esp
	ret
.Lfe5:
	.size	 calcBSF,.Lfe5-calcBSF
.section	.rodata
.LC51:
	.string	"calcBSR(1)"
.LC52:
	.string	"calcBSR(2)"
.text
	.align 4
	.type	 calcBSR,@function
calcBSR:
	subl $20,%esp
	pushl %esi
	pushl %ebx
	movl 32(%esp),%ebx
	movl 36(%esp),%esi
	cmpl $4,%ebx
	je .L392
	cmpl $2,%ebx
	je .L392
	cmpl $1,%ebx
	je .L392
	addl $-12,%esp
	pushl $.LC51
	call panic
	addl $16,%esp
.L392:
	testl %esi,%esi
	jne .L393
	addl $-12,%esp
	pushl $.LC52
	call panic
	addl $16,%esp
.L393:
	leal -1(,%ebx,8),%eax
	btl %eax,%esi
	jc .L395
	.p2align 4,,7
.L396:
	decl %eax
	btl %eax,%esi
	jnc .L396
.L395:
	popl %ebx
	popl %esi
	addl $20,%esp
	ret
.Lfe6:
	.size	 calcBSR,.Lfe6-calcBSR
.section	.rodata
.LC53:
	.string	"calcSHLD"
.text
	.align 4
	.type	 calcSHLD,@function
calcSHLD:
	subl $24,%esp
	pushl %ebx
	movl 32(%esp),%edx
	movl 36(%esp),%ecx
	movl 40(%esp),%ebx
	movl 44(%esp),%eax
	cmpl $2,%edx
	je .L401
	ja .L405
	cmpl $1,%edx
	je .L402
	jmp .L403
	.p2align 4,,7
.L405:
	cmpl $4,%edx
	jne .L403
	movl %eax,%edx
	sall %cl,%edx
	movl $32,%eax
	jmp .L407
	.p2align 4,,7
.L401:
	movl %eax,%edx
	sall %cl,%edx
	movl $16,%eax
	jmp .L407
	.p2align 4,,7
.L402:
	movl %eax,%edx
	sall %cl,%edx
	movl $8,%eax
.L407:
	subl %ecx,%eax
	movl %eax,%ecx
	movl %ebx,%eax
	shrl %cl,%eax
	orl %eax,%edx
	movl %edx,%eax
	jmp .L406
	.p2align 4,,7
.L403:
	addl $-12,%esp
	pushl $.LC53
	call panic
	addl $16,%esp
	xorl %eax,%eax
.L406:
	popl %ebx
	addl $24,%esp
	ret
.Lfe7:
	.size	 calcSHLD,.Lfe7-calcSHLD
.section	.rodata
.LC54:
	.string	"calcSHRD"
.text
	.align 4
	.type	 calcSHRD,@function
calcSHRD:
	subl $24,%esp
	pushl %ebx
	movl 32(%esp),%edx
	movl 36(%esp),%ecx
	movl 40(%esp),%ebx
	movl 44(%esp),%eax
	cmpl $2,%edx
	je .L411
	ja .L415
	cmpl $1,%edx
	je .L412
	jmp .L413
	.p2align 4,,7
.L415:
	cmpl $4,%edx
	jne .L413
	movl %eax,%edx
	shrl %cl,%edx
	movl $32,%eax
	jmp .L417
	.p2align 4,,7
.L411:
	movl %eax,%edx
	shrl %cl,%edx
	movl $16,%eax
	jmp .L417
	.p2align 4,,7
.L412:
	movl %eax,%edx
	shrl %cl,%edx
	movl $8,%eax
.L417:
	subl %ecx,%eax
	movl %eax,%ecx
	movl %ebx,%eax
	sall %cl,%eax
	orl %eax,%edx
	movl %edx,%eax
	jmp .L416
	.p2align 4,,7
.L413:
	addl $-12,%esp
	pushl $.LC54
	call panic
	addl $16,%esp
	xorl %eax,%eax
.L416:
	popl %ebx
	addl $24,%esp
	ret
.Lfe8:
	.size	 calcSHRD,.Lfe8-calcSHRD
.section	.rodata
.LC55:
	.string	"calcROL"
.text
	.align 4
	.type	 calcROL,@function
calcROL:
	subl $16,%esp
	pushl %edi
	pushl %esi
	pushl %ebx
	movl 32(%esp),%edi
	movl 36(%esp),%esi
	movl 40(%esp),%ebx
	testl %esi,%esi
	je .L420
	.p2align 4,,7
.L421:
	cmpl $2,%edi
	je .L424
	ja .L428
	cmpl $1,%edi
	je .L425
	jmp .L426
	.p2align 4,,7
.L428:
	cmpl $4,%edi
	jne .L426
	movl %ebx,%eax
	shrl $31,%eax
	jmp .L430
	.p2align 4,,7
.L424:
	movl %ebx,%eax
	shrl $15,%eax
	jmp .L431
	.p2align 4,,7
.L425:
	movl %ebx,%eax
	shrl $7,%eax
.L431:
	andl $1,%eax
.L430:
	addl %ebx,%ebx
	orl %eax,%ebx
	jmp .L422
	.p2align 4,,7
.L426:
	addl $-12,%esp
	pushl $.LC55
	call panic
	addl $16,%esp
.L422:
	decl %esi
	jnz .L421
.L420:
	movl %ebx,%eax
	popl %ebx
	popl %esi
	popl %edi
	addl $16,%esp
	ret
.Lfe9:
	.size	 calcROL,.Lfe9-calcROL
.section	.rodata
.LC56:
	.string	"calcROR"
.text
	.align 4
	.type	 calcROR,@function
calcROR:
	subl $16,%esp
	pushl %edi
	pushl %esi
	pushl %ebx
	movl 32(%esp),%edi
	movl 36(%esp),%esi
	movl 40(%esp),%ebx
	testl %esi,%esi
	je .L434
	.p2align 4,,7
.L435:
	cmpl $2,%edi
	je .L438
	ja .L442
	cmpl $1,%edi
	je .L439
	jmp .L440
	.p2align 4,,7
.L442:
	cmpl $4,%edi
	jne .L440
	movl %ebx,%eax
	andl $1,%eax
	shrl $1,%ebx
	sall $31,%eax
	orl %eax,%ebx
	jmp .L436
	.p2align 4,,7
.L438:
	movl %ebx,%eax
	andl $1,%eax
	shrl $1,%ebx
	sall $15,%eax
	orl %eax,%ebx
	jmp .L436
	.p2align 4,,7
.L439:
	movl %ebx,%eax
	andl $1,%eax
	shrl $1,%ebx
	sall $7,%eax
	orl %eax,%ebx
	jmp .L436
	.p2align 4,,7
.L440:
	addl $-12,%esp
	pushl $.LC56
	call panic
	addl $16,%esp
.L436:
	decl %esi
	jnz .L435
.L434:
	movl %ebx,%eax
	popl %ebx
	popl %esi
	popl %edi
	addl $16,%esp
	ret
.Lfe10:
	.size	 calcROR,.Lfe10-calcROR
.section	.rodata
.LC57:
	.string	"calcSAR"
.text
	.align 4
	.type	 calcSAR,@function
calcSAR:
	subl $24,%esp
	pushl %ebx
	movl 32(%esp),%eax
	movl 40(%esp),%ebx
	cmpl $2,%eax
	je .L447
	ja .L451
	cmpl $1,%eax
	je .L448
	jmp .L449
	.p2align 4,,7
.L451:
	cmpl $4,%eax
	jne .L449
	jmp .L445
	.p2align 4,,7
.L447:
	movswl %bx,%ebx
	jmp .L445
	.p2align 4,,7
.L448:
	movsbl %bl,%ebx
	jmp .L445
	.p2align 4,,7
.L449:
	addl $-12,%esp
	pushl $.LC57
	call panic
	addl $16,%esp
.L445:
	movl 36(%esp),%ecx
	sarl %cl,%ebx
	movl %ebx,%eax
	popl %ebx
	addl $24,%esp
	ret
.Lfe11:
	.size	 calcSAR,.Lfe11-calcSAR
.section	.rodata
.LC58:
	.string	"topBitOf"
.text
	.align 4
	.type	 topBitOf,@function
topBitOf:
	subl $20,%esp
	pushl %esi
	pushl %ebx
	movl 32(%esp),%esi
	movl 36(%esp),%ebx
	cmpl $4,%esi
	je .L453
	cmpl $2,%esi
	je .L453
	cmpl $1,%esi
	je .L453
	addl $-12,%esp
	pushl $.LC58
	call panic
	addl $16,%esp
.L453:
	leal -1(,%esi,8),%ecx
	shrl %cl,%ebx
	movl %ebx,%eax
	andl $1,%eax
	popl %ebx
	popl %esi
	addl $20,%esp
	ret
.Lfe12:
	.size	 topBitOf,.Lfe12-topBitOf
	.align 4
	.type	 extend_s_8to16,@function
extend_s_8to16:
	movswl 4(%esp),%eax
	ret
.Lfe13:
	.size	 extend_s_8to16,.Lfe13-extend_s_8to16
	.align 4
	.type	 extend_s_16to32,@function
extend_s_16to32:
	movswl 4(%esp),%eax
	ret
.Lfe14:
	.size	 extend_s_16to32,.Lfe14-extend_s_16to32
.section	.rodata
.LC59:
	.string	"extend_s_to32"
.LC60:
	.string	"extend_s_8to"
.text
	.align 4
	.type	 extend_s_8to,@function
