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
