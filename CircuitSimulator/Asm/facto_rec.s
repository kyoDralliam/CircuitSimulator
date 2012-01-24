	.text

main:	
	li $a0, 7
        li $sp, 1000
	move $s0, $ra
	jal f
        li $a3, 0x1FFFFFFF
	move $a0, $v0
	
f:	
	addi $sp, $sp, -28
	sw $fp, 24($sp)
	addi $fp, $sp, 24
	li $a1, 0
	bne $a0, $a1, else

if:	
	li $v0, 1
	b suite
	
else:   
	addi $a0, $a0, -1
	sw $ra, 16($sp)
	jal f
	addi $a0, $a0, 1
	mul $v0, $v0, $a0
	lw $ra, 16($sp)	
suite:	
	lw $fp, 24($sp)
	addi $sp, $sp, 28
        and $ra, $ra, $ra
	jr $ra



