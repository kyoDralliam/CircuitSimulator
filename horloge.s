# Multiplication et division n'utilisant que l'addition, la soustraction, et le
# décalage à gauche ou à droite.
# La multiplication se comporte bien sur des nombres négatifs, la division
# échoue lamentablement. Par ailleurs, aucun test n'est fait pour vérifier que
# l'on ne divise pas par 0, l'algorithme part en boucle infinie si c'est le cas.

main:
        # On sauvegarde $ra
        addi    $sp, $sp, 4
        sw      $ra, 0($sp)
        # Tests :-)

        li      $a0, 22742 
        li      $a1, 17
        jal     diviser
        move    $a0, $v0
        jal     print_int

        li      $a0, 42
        li      $a1, 17
        jal     multiplier
        move    $a0, $v0
        jal     print_int

        # On récupère $ra
        lw      $ra, 0($sp)
        addi    $sp, $sp, -4
        jr      $ra

# Affiche l'entier présent dans le registre $a0, suivi d'un retour à la ligne
print_int:
        li      $v0, 1
        syscall
        li      $v0, 4
        la      $a0, newline
        syscall
        jr      $ra

# Multiplie les deux entiers contenus dans $a0 et $a1
# Renvoie le résultat dans $v0
multiplier:
        # Si $a1 = 0, on met $a0 = 0
        bne     $a1, $zero, multiplier_debut
        li      $a0, 0 
    multiplier_debut:
        # Si $a1 = 1, on va directement à la sortie en renvoyant $a0
        move    $v0, $a0
        li      $t0, 1
        beq     $a1, $t0, multiplier_fin
        # Sinon, on va avoir besoin d'appeller la fonction "récursivement", donc
        # On sauvegarde $ra
        addi    $sp, $sp, 4
        sw      $ra, 0($sp)
        # On stocke ce qu'on rajoute au résultat en fonction de la parité de $a1
        addi    $sp, $sp, 4
        sw      $zero, 0($sp)
        andi    $t0, $a1, 1
        beq     $t0, $zero, multiplier_fintest
        sw      $a0, 0($sp)
    multiplier_fintest:
        # On divise $a1 par 2 et on fait un bel appel récursif
        li      $t0, 1
        sra     $a1, $a1, $t0
        jal     multiplier
        # On multiplie par 2 le résultat
        li      $t0, 1
        sll    $v0, $v0, $t0
        # On ajoute ce qu'on avait stocké tout à l'heure
        lw      $t0, 0($sp)
        addi    $sp, $sp, -4
        add     $v0, $v0, $t0
        # On récupère $ra
        lw      $ra, 0($sp)
        addi    $sp, $sp, -4
    multiplier_fin:
        jr      $ra

# Divise l'entier contenu dans $a0 par celui contenu dans $a1
# Renvoie le résultat dans $v0
diviser:
        # On sauvegarde $ra
        addi    $sp, $sp, 4
        sw      $ra, 0($sp)
        # On initialise le résultat de l'opération
        li      $v0, 0
    diviser_debut:
        # On regarde si $a1 est plus grand que $a0, dans ce cas on a fini
        bgt     $a1, $a0, diviser_fin
        # Sinon, on met $a1 dans $t1 et on l'augmente jusqu'à dépasser $a0
        move    $t1, $a1
        li      $t2, 1
    diviser_petiteboucle:
        bgt     $t1, $a0, diviser_finpetiteboucle
        sll     $t1, $t1, 1
        sll     $t2, $t2, 1
        b       diviser_petiteboucle
    diviser_finpetiteboucle:
        # On est allés un cran trop loin, on corrige ça
        # (nb : il doit y avoir une méthode plus propre, mais là j'ai la flemme)
        sra     $t1, $t1, 1
        sra     $t2, $t2, 1
        # On a bien positionné "$a1" (dans $t1), on l'enlève à $a0 en rajoutant
        # ce qu'il faut au quotient
        sub     $a0, $a0, $t1
        add     $v0, $v0, $t2
        b       diviser_debut
    diviser_fin:
        # On récupère $ra
        lw      $ra, 0($sp)
        addi    $sp, $sp, -4
        jr      $ra



	.data
newline:
	.asciiz "\n"
