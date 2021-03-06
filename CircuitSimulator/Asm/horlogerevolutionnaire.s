# Multiplication et division n'utilisant que l'addition, la soustraction, et le
# décalage à gauche ou à droite.
# La multiplication ne se comporte probablement pas bien sur des nombres
# négatifs, la division échoue lamentablement. Par ailleurs, aucun test n'est
# fait pour vérifier que l'on ne divise pas par 0, l'algorithme part en boucle
# infinie si c'est le cas.

    .text
        li $s6, 0
        li      $sp, 4000
main:
        # On sauvegarde $ra
        #addi    $sp, $sp, 4
        #sw      $ra, 0($sp)

        #li      $gp, 0
            #li      $s2, 13
            #add     $t0, $s2, $s2
            #li      $t5, two_digits_to_segments 
            #add     $t0, $t0, $t5
            #lb      $t1, 0($t0)
            #la      $t2, clock_display
            #sb      $t1, 0($t2)


attendre:    
        #beq $t6, $zero, recalculer
        # On récupère le timestamp que l'on met dans $s7
        li      $t0, timestamp
        lw      $s7, 0($t0)
        #beq     $t7, $s7, attendre
        #li      $s7, 1328012616
        #li      $s7, 1327338988
        
recalculer:     

        # On récupère le timestamp que l'on met dans $s7
        #li      $t0, timestamp
        #lw      $s7, 0($t0)


        li $t6, 1
        move $t7, $s7
        
        # On calcule le nombre de jours écoulés depuis le 01/01/1970
        move    $a0, $s7
        li      $a1, 86400
        jal     diviser
        move    $s0, $v0 # $s0 = nombre de jours écoulés

        # ---- Calcul de l'heure ----
        # Calcul du nombre de secondes depuis le début de la journée
        move    $a0, $s0
        li      $a1, 86400
        jal     multiplier
        sub     $s1, $s7, $v0 # $s1 = "reste" en secondes
        move    $fp, $s1
        
        # Calcul des 2 premiers chiffres, les "faciles", car 100 | 86400
        move    $a0, $s1
        li      $a1, 864
        jal     diviser
        move    $s2, $v0 # $s2 = "heures" + dizaines de "minutes"
            # Affichage 
            add     $t0, $s2, $s2
            li      $t5, two_digits_to_segments 
            add     $t0, $t0, $t5
            lb      $t1, 0($t0)
            la      $t2, clock_display
            sb      $t1, 1($t2)
            add     $t0, $s2, $s2
            addi    $t0, $t0, 1
            li      $t5, two_digits_to_segments 
            add     $t0, $t0, $t5
            lb      $t1, 0($t0)
            sb      $t1, 2($t2)

        move    $a0, $s2
        li      $a1, 864
        jal     multiplier
        sub     $s1, $s1, $v0 # $s1 = "reste"

        li      $t5, conversiondeux
        add     $s1, $s1, $s1
        add     $s1, $s1, $s1
        add     $t5, $t5, $s1
        lw      $s3, 0($t5) # $s3 = unités des "minutes" + dizaines de "secondes"
            # Affichage 
            add     $t0, $s3, $s3
            li      $t5, two_digits_to_segments 
            add     $t0, $t0, $t5
            lb      $t1, 0($t0)
            la      $t2, clock_display
            sb      $t1, 3($t2)
            add     $t0, $s3, $s3
            addi    $t0, $t0, 1
            li      $t5, two_digits_to_segments 
            add     $t0, $t0, $t5
            lb      $t1, 0($t0)
            sb      $t1, 4($t2)

        li      $t5, conversionun
        add     $t5, $t5, $s1
        lw      $s4, 0($t5) # $s4 = unités des "secondes"
            # Affichage des secondes
            add     $t0, $s4, $s4
            li      $t5, two_digits_to_segments 
            add     $t0, $t0, $t5
            addi    $t0, $t0, 1
            lb      $t1, 0($t0)
            la      $t2, clock_display
            sb      $t1, 5($t2)
        

        ## Si c'est la première fois que l'on passe par là, on force le calcul de la date
        #beq     $gp, $zero, date

        ## Si on n'est pas 00:00:00, on recalcule l'heure sans recalculer la date
        #bne     $fp, $zero, attendre
    
    date:

            # Affichage de la date
            # Jour
            li      $s4, 6
            add     $t0, $s4, $s4
            li      $t5, two_digits_to_segments 
            add     $t0, $t0, $t5
            lb      $t1, 0($t0)
            la      $t2, clock_display
            sb      $t1, 6($t2)
            add     $t0, $s4, $s4
            addi    $t0, $t0, 1
            li      $t5, two_digits_to_segments 
            add     $t0, $t0, $t5
            lb      $t1, 0($t0)
            sb      $t1, 7($t2)
            # Mois
            li      $s5, 5
            add     $t0, $s5, $s5
            li      $t5, two_digits_to_segments 
            add     $t0, $t0, $t5
            lb      $t1, 0($t0)
            la      $t2, clock_display
            sb      $t1, 8($t2)
            add     $t0, $s5, $s5
            addi    $t0, $t0, 1
            li      $t5, two_digits_to_segments 
            add     $t0, $t0, $t5
            lb      $t1, 0($t0)
            sb      $t1, 9($t2)
            # Années
            li      $s6, 2
            add     $t0, $s6, $s6
            li      $t5, two_digits_to_segments 
            add     $t0, $t0, $t5
            lb      $t1, 0($t0)
            la      $t2, clock_display
            sb      $t1, 10($t2)
            add     $t0, $s6, $s6
            addi    $t0, $t0, 1
            li      $t5, two_digits_to_segments 
            add     $t0, $t0, $t5
            lb      $t1, 0($t0)
            sb      $t1, 11($t2)
            li      $s6, 20
            add     $t0, $s6, $s6
            li      $t5, two_digits_to_segments 
            add     $t0, $t0, $t5
            lb      $t1, 0($t0)
            la      $t2, clock_display
            sb      $t1, 12($t2)
            add     $t0, $s6, $s6
            addi    $t0, $t0, 1
            li      $t5, two_digits_to_segments 
            add     $t0, $t0, $t5
            lb      $t1, 0($t0)
            sb      $t1, 13($t2)

        addi    $s7, $s7, 1
        j       recalculer
        
        #li      $a0, 22742 
        #li      $a1, 17
        #jal     diviser
        #move    $a0, $v0
        #jal     print_int

        #li      $a0, 42
        #li      $a1, 17
        #jal     multiplier
        #move    $a0, $v0
        #jal     print_int

        # On récupère $ra
        #lw      $ra, 0($sp)
        #addi    $sp, $sp, -4
        #jr      $ra
        j       main

# Affiche l'entier présent dans le registre $a0, suivi d'un retour à la ligne
#print_int:
        #li      $v0, 1
        #syscall
        #li      $v0, 4
        #la      $a0, newline
        #syscall
        #jr      $ra

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
        # on sauvegarde $ra
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
        srl     $a1, $a1, 1
        jal     multiplier
        # On multiplie par 2 le résultat
        add     $v0, $v0, $v0
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
        add     $t1, $t1, $t1
        add     $t2, $t2, $t2
        j       diviser_petiteboucle
    diviser_finpetiteboucle:
        # On est allés un cran trop loin, on corrige ça
        # (nb : il doit y avoir une méthode plus propre, mais là j'ai la flemme)
        srl     $t1, $t1, 1
        srl     $t2, $t2, 1
        # On a bien positionné "$a1" (dans $t1), on l'enlève à $a0 en rajoutant
        # ce qu'il faut au quotient
        sub     $a0, $a0, $t1
        add     $v0, $v0, $t2
        j       diviser_debut
    diviser_fin:
        # On récupère $ra
        lw      $ra, 0($sp)
        addi    $sp, $sp, -4
        jr      $ra


megaboucle:
        j       megaboucle

	.data
newline:
	.asciiz "\n"

conversiondeux:
    .byte 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 2, 0, 0, 0, 2, 0, 0, 0, 2, 0, 0, 0, 2, 0, 0, 0, 2, 0, 0, 0, 2, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 3, 0, 0, 0, 3, 0, 0, 0, 3, 0, 0, 0, 3, 0, 0, 0, 3, 0, 0, 0, 3, 0, 0, 0, 3, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 4, 0, 0, 0, 4, 0, 0, 0, 4, 0, 0, 0, 4, 0, 0, 0, 4, 0, 0, 0, 4, 0, 0, 0, 4, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 5, 0, 0, 0, 5, 0, 0, 0, 5, 0, 0, 0, 5, 0, 0, 0, 5, 0, 0, 0, 5, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 0, 6, 0, 0, 0, 6, 0, 0, 0, 6, 0, 0, 0, 6, 0, 0, 0, 6, 0, 0, 0, 6, 0, 0, 0, 6, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 7, 0, 0, 0, 7, 0, 0, 0, 7, 0, 0, 0, 7, 0, 0, 0, 7, 0, 0, 0, 7, 0, 0, 0, 7, 0, 0, 0, 7, 0, 0, 0, 8, 0, 0, 0, 8, 0, 0, 0, 8, 0, 0, 0, 8, 0, 0, 0, 8, 0, 0, 0, 8, 0, 0, 0, 8, 0, 0, 0, 8, 0, 0, 0, 9, 0, 0, 0, 9, 0, 0, 0, 9, 0, 0, 0, 9, 0, 0, 0, 9, 0, 0, 0, 9, 0, 0, 0, 9, 0, 0, 0, 9, 0, 0, 0, 9, 0, 0, 0, 10, 0, 0, 0, 10, 0, 0, 0, 10, 0, 0, 0, 10, 0, 0, 0, 10, 0, 0, 0, 10, 0, 0, 0, 10, 0, 0, 0, 10, 0, 0, 0, 10, 0, 0, 0, 11, 0, 0, 0, 11, 0, 0, 0, 11, 0, 0, 0, 11, 0, 0, 0, 11, 0, 0, 0, 11, 0, 0, 0, 11, 0, 0, 0, 11, 0, 0, 0, 12, 0, 0, 0, 12, 0, 0, 0, 12, 0, 0, 0, 12, 0, 0, 0, 12, 0, 0, 0, 12, 0, 0, 0, 12, 0, 0, 0, 12, 0, 0, 0, 12, 0, 0, 0, 13, 0, 0, 0, 13, 0, 0, 0, 13, 0, 0, 0, 13, 0, 0, 0, 13, 0, 0, 0, 13, 0, 0, 0, 13, 0, 0, 0, 13, 0, 0, 0, 14, 0, 0, 0, 14, 0, 0, 0, 14, 0, 0, 0, 14, 0, 0, 0, 14, 0, 0, 0, 14, 0, 0, 0, 14, 0, 0, 0, 14, 0, 0, 0, 14, 0, 0, 0, 15, 0, 0, 0, 15, 0, 0, 0, 15, 0, 0, 0, 15, 0, 0, 0, 15, 0, 0, 0, 15, 0, 0, 0, 15, 0, 0, 0, 15, 0, 0, 0, 15, 0, 0, 0, 16, 0, 0, 0, 16, 0, 0, 0, 16, 0, 0, 0, 16, 0, 0, 0, 16, 0, 0, 0, 16, 0, 0, 0, 16, 0, 0, 0, 16, 0, 0, 0, 17, 0, 0, 0, 17, 0, 0, 0, 17, 0, 0, 0, 17, 0, 0, 0, 17, 0, 0, 0, 17, 0, 0, 0, 17, 0, 0, 0, 17, 0, 0, 0, 17, 0, 0, 0, 18, 0, 0, 0, 18, 0, 0, 0, 18, 0, 0, 0, 18, 0, 0, 0, 18, 0, 0, 0, 18, 0, 0, 0, 18, 0, 0, 0, 18, 0, 0, 0, 18, 0, 0, 0, 19, 0, 0, 0, 19, 0, 0, 0, 19, 0, 0, 0, 19, 0, 0, 0, 19, 0, 0, 0, 19, 0, 0, 0, 19, 0, 0, 0, 19, 0, 0, 0, 20, 0, 0, 0, 20, 0, 0, 0, 20, 0, 0, 0, 20, 0, 0, 0, 20, 0, 0, 0, 20, 0, 0, 0, 20, 0, 0, 0, 20, 0, 0, 0, 20, 0, 0, 0, 21, 0, 0, 0, 21, 0, 0, 0, 21, 0, 0, 0, 21, 0, 0, 0, 21, 0, 0, 0, 21, 0, 0, 0, 21, 0, 0, 0, 21, 0, 0, 0, 21, 0, 0, 0, 22, 0, 0, 0, 22, 0, 0, 0, 22, 0, 0, 0, 22, 0, 0, 0, 22, 0, 0, 0, 22, 0, 0, 0, 22, 0, 0, 0, 22, 0, 0, 0, 23, 0, 0, 0, 23, 0, 0, 0, 23, 0, 0, 0, 23, 0, 0, 0, 23, 0, 0, 0, 23, 0, 0, 0, 23, 0, 0, 0, 23, 0, 0, 0, 23, 0, 0, 0, 24, 0, 0, 0, 24, 0, 0, 0, 24, 0, 0, 0, 24, 0, 0, 0, 24, 0, 0, 0, 24, 0, 0, 0, 24, 0, 0, 0, 24, 0, 0, 0, 25, 0, 0, 0, 25, 0, 0, 0, 25, 0, 0, 0, 25, 0, 0, 0, 25, 0, 0, 0, 25, 0, 0, 0, 25, 0, 0, 0, 25, 0, 0, 0, 25, 0, 0, 0, 26, 0, 0, 0, 26, 0, 0, 0, 26, 0, 0, 0, 26, 0, 0, 0, 26, 0, 0, 0, 26, 0, 0, 0, 26, 0, 0, 0, 26, 0, 0, 0, 26, 0, 0, 0, 27, 0, 0, 0, 27, 0, 0, 0, 27, 0, 0, 0, 27, 0, 0, 0, 27, 0, 0, 0, 27, 0, 0, 0, 27, 0, 0, 0, 27, 0, 0, 0, 28, 0, 0, 0, 28, 0, 0, 0, 28, 0, 0, 0, 28, 0, 0, 0, 28, 0, 0, 0, 28, 0, 0, 0, 28, 0, 0, 0, 28, 0, 0, 0, 28, 0, 0, 0, 29, 0, 0, 0, 29, 0, 0, 0, 29, 0, 0, 0, 29, 0, 0, 0, 29, 0, 0, 0, 29, 0, 0, 0, 29, 0, 0, 0, 29, 0, 0, 0, 29, 0, 0, 0, 30, 0, 0, 0, 30, 0, 0, 0, 30, 0, 0, 0, 30, 0, 0, 0, 30, 0, 0, 0, 30, 0, 0, 0, 30, 0, 0, 0, 30, 0, 0, 0, 31, 0, 0, 0, 31, 0, 0, 0, 31, 0, 0, 0, 31, 0, 0, 0, 31, 0, 0, 0, 31, 0, 0, 0, 31, 0, 0, 0, 31, 0, 0, 0, 31, 0, 0, 0, 32, 0, 0, 0, 32, 0, 0, 0, 32, 0, 0, 0, 32, 0, 0, 0, 32, 0, 0, 0, 32, 0, 0, 0, 32, 0, 0, 0, 32, 0, 0, 0, 32, 0, 0, 0, 33, 0, 0, 0, 33, 0, 0, 0, 33, 0, 0, 0, 33, 0, 0, 0, 33, 0, 0, 0, 33, 0, 0, 0, 33, 0, 0, 0, 33, 0, 0, 0, 34, 0, 0, 0, 34, 0, 0, 0, 34, 0, 0, 0, 34, 0, 0, 0, 34, 0, 0, 0, 34, 0, 0, 0, 34, 0, 0, 0, 34, 0, 0, 0, 34, 0, 0, 0, 35, 0, 0, 0, 35, 0, 0, 0, 35, 0, 0, 0, 35, 0, 0, 0, 35, 0, 0, 0, 35, 0, 0, 0, 35, 0, 0, 0, 35, 0, 0, 0, 35, 0, 0, 0, 36, 0, 0, 0, 36, 0, 0, 0, 36, 0, 0, 0, 36, 0, 0, 0, 36, 0, 0, 0, 36, 0, 0, 0, 36, 0, 0, 0, 36, 0, 0, 0, 37, 0, 0, 0, 37, 0, 0, 0, 37, 0, 0, 0, 37, 0, 0, 0, 37, 0, 0, 0, 37, 0, 0, 0, 37, 0, 0, 0, 37, 0, 0, 0, 37, 0, 0, 0, 38, 0, 0, 0, 38, 0, 0, 0, 38, 0, 0, 0, 38, 0, 0, 0, 38, 0, 0, 0, 38, 0, 0, 0, 38, 0, 0, 0, 38, 0, 0, 0, 39, 0, 0, 0, 39, 0, 0, 0, 39, 0, 0, 0, 39, 0, 0, 0, 39, 0, 0, 0, 39, 0, 0, 0, 39, 0, 0, 0, 39, 0, 0, 0, 39, 0, 0, 0, 40, 0, 0, 0, 40, 0, 0, 0, 40, 0, 0, 0, 40, 0, 0, 0, 40, 0, 0, 0, 40, 0, 0, 0, 40, 0, 0, 0, 40, 0, 0, 0, 40, 0, 0, 0, 41, 0, 0, 0, 41, 0, 0, 0, 41, 0, 0, 0, 41, 0, 0, 0, 41, 0, 0, 0, 41, 0, 0, 0, 41, 0, 0, 0, 41, 0, 0, 0, 42, 0, 0, 0, 42, 0, 0, 0, 42, 0, 0, 0, 42, 0, 0, 0, 42, 0, 0, 0, 42, 0, 0, 0, 42, 0, 0, 0, 42, 0, 0, 0, 42, 0, 0, 0, 43, 0, 0, 0, 43, 0, 0, 0, 43, 0, 0, 0, 43, 0, 0, 0, 43, 0, 0, 0, 43, 0, 0, 0, 43, 0, 0, 0, 43, 0, 0, 0, 43, 0, 0, 0, 44, 0, 0, 0, 44, 0, 0, 0, 44, 0, 0, 0, 44, 0, 0, 0, 44, 0, 0, 0, 44, 0, 0, 0, 44, 0, 0, 0, 44, 0, 0, 0, 45, 0, 0, 0, 45, 0, 0, 0, 45, 0, 0, 0, 45, 0, 0, 0, 45, 0, 0, 0, 45, 0, 0, 0, 45, 0, 0, 0, 45, 0, 0, 0, 45, 0, 0, 0, 46, 0, 0, 0, 46, 0, 0, 0, 46, 0, 0, 0, 46, 0, 0, 0, 46, 0, 0, 0, 46, 0, 0, 0, 46, 0, 0, 0, 46, 0, 0, 0, 46, 0, 0, 0, 47, 0, 0, 0, 47, 0, 0, 0, 47, 0, 0, 0, 47, 0, 0, 0, 47, 0, 0, 0, 47, 0, 0, 0, 47, 0, 0, 0, 47, 0, 0, 0, 48, 0, 0, 0, 48, 0, 0, 0, 48, 0, 0, 0, 48, 0, 0, 0, 48, 0, 0, 0, 48, 0, 0, 0, 48, 0, 0, 0, 48, 0, 0, 0, 48, 0, 0, 0, 49, 0, 0, 0, 49, 0, 0, 0, 49, 0, 0, 0, 49, 0, 0, 0, 49, 0, 0, 0, 49, 0, 0, 0, 49, 0, 0, 0, 49, 0, 0, 0, 50, 0, 0, 0, 50, 0, 0, 0, 50, 0, 0, 0, 50, 0, 0, 0, 50, 0, 0, 0, 50, 0, 0, 0, 50, 0, 0, 0, 50, 0, 0, 0, 50, 0, 0, 0, 51, 0, 0, 0, 51, 0, 0, 0, 51, 0, 0, 0, 51, 0, 0, 0, 51, 0, 0, 0, 51, 0, 0, 0, 51, 0, 0, 0, 51, 0, 0, 0, 51, 0, 0, 0, 52, 0, 0, 0, 52, 0, 0, 0, 52, 0, 0, 0, 52, 0, 0, 0, 52, 0, 0, 0, 52, 0, 0, 0, 52, 0, 0, 0, 52, 0, 0, 0, 53, 0, 0, 0, 53, 0, 0, 0, 53, 0, 0, 0, 53, 0, 0, 0, 53, 0, 0, 0, 53, 0, 0, 0, 53, 0, 0, 0, 53, 0, 0, 0, 53, 0, 0, 0, 54, 0, 0, 0, 54, 0, 0, 0, 54, 0, 0, 0, 54, 0, 0, 0, 54, 0, 0, 0, 54, 0, 0, 0, 54, 0, 0, 0, 54, 0, 0, 0, 54, 0, 0, 0, 55, 0, 0, 0, 55, 0, 0, 0, 55, 0, 0, 0, 55, 0, 0, 0, 55, 0, 0, 0, 55, 0, 0, 0, 55, 0, 0, 0, 55, 0, 0, 0, 56, 0, 0, 0, 56, 0, 0, 0, 56, 0, 0, 0, 56, 0, 0, 0, 56, 0, 0, 0, 56, 0, 0, 0, 56, 0, 0, 0, 56, 0, 0, 0, 56, 0, 0, 0, 57, 0, 0, 0, 57, 0, 0, 0, 57, 0, 0, 0, 57, 0, 0, 0, 57, 0, 0, 0, 57, 0, 0, 0, 57, 0, 0, 0, 57, 0, 0, 0, 57, 0, 0, 0, 58, 0, 0, 0, 58, 0, 0, 0, 58, 0, 0, 0, 58, 0, 0, 0, 58, 0, 0, 0, 58, 0, 0, 0, 58, 0, 0, 0, 58, 0, 0, 0, 59, 0, 0, 0, 59, 0, 0, 0, 59, 0, 0, 0, 59, 0, 0, 0, 59, 0, 0, 0, 59, 0, 0, 0, 59, 0, 0, 0, 59, 0, 0, 0, 59, 0, 0, 0, 60, 0, 0, 0, 60, 0, 0, 0, 60, 0, 0, 0, 60, 0, 0, 0, 60, 0, 0, 0, 60, 0, 0, 0, 60, 0, 0, 0, 60, 0, 0, 0, 60, 0, 0, 0, 61, 0, 0, 0, 61, 0, 0, 0, 61, 0, 0, 0, 61, 0, 0, 0, 61, 0, 0, 0, 61, 0, 0, 0, 61, 0, 0, 0, 61, 0, 0, 0, 62, 0, 0, 0, 62, 0, 0, 0, 62, 0, 0, 0, 62, 0, 0, 0, 62, 0, 0, 0, 62, 0, 0, 0, 62, 0, 0, 0, 62, 0, 0, 0, 62, 0, 0, 0, 63, 0, 0, 0, 63, 0, 0, 0, 63, 0, 0, 0, 63, 0, 0, 0, 63, 0, 0, 0, 63, 0, 0, 0, 63, 0, 0, 0, 63, 0, 0, 0, 64, 0, 0, 0, 64, 0, 0, 0, 64, 0, 0, 0, 64, 0, 0, 0, 64, 0, 0, 0, 64, 0, 0, 0, 64, 0, 0, 0, 64, 0, 0, 0, 64, 0, 0, 0, 65, 0, 0, 0, 65, 0, 0, 0, 65, 0, 0, 0, 65, 0, 0, 0, 65, 0, 0, 0, 65, 0, 0, 0, 65, 0, 0, 0, 65, 0, 0, 0, 65, 0, 0, 0, 66, 0, 0, 0, 66, 0, 0, 0, 66, 0, 0, 0, 66, 0, 0, 0, 66, 0, 0, 0, 66, 0, 0, 0, 66, 0, 0, 0, 66, 0, 0, 0, 67, 0, 0, 0, 67, 0, 0, 0, 67, 0, 0, 0, 67, 0, 0, 0, 67, 0, 0, 0, 67, 0, 0, 0, 67, 0, 0, 0, 67, 0, 0, 0, 67, 0, 0, 0, 68, 0, 0, 0, 68, 0, 0, 0, 68, 0, 0, 0, 68, 0, 0, 0, 68, 0, 0, 0, 68, 0, 0, 0, 68, 0, 0, 0, 68, 0, 0, 0, 68, 0, 0, 0, 69, 0, 0, 0, 69, 0, 0, 0, 69, 0, 0, 0, 69, 0, 0, 0, 69, 0, 0, 0, 69, 0, 0, 0, 69, 0, 0, 0, 69, 0, 0, 0, 70, 0, 0, 0, 70, 0, 0, 0, 70, 0, 0, 0, 70, 0, 0, 0, 70, 0, 0, 0, 70, 0, 0, 0, 70, 0, 0, 0, 70, 0, 0, 0, 70, 0, 0, 0, 71, 0, 0, 0, 71, 0, 0, 0, 71, 0, 0, 0, 71, 0, 0, 0, 71, 0, 0, 0, 71, 0, 0, 0, 71, 0, 0, 0, 71, 0, 0, 0, 71, 0, 0, 0, 72, 0, 0, 0, 72, 0, 0, 0, 72, 0, 0, 0, 72, 0, 0, 0, 72, 0, 0, 0, 72, 0, 0, 0, 72, 0, 0, 0, 72, 0, 0, 0, 73, 0, 0, 0, 73, 0, 0, 0, 73, 0, 0, 0, 73, 0, 0, 0, 73, 0, 0, 0, 73, 0, 0, 0, 73, 0, 0, 0, 73, 0, 0, 0, 73, 0, 0, 0, 74, 0, 0, 0, 74, 0, 0, 0, 74, 0, 0, 0, 74, 0, 0, 0, 74, 0, 0, 0, 74, 0, 0, 0, 74, 0, 0, 0, 74, 0, 0, 0, 75, 0, 0, 0, 75, 0, 0, 0, 75, 0, 0, 0, 75, 0, 0, 0, 75, 0, 0, 0, 75, 0, 0, 0, 75, 0, 0, 0, 75, 0, 0, 0, 75, 0, 0, 0, 76, 0, 0, 0, 76, 0, 0, 0, 76, 0, 0, 0, 76, 0, 0, 0, 76, 0, 0, 0, 76, 0, 0, 0, 76, 0, 0, 0, 76, 0, 0, 0, 76, 0, 0, 0, 77, 0, 0, 0, 77, 0, 0, 0, 77, 0, 0, 0, 77, 0, 0, 0, 77, 0, 0, 0, 77, 0, 0, 0, 77, 0, 0, 0, 77, 0, 0, 0, 78, 0, 0, 0, 78, 0, 0, 0, 78, 0, 0, 0, 78, 0, 0, 0, 78, 0, 0, 0, 78, 0, 0, 0, 78, 0, 0, 0, 78, 0, 0, 0, 78, 0, 0, 0, 79, 0, 0, 0, 79, 0, 0, 0, 79, 0, 0, 0, 79, 0, 0, 0, 79, 0, 0, 0, 79, 0, 0, 0, 79, 0, 0, 0, 79, 0, 0, 0, 79, 0, 0, 0, 80, 0, 0, 0, 80, 0, 0, 0, 80, 0, 0, 0, 80, 0, 0, 0, 80, 0, 0, 0, 80, 0, 0, 0, 80, 0, 0, 0, 80, 0, 0, 0, 81, 0, 0, 0, 81, 0, 0, 0, 81, 0, 0, 0, 81, 0, 0, 0, 81, 0, 0, 0, 81, 0, 0, 0, 81, 0, 0, 0, 81, 0, 0, 0, 81, 0, 0, 0, 82, 0, 0, 0, 82, 0, 0, 0, 82, 0, 0, 0, 82, 0, 0, 0, 82, 0, 0, 0, 82, 0, 0, 0, 82, 0, 0, 0, 82, 0, 0, 0, 82, 0, 0, 0, 83, 0, 0, 0, 83, 0, 0, 0, 83, 0, 0, 0, 83, 0, 0, 0, 83, 0, 0, 0, 83, 0, 0, 0, 83, 0, 0, 0, 83, 0, 0, 0, 84, 0, 0, 0, 84, 0, 0, 0, 84, 0, 0, 0, 84, 0, 0, 0, 84, 0, 0, 0, 84, 0, 0, 0, 84, 0, 0, 0, 84, 0, 0, 0, 84, 0, 0, 0, 85, 0, 0, 0, 85, 0, 0, 0, 85, 0, 0, 0, 85, 0, 0, 0, 85, 0, 0, 0, 85, 0, 0, 0, 85, 0, 0, 0, 85, 0, 0, 0, 85, 0, 0, 0, 86, 0, 0, 0, 86, 0, 0, 0, 86, 0, 0, 0, 86, 0, 0, 0, 86, 0, 0, 0, 86, 0, 0, 0, 86, 0, 0, 0, 86, 0, 0, 0, 87, 0, 0, 0, 87, 0, 0, 0, 87, 0, 0, 0, 87, 0, 0, 0, 87, 0, 0, 0, 87, 0, 0, 0, 87, 0, 0, 0, 87, 0, 0, 0, 87, 0, 0, 0, 88, 0, 0, 0, 88, 0, 0, 0, 88, 0, 0, 0, 88, 0, 0, 0, 88, 0, 0, 0, 88, 0, 0, 0, 88, 0, 0, 0, 88, 0, 0, 0, 89, 0, 0, 0, 89, 0, 0, 0, 89, 0, 0, 0, 89, 0, 0, 0, 89, 0, 0, 0, 89, 0, 0, 0, 89, 0, 0, 0, 89, 0, 0, 0, 89, 0, 0, 0, 90, 0, 0, 0, 90, 0, 0, 0, 90, 0, 0, 0, 90, 0, 0, 0, 90, 0, 0, 0, 90, 0, 0, 0, 90, 0, 0, 0, 90, 0, 0, 0, 90, 0, 0, 0, 91, 0, 0, 0, 91, 0, 0, 0, 91, 0, 0, 0, 91, 0, 0, 0, 91, 0, 0, 0, 91, 0, 0, 0, 91, 0, 0, 0, 91, 0, 0, 0, 92, 0, 0, 0, 92, 0, 0, 0, 92, 0, 0, 0, 92, 0, 0, 0, 92, 0, 0, 0, 92, 0, 0, 0, 92, 0, 0, 0, 92, 0, 0, 0, 92, 0, 0, 0, 93, 0, 0, 0, 93, 0, 0, 0, 93, 0, 0, 0, 93, 0, 0, 0, 93, 0, 0, 0, 93, 0, 0, 0, 93, 0, 0, 0, 93, 0, 0, 0, 93, 0, 0, 0, 94, 0, 0, 0, 94, 0, 0, 0, 94, 0, 0, 0, 94, 0, 0, 0, 94, 0, 0, 0, 94, 0, 0, 0, 94, 0, 0, 0, 94, 0, 0, 0, 95, 0, 0, 0, 95, 0, 0, 0, 95, 0, 0, 0, 95, 0, 0, 0, 95, 0, 0, 0, 95, 0, 0, 0, 95, 0, 0, 0, 95, 0, 0, 0, 95, 0, 0, 0, 96, 0, 0, 0, 96, 0, 0, 0, 96, 0, 0, 0, 96, 0, 0, 0, 96, 0, 0, 0, 96, 0, 0, 0, 96, 0, 0, 0, 96, 0, 0, 0, 96, 0, 0, 0, 97, 0, 0, 0, 97, 0, 0, 0, 97, 0, 0, 0, 97, 0, 0, 0, 97, 0, 0, 0, 97, 0, 0, 0, 97, 0, 0, 0, 97, 0, 0, 0, 98, 0, 0, 0, 98, 0, 0, 0, 98, 0, 0, 0, 98, 0, 0, 0, 98, 0, 0, 0, 98, 0, 0, 0, 98, 0, 0, 0, 98, 0, 0, 0, 98, 0, 0, 0, 99, 0, 0, 0, 99, 0, 0, 0, 99, 0, 0, 0, 99, 0, 0, 0, 99, 0, 0, 0, 99, 0, 0, 0, 99, 0, 0, 0, 99

conversionun:
    .byte 0, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 0, 8, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 8, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 8, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 7, 0, 0, 0, 8, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 8, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 7, 0, 0, 0, 8, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 8, 0, 0, 0, 9, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 8, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 8, 0, 0, 0, 9, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 0, 8, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 8, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 8, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 0, 8, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 8, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 8, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 7, 0, 0, 0, 8, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 8, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 8, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 8, 0, 0, 0, 9, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 0, 8, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 8, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 8, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 0, 8, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 8, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 8, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 7, 0, 0, 0, 8, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 8, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 7, 0, 0, 0, 8, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 8, 0, 0, 0, 9, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 8, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 8, 0, 0, 0, 9, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 0, 8, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 8, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 8, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 0, 8, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 8, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 8, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 7, 0, 0, 0, 8, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 8, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 8, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 8, 0, 0, 0, 9, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 0, 8, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 8, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 8, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 0, 8, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 8, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 8, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 7, 0, 0, 0, 8, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 8, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 7, 0, 0, 0, 8, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 8, 0, 0, 0, 9, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 8, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 8, 0, 0, 0, 9, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 0, 8, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 8, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 8, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 0, 8, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 8, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 8, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 7, 0, 0, 0, 8, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 8, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 8, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 8, 0, 0, 0, 9, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 0, 8, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 8, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 8, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 0, 8, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 8, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 8, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 7, 0, 0, 0, 8, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 8, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 7, 0, 0, 0, 8, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 8, 0, 0, 0, 9, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 8, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 8, 0, 0, 0, 9, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 0, 8, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 8, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 8, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 0, 8, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 8, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 8, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 7, 0, 0, 0, 8, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 8, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 8, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 8, 0, 0, 0, 9, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 0, 8, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 8, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 8, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 0, 8, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 8, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 8, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 7, 0, 0, 0, 8, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 8, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 7, 0, 0, 0, 8, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 8, 0, 0, 0, 9, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 8, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 8, 0, 0, 0, 9, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 0, 8, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 8, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 0, 7, 0, 0, 0, 8, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 4, 0, 0, 0, 5, 0, 0, 0, 6, 0, 0, 0, 8, 0, 0, 0, 9, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 3, 0, 0, 0, 5, 0, 0, 0, 6

#.align 2
year_to_segments:
    .byte 0x06, 0x6F, 0x07, 0x3F, 0x06, 0x6F, 0x07, 0x06, 0x06, 0x6F, 0x07, 0x5B, 0x06, 0x6F, 0x07, 0x4F, 0x06, 0x6F, 0x07, 0x66, 0x06, 0x6F, 0x07, 0x6D, 0x06, 0x6F, 0x07, 0x7D, 0x06, 0x6F, 0x07, 0x07, 0x06, 0x6F, 0x07, 0x7F, 0x06, 0x6F, 0x07, 0x6F, 0x06, 0x6F, 0x7F, 0x3F, 0x06, 0x6F, 0x7F, 0x06, 0x06, 0x6F, 0x7F, 0x5B, 0x06, 0x6F, 0x7F, 0x4F, 0x06, 0x6F, 0x7F, 0x66, 0x06, 0x6F, 0x7F, 0x6D, 0x06, 0x6F, 0x7F, 0x7D, 0x06, 0x6F, 0x7F, 0x07, 0x06, 0x6F, 0x7F, 0x7F, 0x06, 0x6F, 0x7F, 0x6F, 0x06, 0x6F, 0x6F, 0x3F, 0x06, 0x6F, 0x6F, 0x06, 0x06, 0x6F, 0x6F, 0x5B, 0x06, 0x6F, 0x6F, 0x4F, 0x06, 0x6F, 0x6F, 0x66, 0x06, 0x6F, 0x6F, 0x6D, 0x06, 0x6F, 0x6F, 0x7D, 0x06, 0x6F, 0x6F, 0x07, 0x06, 0x6F, 0x6F, 0x7F, 0x06, 0x6F, 0x6F, 0x6F, 0x5B, 0x3F, 0x3F, 0x3F, 0x5B, 0x3F, 0x3F, 0x06, 0x5B, 0x3F, 0x3F, 0x5B, 0x5B, 0x3F, 0x3F, 0x4F, 0x5B, 0x3F, 0x3F, 0x66, 0x5B, 0x3F, 0x3F, 0x6D, 0x5B, 0x3F, 0x3F, 0x7D, 0x5B, 0x3F, 0x3F, 0x07, 0x5B, 0x3F, 0x3F, 0x7F, 0x5B, 0x3F, 0x3F, 0x6F, 0x5B, 0x3F, 0x06, 0x3F, 0x5B, 0x3F, 0x06, 0x06, 0x5B, 0x3F, 0x06, 0x5B, 0x5B, 0x3F, 0x06, 0x4F, 0x5B, 0x3F, 0x06, 0x66, 0x5B, 0x3F, 0x06, 0x6D, 0x5B, 0x3F, 0x06, 0x7D, 0x5B, 0x3F, 0x06, 0x07, 0x5B, 0x3F, 0x06, 0x7F, 0x5B, 0x3F, 0x06, 0x6F, 0x5B, 0x3F, 0x5B, 0x3F, 0x5B, 0x3F, 0x5B, 0x06, 0x5B, 0x3F, 0x5B, 0x5B, 0x5B, 0x3F, 0x5B, 0x4F, 0x5B, 0x3F, 0x5B, 0x66, 0x5B, 0x3F, 0x5B, 0x6D, 0x5B, 0x3F, 0x5B, 0x7D, 0x5B, 0x3F, 0x5B, 0x07, 0x5B, 0x3F, 0x5B, 0x7F, 0x5B, 0x3F, 0x5B, 0x6F, 0x5B, 0x3F, 0x4F, 0x3F, 0x5B, 0x3F, 0x4F, 0x06, 0x5B, 0x3F, 0x4F, 0x5B, 0x5B, 0x3F, 0x4F, 0x4F, 0x5B, 0x3F, 0x4F, 0x66, 0x5B, 0x3F, 0x4F, 0x6D, 0x5B, 0x3F, 0x4F, 0x7D, 0x5B, 0x3F, 0x4F, 0x07, 0x5B, 0x3F, 0x4F, 0x7F 

#.align 2
two_digits_to_segments:
    .byte 0x3F, 0x3F, 0x3F, 0x06, 0x3F, 0x5B, 0x3F, 0x4F, 0x3F, 0x66, 0x3F, 0x6D, 0x3F, 0x7D, 0x3F, 0x07, 0x3F, 0x7F, 0x3F, 0x6F, 0x06, 0x3F, 0x06, 0x06, 0x06, 0x5B, 0x06, 0x4F, 0x06, 0x66, 0x06, 0x6D, 0x06, 0x7D, 0x06, 0x07, 0x06, 0x7F, 0x06, 0x6F, 0x5B, 0x3F, 0x5B, 0x06, 0x5B, 0x5B, 0x5B, 0x4F, 0x5B, 0x66, 0x5B, 0x6D, 0x5B, 0x7D, 0x5B, 0x07, 0x5B, 0x7F, 0x5B, 0x6F, 0x4F, 0x3F, 0x4F, 0x06, 0x4F, 0x5B, 0x4F, 0x4F, 0x4F, 0x66, 0x4F, 0x6D, 0x4F, 0x7D, 0x4F, 0x07, 0x4F, 0x7F, 0x4F, 0x6F, 0x66, 0x3F, 0x66, 0x06, 0x66, 0x5B, 0x66, 0x4F, 0x66, 0x66, 0x66, 0x6D, 0x66, 0x7D, 0x66, 0x07, 0x66, 0x7F, 0x66, 0x6F, 0x6D, 0x3F, 0x6D, 0x06, 0x6D, 0x5B, 0x6D, 0x4F, 0x6D, 0x66, 0x6D, 0x6D, 0x6D, 0x7D, 0x6D, 0x07, 0x6D, 0x7F, 0x6D, 0x6F, 0x7D, 0x3F, 0x7D, 0x06, 0x7D, 0x5B, 0x7D, 0x4F, 0x7D, 0x66, 0x7D, 0x6D, 0x7D, 0x7D, 0x7D, 0x07, 0x7D, 0x7F, 0x7D, 0x6F, 0x07, 0x3F, 0x07, 0x06, 0x07, 0x5B, 0x07, 0x4F, 0x07, 0x66, 0x07, 0x6D, 0x07, 0x7D, 0x07, 0x07, 0x07, 0x7F, 0x07, 0x6F, 0x7F, 0x3F, 0x7F, 0x06, 0x7F, 0x5B, 0x7F, 0x4F, 0x7F, 0x66, 0x7F, 0x6D, 0x7F, 0x7D, 0x7F, 0x07, 0x7F, 0x7F, 0x7F, 0x6F, 0x6F, 0x3F, 0x6F, 0x06, 0x6F, 0x5B, 0x6F, 0x4F, 0x6F, 0x66, 0x6F, 0x6D, 0x6F, 0x7D, 0x6F, 0x07, 0x6F, 0x7F, 0x6F, 0x6F 

digit_to_segments:
     .byte 0x3F, 0x06, 0x5B, 0x4F, 0x66, 0x6D, 0x7D, 0x07, 0x7F, 0x6F

# --- TOUTE LA FIN EST À COMMENTER LORS DES TESTS FINAUX ---
#timestamp:
    #.word 1327345992

#.align 2
    #.byte 0x00, 0x00
#clock_display:
    #.byte 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42, 0x42
