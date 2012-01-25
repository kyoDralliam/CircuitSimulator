# Multiplication et division n'utilisant que l'addition, la soustraction, et le
# décalage à gauche ou à droite.
# La multiplication ne se comporte probablement pas bien sur des nombres
# négatifs, la division échoue lamentablement. Par ailleurs, aucun test n'est
# fait pour vérifier que l'on ne divise pas par 0, l'algorithme part en boucle
# infinie si c'est le cas.

    .text
        li $t6, 0
main:

        li      $sp, 8000
        # On sauvegarde $ra
        addi    $sp, $sp, 4
        sw      $ra, 0($sp)

        li      $gp, 0

attendre:    
        beq $t6, $zero, recalculer
        # On récupère le timestamp que l'on met dans $s7
        li      $t0, timestamp
        lw      $s7, 0($t0)
        beq     $t7, $s7, attendre
        
recalculer:     

        # On récupère le timestamp que l'on met dans $s7
        li      $t0, timestamp
        lw      $s7, 0($t0)
        #li      $s7, 1327338988

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
            lb      $t1, 0($t0)
            la      $t2, clock_display
            sb      $t1, 5($t2)

        # Si c'est la première fois que l'on passe par là, on force le calcul de la date
        beq     $gp, $zero, date

        # Si on n'est pas 00:00:00, on recalcule l'heure sans recalculer la date
        bne     $fp, $zero, attendre
    
    date:
        li      $gp, 42
        # ---- Calcul de la date ----
        # On commence par rajouter un décalage pour les années bissextiles
        # Dans le cas absurde où on se trouverait avant le premier décalage, on
        # ne fait rien du tout
        li      $t0, 68256000
        bgt     $t0, $s7, finbissextile
        # On veut donc savoir combien de 29 févriers on doit rajouter
        # artificiellement (information mise dans $s1). On commence par mettre
        # le point de départ du comptage le 1er Mars 1970
        addi    $s6, $s0, -58 # 31+28-1
        li      $s1, 0
        move    $a0, $s6
        li      $a1, 1461 # 4*365 + 1
        jal     diviser
        move    $s5, $v0 # $s5 = Nombre de paquets de 4 ans
        move    $a0, $v0
        li      $a1, 1461
        jal     multiplier
        sub     $s6, $s6, $v0 # $s6 = "reste"
        move    $a0, $s5
        li      $a1, 3 # 3 corrections en 4 ans
        jal multiplier
        add     $s1, $s1, $v0 # Corrections par paquets de 4 ans
        addi    $s1, $s1, 1
        li      $t0, 366
        bgt     $t0, $s6, finbissextile
        addi    $s1, $s1, 1
        li      $t0, 1097
        bgt     $t0, $s6, finbissextile
        addi    $s1, $s1, 1
    finbissextile:
        # 2000 n'est pas une année bissextile
        li      $t0, 951868799
        bgt     $t0, $s7, antibug
        addi    $s1, $s1, 1
    antibug:
        # Et on effectue la correction calculée dans $s1
        add     $s0, $s0, $s1 # Rappel // $s0 = Jours depuis le 01/01/1970

        # Maintenant on a facilement l'année
        li      $s6, 0
        move    $a0, $s0
        li      $a1, 366
        jal     diviser
        move    $s6, $v0 # $s6 = Année
            move    $a0, $s6

        # On trouve le mois en mode "gros sac"
        move    $a0, $s6
        li      $a1, 366
        jal     multiplier
        sub     $s4, $s0, $v0 # $s4 = Nombre de jours depuis le début de l'année
        li      $s5, 1 # $s5 = Mois
        li      $t1, 0
        li      $t0, 31 # Janvier + 1 (à cause de la comparaison stricte)
        bgt     $t0, $s4, moistrouve
        addi    $s5, $s5, 1
        move    $t1, $t0
        addi    $t0, $t0, 29 # Février
        bgt     $t0, $s4, moistrouve
        addi    $s5, $s5, 1
        move    $t1, $t0
        addi    $t0, $t0, 31 # Mars
        bgt     $t0, $s4, moistrouve
        addi    $s5, $s5, 1
        move    $t1, $t0
        addi    $t0, $t0, 30 # Avril
        bgt     $t0, $s4, moistrouve
        addi    $s5, $s5, 1
        move    $t1, $t0
        addi    $t0, $t0, 31 # Mai
        bgt     $t0, $s4, moistrouve
        addi    $s5, $s5, 1
        move    $t1, $t0
        addi    $t0, $t0, 30 # Juin
        bgt     $t0, $s4, moistrouve
        addi    $s5, $s5, 1
        move    $t1, $t0
        addi    $t0, $t0, 31 # Juillet
        bgt     $t0, $s4, moistrouve
        addi    $s5, $s5, 1
        move    $t1, $t0
        addi    $t0, $t0, 31 # Août
        bgt     $t0, $s4, moistrouve
        addi    $s5, $s5, 1
        move    $t1, $t0
        addi    $t0, $t0, 30 # Septembre
        bgt     $t0, $s4, moistrouve
        addi    $s5, $s5, 1
        move    $t1, $t0
        addi    $t0, $t0, 31 # Octobre
        bgt     $t0, $s4, moistrouve
        addi    $s5, $s5, 1
        move    $t1, $t0
        addi    $t0, $t0, 30 # Novembre
        bgt     $t0, $s4, moistrouve
        addi    $s5, $s5, 1
        move    $t1, $t0
    moistrouve:
            move    $a0, $s5 # $s5 = Mois
        addi    $t1, $t1, -1
        sub     $s4, $s4, $t1
            move    $a0, $s4 # $s4 = Jour

            # Affichage de la date
            # Jour
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
            add     $t0, $s6, $s6
            add     $t0, $t0, $t0
            li      $t5, year_to_segments 
            add     $t0, $t0, $t5
            lw      $t1, 0($t0)
            la      $t2, clock_display
            addi    $t2, $t2, 10
            sw      $t1, 0($t2)

        
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
#ebfe:
#        j ebfe
        # On récupère $ra
        #lw      $ra, 0($sp)
        #addi    $sp, $sp, -4
        #jr      $ra
        j attendre
        
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



	.data
newline:
	.asciiz "\n"

conversiondeux:
    .word 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7, 7, 7, 7, 8, 8, 8, 8, 8, 8, 8, 8, 9, 9, 9, 9, 9, 9, 9, 9, 9, 10, 10, 10, 10, 10, 10, 10, 10, 10, 11, 11, 11, 11, 11, 11, 11, 11, 12, 12, 12, 12, 12, 12, 12, 12, 12, 13, 13, 13, 13, 13, 13, 13, 13, 14, 14, 14, 14, 14, 14, 14, 14, 14, 15, 15, 15, 15, 15, 15, 15, 15, 15, 16, 16, 16, 16, 16, 16, 16, 16, 17, 17, 17, 17, 17, 17, 17, 17, 17, 18, 18, 18, 18, 18, 18, 18, 18, 18, 19, 19, 19, 19, 19, 19, 19, 19, 20, 20, 20, 20, 20, 20, 20, 20, 20, 21, 21, 21, 21, 21, 21, 21, 21, 21, 22, 22, 22, 22, 22, 22, 22, 22, 23, 23, 23, 23, 23, 23, 23, 23, 23, 24, 24, 24, 24, 24, 24, 24, 24, 25, 25, 25, 25, 25, 25, 25, 25, 25, 26, 26, 26, 26, 26, 26, 26, 26, 26, 27, 27, 27, 27, 27, 27, 27, 27, 28, 28, 28, 28, 28, 28, 28, 28, 28, 29, 29, 29, 29, 29, 29, 29, 29, 29, 30, 30, 30, 30, 30, 30, 30, 30, 31, 31, 31, 31, 31, 31, 31, 31, 31, 32, 32, 32, 32, 32, 32, 32, 32, 32, 33, 33, 33, 33, 33, 33, 33, 33, 34, 34, 34, 34, 34, 34, 34, 34, 34, 35, 35, 35, 35, 35, 35, 35, 35, 35, 36, 36, 36, 36, 36, 36, 36, 36, 37, 37, 37, 37, 37, 37, 37, 37, 37, 38, 38, 38, 38, 38, 38, 38, 38, 39, 39, 39, 39, 39, 39, 39, 39, 39, 40, 40, 40, 40, 40, 40, 40, 40, 40, 41, 41, 41, 41, 41, 41, 41, 41, 42, 42, 42, 42, 42, 42, 42, 42, 42, 43, 43, 43, 43, 43, 43, 43, 43, 43, 44, 44, 44, 44, 44, 44, 44, 44, 45, 45, 45, 45, 45, 45, 45, 45, 45, 46, 46, 46, 46, 46, 46, 46, 46, 46, 47, 47, 47, 47, 47, 47, 47, 47, 48, 48, 48, 48, 48, 48, 48, 48, 48, 49, 49, 49, 49, 49, 49, 49, 49, 50, 50, 50, 50, 50, 50, 50, 50, 50, 51, 51, 51, 51, 51, 51, 51, 51, 51, 52, 52, 52, 52, 52, 52, 52, 52, 53, 53, 53, 53, 53, 53, 53, 53, 53, 54, 54, 54, 54, 54, 54, 54, 54, 54, 55, 55, 55, 55, 55, 55, 55, 55, 56, 56, 56, 56, 56, 56, 56, 56, 56, 57, 57, 57, 57, 57, 57, 57, 57, 57, 58, 58, 58, 58, 58, 58, 58, 58, 59, 59, 59, 59, 59, 59, 59, 59, 59, 60, 60, 60, 60, 60, 60, 60, 60, 60, 61, 61, 61, 61, 61, 61, 61, 61, 62, 62, 62, 62, 62, 62, 62, 62, 62, 63, 63, 63, 63, 63, 63, 63, 63, 64, 64, 64, 64, 64, 64, 64, 64, 64, 65, 65, 65, 65, 65, 65, 65, 65, 65, 66, 66, 66, 66, 66, 66, 66, 66, 67, 67, 67, 67, 67, 67, 67, 67, 67, 68, 68, 68, 68, 68, 68, 68, 68, 68, 69, 69, 69, 69, 69, 69, 69, 69, 70, 70, 70, 70, 70, 70, 70, 70, 70, 71, 71, 71, 71, 71, 71, 71, 71, 71, 72, 72, 72, 72, 72, 72, 72, 72, 73, 73, 73, 73, 73, 73, 73, 73, 73, 74, 74, 74, 74, 74, 74, 74, 74, 75, 75, 75, 75, 75, 75, 75, 75, 75, 76, 76, 76, 76, 76, 76, 76, 76, 76, 77, 77, 77, 77, 77, 77, 77, 77, 78, 78, 78, 78, 78, 78, 78, 78, 78, 79, 79, 79, 79, 79, 79, 79, 79, 79, 80, 80, 80, 80, 80, 80, 80, 80, 81, 81, 81, 81, 81, 81, 81, 81, 81, 82, 82, 82, 82, 82, 82, 82, 82, 82, 83, 83, 83, 83, 83, 83, 83, 83, 84, 84, 84, 84, 84, 84, 84, 84, 84, 85, 85, 85, 85, 85, 85, 85, 85, 85, 86, 86, 86, 86, 86, 86, 86, 86, 87, 87, 87, 87, 87, 87, 87, 87, 87, 88, 88, 88, 88, 88, 88, 88, 88, 89, 89, 89, 89, 89, 89, 89, 89, 89, 90, 90, 90, 90, 90, 90, 90, 90, 90, 91, 91, 91, 91, 91, 91, 91, 91, 92, 92, 92, 92, 92, 92, 92, 92, 92, 93, 93, 93, 93, 93, 93, 93, 93, 93, 94, 94, 94, 94, 94, 94, 94, 94, 95, 95, 95, 95, 95, 95, 95, 95, 95, 96, 96, 96, 96, 96, 96, 96, 96, 96, 97, 97, 97, 97, 97, 97, 97, 97, 98, 98, 98, 98, 98, 98, 98, 98, 98, 99, 99, 99, 99, 99, 99, 99, 99, 100, 100, 100, 100, 100, 100, 100, 100, 100, 101, 101, 101, 101, 101, 101, 101, 101, 101, 102, 102, 102, 102, 102, 102, 102, 102, 103, 103, 103, 103, 103, 103, 103, 103, 103, 104, 104, 104, 104, 104, 104, 104, 104, 104, 105, 105, 105, 105, 105, 105, 105, 105, 106, 106, 106, 106, 106, 106, 106, 106, 106, 107, 107, 107, 107, 107, 107, 107, 107, 107, 108, 108, 108, 108, 108, 108, 108, 108, 109, 109, 109, 109, 109, 109, 109, 109, 109, 110, 110, 110, 110, 110, 110, 110, 110, 110, 111, 111, 111, 111, 111, 111, 111, 111, 112, 112, 112, 112, 112, 112, 112, 112, 112, 113, 113, 113, 113, 113, 113, 113, 113, 114, 114, 114, 114, 114, 114, 114, 114, 114, 115, 115, 115, 115, 115, 115 

conversionun:
    .word 0, 1, 2, 3, 4, 5, 6, 8, 9, 0, 1, 2, 3, 5, 6, 7, 8, 9, 0, 1, 3, 4, 5, 6, 7, 8, 0, 1, 2, 3, 4, 5, 7, 8, 9, 0, 1, 2, 3, 5, 6, 7, 8, 9, 0, 2, 3, 4, 5, 6, 7, 9, 0, 1, 2, 3, 4, 5, 7, 8, 9, 0, 1, 2, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5, 6, 7, 9, 0, 1, 2, 3, 4, 6, 7, 8, 9, 0, 1, 3, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5, 6, 8, 9, 0, 1, 2, 3, 5, 6, 7, 8, 9, 0, 1, 3, 4, 5, 6, 7, 8, 0, 1, 2, 3, 4, 5, 6, 8, 9, 0, 1, 2, 3, 5, 6, 7, 8, 9, 0, 2, 3, 4, 5, 6, 7, 8, 0, 1, 2, 3, 4, 5, 7, 8, 9, 0, 1, 2, 4, 5, 6, 7, 8, 9, 0, 2, 3, 4, 5, 6, 7, 9, 0, 1, 2, 3, 4, 6, 7, 8, 9, 0, 1, 2, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5, 6, 8, 9, 0, 1, 2, 3, 4, 6, 7, 8, 9, 0, 1, 3, 4, 5, 6, 7, 8, 0, 1, 2, 3, 4, 5, 6, 8, 9, 0, 1, 2, 3, 5, 6, 7, 8, 9, 0, 1, 3, 4, 5, 6, 7, 8, 0, 1, 2, 3, 4, 5, 7, 8, 9, 0, 1, 2, 3, 5, 6, 7, 8, 9, 0, 2, 3, 4, 5, 6, 7, 9, 0, 1, 2, 3, 4, 5, 7, 8, 9, 0, 1, 2, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5, 6, 7, 9, 0, 1, 2, 3, 4, 6, 7, 8, 9, 0, 1, 3, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5, 6, 8, 9, 0, 1, 2, 3, 5, 6, 7, 8, 9, 0, 1, 3, 4, 5, 6, 7, 8, 0, 1, 2, 3, 4, 5, 6, 8, 9, 0, 1, 2, 3, 5, 6, 7, 8, 9, 0, 2, 3, 4, 5, 6, 7, 8, 0, 1, 2, 3, 4, 5, 7, 8, 9, 0, 1, 2, 4, 5, 6, 7, 8, 9, 0, 2, 3, 4, 5, 6, 7, 9, 0, 1, 2, 3, 4, 6, 7, 8, 9, 0, 1, 2, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5, 6, 8, 9, 0, 1, 2, 3, 4, 6, 7, 8, 9, 0, 1, 3, 4, 5, 6, 7, 8, 0, 1, 2, 3, 4, 5, 6, 8, 9, 0, 1, 2, 3, 5, 6, 7, 8, 9, 0, 1, 3, 4, 5, 6, 7, 8, 0, 1, 2, 3, 4, 5, 7, 8, 9, 0, 1, 2, 3, 5, 6, 7, 8, 9, 0, 2, 3, 4, 5, 6, 7, 9, 0, 1, 2, 3, 4, 5, 7, 8, 9, 0, 1, 2, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5, 6, 7, 9, 0, 1, 2, 3, 4, 6, 7, 8, 9, 0, 1, 3, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5, 6, 8, 9, 0, 1, 2, 3, 5, 6, 7, 8, 9, 0, 1, 3, 4, 5, 6, 7, 8, 0, 1, 2, 3, 4, 5, 6, 8, 9, 0, 1, 2, 3, 5, 6, 7, 8, 9, 0, 2, 3, 4, 5, 6, 7, 8, 0, 1, 2, 3, 4, 5, 7, 8, 9, 0, 1, 2, 4, 5, 6, 7, 8, 9, 0, 2, 3, 4, 5, 6, 7, 9, 0, 1, 2, 3, 4, 6, 7, 8, 9, 0, 1, 2, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5, 6, 8, 9, 0, 1, 2, 3, 4, 6, 7, 8, 9, 0, 1, 3, 4, 5, 6, 7, 8, 0, 1, 2, 3, 4, 5, 6, 8, 9, 0, 1, 2, 3, 5, 6, 7, 8, 9, 0, 1, 3, 4, 5, 6, 7, 8, 0, 1, 2, 3, 4, 5, 7, 8, 9, 0, 1, 2, 3, 5, 6, 7, 8, 9, 0, 2, 3, 4, 5, 6, 7, 9, 0, 1, 2, 3, 4, 5, 7, 8, 9, 0, 1, 2, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5, 6, 7, 9, 0, 1, 2, 3, 4, 6, 7, 8, 9, 0, 1, 3, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5, 6, 8, 9, 0, 1, 2, 3, 5, 6, 7, 8, 9, 0, 1, 3, 4, 5, 6, 7, 8, 0, 1, 2, 3, 4, 5, 6, 8, 9, 0, 1, 2, 3, 5, 6, 7, 8, 9, 0, 2, 3, 4, 5, 6, 7, 8, 0, 1, 2, 3, 4, 5, 7, 8, 9, 0, 1, 2, 4, 5, 6, 7, 8, 9, 0, 2, 3, 4, 5, 6, 7, 9, 0, 1, 2, 3, 4, 6, 7, 8, 9, 0, 1, 2, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5, 6, 8, 9, 0, 1, 2, 3, 4, 6, 7, 8, 9, 0, 1, 3, 4, 5, 6, 7, 8, 0, 1, 2, 3, 4, 5, 6, 8, 9, 0, 1, 2, 3, 5, 6, 7, 8, 9, 0, 1, 3, 4, 5, 6, 7, 8, 0, 1, 2, 3, 4, 5, 7, 8, 9, 0, 1, 2, 3, 5, 6, 7, 8, 9, 0, 2, 3, 4, 5, 6, 7, 9, 0, 1, 2, 3, 4, 5, 7, 8, 9, 0, 1, 2, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5, 6, 7, 9, 0, 1, 2, 3, 4, 6, 7, 8, 9, 0, 1, 3, 4, 5, 6, 7, 8, 9, 1, 2, 3, 4, 5, 6, 8, 9, 0, 1, 2, 3, 5, 6, 7, 8, 9, 0, 1, 3, 4, 5, 6, 7, 8, 0, 1, 2, 3, 4, 5, 6, 8, 9, 0, 1, 2, 3, 5, 6

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
