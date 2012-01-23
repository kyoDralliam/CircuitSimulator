# Multiplication et division n'utilisant que l'addition, la soustraction, et le
# décalage à gauche ou à droite.
# La multiplication se comporte bien sur des nombres négatifs, la division
# échoue lamentablement. Par ailleurs, aucun test n'est fait pour vérifier que
# l'on ne divise pas par 0, l'algorithme part en boucle infinie si c'est le cas.

    .text

main:
        # On sauvegarde $ra
        addi    $sp, $sp, 4
        sw      $ra, 0($sp)

        # On récupère le timestamp que l'on met dans $s7
        #lw      $s7, timestamp
        li      $s7, 1329500537

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
        
        # Calcul de l'heure
        move    $a0, $s1
        li      $a1, 3600
        jal     diviser
        move    $s2, $v0 # $s2 = Heures
            move    $a0, $s2
            jal print_int
        move    $a0, $s2
        li      $a1, 3600
        jal     multiplier
        sub     $s1, $s1, $v0 # $s1 = "reste"

        move    $a0, $s1
        li      $a1, 60
        jal     diviser
        move    $s3, $v0 # $s3 = Minutes
            move    $a0, $s3
            jal print_int

        move    $a0, $s3
        li      $a1, 60
        jal     multiplier
        sub     $s4, $s1, $v0 # $s4 = Secondes
            move    $a0, $s4
            jal print_int
        
        # ---- Calcul de la date ----
        # On commence par rajouter un décalage pour les années bissextiles
        # Dans le cas absurde où on se trouverait avant le premier décalage, on
        # ne fait rien du tout
        li      $t0, 68256000
        bgt     $t0, $s7, antibug
        # On veut donc savoir combien de 29 févriers on doit rajouter
        # artificiellement (information mise dans $s1). On commence par mettre
        # le point de départ du comptage le 1er Mars 1970
        addi    $s6, $s0, -59 # 31+28
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
        li      $s6, 1970
        move    $a0, $s0
        li      $a1, 366
        jal     diviser
        add     $s6, $s6, $v0 # $s6 = Année
            move    $a0, $s6
            jal print_int

        # On trouve le mois en mode "gros sac"
        move    $a0, $s6
        addi    $a0, $a0, -1970
        li      $a1, 366
        jal     multiplier
        sub     $s4, $s0, $v0 # $s4 = Nombre de jours depuis le début de l'année
        li      $s5, 1 # $s5 = Mois
        li      $t1, 0
        li      $t0, 32 # Janvier + 1 (à cause de la comparaison stricte)
        bgt     $t0, $s4, moistrouve
        addi    $s5, $s5, 1
        move    $t1, $t0
        li      $t4, 1
        sub     $t1, $t1, $t4
        addi    $t0, $t0, 29 # Février
        bgt     $t0, $s4, moistrouve
        addi    $s5, $s5, 1
        move    $t1, $t0
        li      $t4, 1
        sub     $t1, $t1, $t4
        addi    $t0, $t0, 31 # Mars
        bgt     $t0, $s4, moistrouve
        addi    $s5, $s5, 1
        move    $t1, $t0
        li      $t4, 1
        sub     $t1, $t1, $t4
        addi    $t0, $t0, 30 # Avril
        bgt     $t0, $s4, moistrouve
        addi    $s5, $s5, 1
        move    $t1, $t0
        li      $t4, 1
        sub     $t1, $t1, $t4
        addi    $t0, $t0, 31 # Mai
        bgt     $t0, $s4, moistrouve
        addi    $s5, $s5, 1
        move    $t1, $t0
        li      $t4, 1
        sub     $t1, $t1, $t4
        addi    $t0, $t0, 30 # Juin
        bgt     $t0, $s4, moistrouve
        addi    $s5, $s5, 1
        move    $t1, $t0
        li      $t4, 1
        sub     $t1, $t1, $t4
        addi    $t0, $t0, 31 # Juillet
        bgt     $t0, $s4, moistrouve
        addi    $s5, $s5, 1
        move    $t1, $t0
        li      $t4, 1
        sub     $t1, $t1, $t4
        addi    $t0, $t0, 31 # Août
        bgt     $t0, $s4, moistrouve
        addi    $s5, $s5, 1
        move    $t1, $t0
        li      $t4, 1
        sub     $t1, $t1, $t4
        addi    $t0, $t0, 30 # Septembre
        bgt     $t0, $s4, moistrouve
        addi    $s5, $s5, 1
        move    $t1, $t0
        li      $t4, 1
        sub     $t1, $t1, $t4
        addi    $t0, $t0, 31 # Octobre
        bgt     $t0, $s4, moistrouve
        addi    $s5, $s5, 1
        move    $t1, $t0
        li      $t4, 1
        sub     $t1, $t1, $t4
        addi    $t0, $t0, 30 # Novembre
        bgt     $t0, $s4, moistrouve
        addi    $s5, $s5, 1
        move    $t1, $t0
        li      $t4, 1
        sub     $t1, $t1, $t4
    moistrouve:
            move    $a0, $s5 # $s5 = Mois
            jal print_int
        sub     $s4, $s4, $t1
            move    $a0, $s4 # $s4 = Jour
            jal print_int
        
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
        lw      $ra, 0($sp)
        addi    $sp, $sp, -4
        jr      $ra

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
        srl     $a1, $a1, $t0
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
        j       diviser_petiteboucle
    diviser_finpetiteboucle:
        # On est allés un cran trop loin, on corrige ça
        # (nb : il doit y avoir une méthode plus propre, mais là j'ai la flemme)
        sra     $t1, $t1, 1
        sra     $t2, $t2, 1
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

