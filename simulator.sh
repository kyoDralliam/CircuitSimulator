#!/bin/sh

if [ ! -f obsidian ];
then echo -e "Erreur : le compilateur n'est pas compilé." 1>&2;
exit 1;
fi

if [ -e $1.oit ];
then echo -e "Erreur : le fichier $1.out existe." 1>&2;
exit 1;
fi

./obsidian $1 -o $1.out
./$1.out -s $2
rm -f $1.out