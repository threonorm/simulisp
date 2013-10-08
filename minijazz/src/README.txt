****  Compilation ******

Pour compiler MiniJazz, utilisez la commande suivante:
  > ocamlbuild mjc.byte

**** Utilisation *********

Il suffit de lancer le compilateur en lui donnant un fichier .mj:
  > ./mjc.byte test/nadder.mj
Cela genere un fichier test/nadder.net contennat une net-list non ordonnee.

Pour obtenir les options du compilateur, utilisez l'option '-h':
  > ./mjc.byte -h
Une option importante est -m qui doit etre suivi du nom du bloc principal du circuit.
Par defaut, le compilateur recherche un bloc appele 'main'


Pour la description du langage MiniJazz et du langage de net-list, voir le sujet du TP1.
