# Auteurs
DIAMANT Alexandre, @diamant, 21952751
MANYIM Olivier, @manyim, 21959946

# Fonctionnalités

- Construction des permutations des cartes en fonction de la graine donnée en paramètre
- L'utilisateur peut utilisé -check pour ouvrir lire le contenu d'un fichier solution et vérifier si la solution est correcte
    - Cette fonctionnalité ne fonctionne pas corectement. La normalisation n'est pas complete: Si on normalise les colonnes et qu'on envoie des cartes dans le dépots alors on doit refaire un tour de normalisation dans les colonnes mais aussi dans les registres et cette dernière étape n'est pas faite. Symétriquement pareil dans le cas de la normalisation des registres.
- L'utilisateur peut utiliser -search pour lancer la recherche de solution mais la recherche de solution n'est pas implémenté.

# Compilation et exécution
#### Compilation
`make` pour compiler le projet
`make clean` pour effacer le répertoire provisoire `_build` produit par `dune` lors de ses compilations.
#### Execution
`./run <game>.<seed> ([-check | -search] <path_to_filename>.sol)` pour executer le projet

##### Execution des tests
`dune runtest tests\I` pour executer les tests de la partie I
Les tests de la partie II n'ont pas a être executer car cette partie n'est pas implémenté.

# Découpage modulaire

- `Card.ml` : définition des cartes
- `Fifo.ml`: structure "First in First out" pour la création des permutations
- `XpatRandom.ml` : création de la permutation des cartes en fonction de la graine
- `XpatSolver.ml` : configuration des différentes structures de données (colonnes, registres, dépots) et implémentation des fonctions de normalisation, de coup et de recherche de solution

# Organisation du travail

- Les premiers commits avant le premier jalon ont été fait par les deux membres du groupe.
- Les commits après le premier jalon ont été fait par Alexandre Diamant.

#### Chronologie
- Commit effectué pour le premier jalon puis reprise tardive du projet avec commit quelques jours avant le rendu final.