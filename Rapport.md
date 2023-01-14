# Compilateur-Ocaml

Le lexer, le parser: 
•    complets 

 TypeChecker :
•    Il manque dans le typechecker le typages du struct avec son get et son set

 Interprète :
•    Toutes les fonction ont été réalisées
•    La gestion de la mémoire est réalisé
•    Les cas des structures ont étés traités n’ont pas étés teste car ils ne sont pas fait dans le type checker
•    L’extension de l’Egalite structurelle est ajouté
•    Les opérations binaire et conditions ont étés testes

Difficultés : 
•    La contrainte du temps a été la plus grande difficulté, en effet on devait gérer entre notre deuxième filière (mathématiques) et le job étudiant effectué à côté, et le fait que ça soit tombé en période de contrôles et d’examens.
•    La difficulté technique rencontré réside dans les cas des structures dans le type checker et dans l’interprète pour gérer le cas des fonction récursives.
•    La troisième difficulté est la gestion des fichiers ensemble et le repère d’erreurs.
•    Nous avons aussi eu du mal à débuger et trouver d'où venait certaines erreur

Organisation du travail:
•    Nous avons travaillé fichier par fichier, donc lexer, puis parser, puis typechecker et enfin interprete. Avec du recul, nous pensons que ca aurait été mieux de travailler theme par theme: arithmétique, puis variables,...
