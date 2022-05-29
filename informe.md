# Programación declarativa
## Proyecto Prolog -  Juego Azul

- Javier A. Oramas López C-212
- Daniel A. Cárdenas Cabrera C-213
  
## Estructuras Principales

### Player 

1. Wall : Lista que contiene las coordenadas de las baldosas del muro del jugador
2. Strategy : Estrategia del jugador
3. Score : Puntuación del jugador
4. Penalizations : Lista de penalizaciones pendientes por aplicar al jugador
5. Board : tablero del jugador, zona de preparación

### Game

Contiene todos los elementos necesarios para el desarrollo del juego

1. Players : Lista de jugadores que intervienen en la partida
2. Factories : Lista de las factories del juego, Center será una factory especial que contendrá las baldosas en el centro
3. Outs : Cantidad de piezas de cada color que están fuera del juego
4. Ammount : Cantidad de piezas de cada color que están en la bolsa.

### Line 
Stocks : Cantidad de baldosas en una linea de la zona de preparación
Valid : Lista con los colores que acepta la linea
All : Colores que no se han colocado en la linea durante el juego.

## Estrategias para los jugadores
1. First : Toma la lista de posibles jugadas y ejecuta la primera en la lista sin revisar ni hacer más nada
2. Random : Similar a First solo con la particularidad que antes de seleccionar una jugada se ordena de manera aleatoria la lista de posibles jugadas
3. Greedy : Toma todas las posibles jugadas, las simula, termina ejecutando la que maximizó el score y/o la que minimizó la penalización.

## Ejecución
Para la ejecución del juego es necesario tener instalado swipl
correr el siguiente comando:
`swipl game.pl`
una vez dentro del interprete, se puede ejecutar una simuación de juego de la siguiente Línea

`?- start_game(P,F)`

Importante remarcar que hay que darle valores a P y F, (2,5),(3,7) etc...
de lo contrario la simulación fallará

el log de lo ocurrido en el juego se guardará en log.txt.

