% Tools 

% % % Dictionary Tools
find_dict(Key, Value, Dict) :-
    member(Key:Value, Dict).

del_dict(_, [], []).
del_dict(Key, [_:Key|T], L) :- !, del_dict(Key, T, L).
del_dict(Key, [X:Y|T], [X:Y|L]) :-
    Y \= Key,
    del_dict(Key, T, L).

set_dict(Key, Value, Dict, FinalDict) :-
    del_dict(Key, Dict, NewDict),
    concat([Key:Value], NewDict, FinalDict).

add(L, 0, _, L).
add(L,K,X,R) :-
    K>0,
    P is K-1,
    concat(L, [X], L1),
    add(L1, P, X, R).

enumerate([], _, []).
enumerate([E1|List], Number, [E1-Number|R]) :-
    Number1 is Number+1,
    enumerate(List, Number1, R).


% Players

list_colors([blue, red, yellow, black, white]).

strategies([basic, greedy]).

select_strategy(Strategy):-
    strategies(Strategies),
    random_permutation(Strategies, [Strategy|_]).

generate_floor([-1,-1,-2,-2,-2,-3,-3]:penalties).

% Genera tablero en escalera para fase de preparación
generate_board(Board):-
    % Genera una lista de listas de longitud 5
    add([], 5, 1, List),
    enumerate(List, 1, Enum),
    list_colors(C),

    % asigna los colores válidos a cada fila
    findall([New:stocks, C:valid, C:all]:Sz,
        (find_dict(Sz, Enum, _),
        add([], Sz, empty, New)
    ), Data).

% Genera todos los jugadores del juego y les asigna sus tableros
genereate_players(N,Players:players) :-
    % El tablero en forma de escalera donde se almacenan las piezas
    % durante la fase de preparación
    generate_board(Board),
    % Tablero de penalización
    generate_floor(Floor),
    % Asigna a cada jugador un tablero de cada tipo, además creal el muro 
    % y define el score = 0
    add([], N, [Board, Penalties, []:wall, 0:score], List),
    findall(P, (
        member(P, List),
        select_strategy(S),
        set_dict(strategy, X,S,P)
    ), UnorderedPlayers),
    % Ordena los jugadores
    enumerate(UnorderedPlayers, 1, Players).

basic(Game, Player, NewGame, NewPlayer, A) :- 
    valid_moves(Game, Player, [A|_]),
    update_player(Player, Game, A, NewPlayer, Return, _),
    update_game(Game, A, NewGame, Return).
basic(Game, Player, NewGame, NewPlayer, none:Id:Color) :- 
    valid_moves(Game, none:Id:Col
% Si no quedan Jugadores por Jugar termina la ronda
run_round(Game, [], Game, []).
run_round(Game, [Player:Id| Players], NewGame, [Id:FId| Events]) :-
    % Detecta estrategia del jugador
    find_dict(strategy, Player, Strategy),
    % TODO Print Player info

    % Ejecuta la estrategia utilizando el tablero del jugador y las fabricas que hay disponibles 
    Move =..[Strategy, Game, Player, TempGame, NewPlayer, LId:FID:Color],
    % TODO Print Move

    % Obtiene el estado de las fabricas despues de la jugada
    find_dict(factories, TempGame, Factories),
    % TODO Print Factories info and Preparation zone for Player

    % Actualiza el tablero del jugador
    find_dict(players, TempGame, PrevPlayers),
    set_dict(Id, PrevPlayers, NewPlayer, CurrPlayers),
    set_dict(players,TempGame, CurrPlayers, NewTempGame),

    % Cede el turno al siguiente jugador 
    run_round(NewTempGame, Players, NewGame, Events).
