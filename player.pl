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

select_strategy([basic, greedy, finish]).

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









