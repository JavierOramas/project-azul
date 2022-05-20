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

% % % Others
count(L, X, N) :-
    findall(1,member(X,L), K),
    length(K, N).

% Sort list by index
sort_by_index(L, R) :-
    findall(X:Y, find_dict(X, L, Y), L1),
    sort(L1, R1),
    findall(X:Y, find_dict(X, R1, Y), R).

% Reemplaza las primeras K ocurrencias en L de V por X en R
% Si no hay mas que reemplazar, se queda igual
replace(L, 0, _, _, L) :- !.
% Si no hay mas ocurrencias de V en L, se queda igual
replace(L, _, V, _, L) :-
    not(member(V, L)), !.
% Si quedan caracteres por reemplazar
replace(L, K, V, X, R) :-
    K>0,
    P is K-1,
    % separa la lista en dos partes partiendo de la primera oocurrencia de V
    append(A, [V|B], L ), !,
    % manda a reemplazar en la cola
    replace(B, P, V, X, R1),
    % une los resultados
    append(A, [X|R1], R).

% lista de colores
list_colors([blue, red, yellow, black, white]).

% lista de estrategias
strategies([basic, greedy]).

% Selecciona una estrategia aleatoria
select_strategy(Strategy):-
    strategies(Strategies),
    random_permutation(Strategies, [Strategy|_]).

% Genera el Piso del juego
generate_floor([-1,-1,-2,-2,-2,-3,-3]:penalizations).

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

% Game

penalize(Player, Penalization, NewPlayer) :-
    Penalization < 0,
    find_dict(penalizations, Player, Penalizations),
    length(Penalizations, N),
    N > 0, !,
    append([Penalization], R, Penalizations),
    % TODO Print penalization for Player

    set_dict(penalizations, Player, R, TempPlayer).
    find_dict(score, Player, Score),
    UpdatedScore is Score + Penalization,
    set_dict(score, TempPlayer, UpdatedScore, TempPlayer2),
    PnalizationTimes is Penalization+1,
    penalize(TempPlayer2, PnalizationTimes, NewPlayer).
penalize(Player, _, Player).

update_score(Player, (Line,Color), NewPlayer) :-
    find_dict(board, Player, Board),
    find_dict(Line, Board, LineData),
    find_dict(stocks, LineData, Stocks),
    count(Stocks, empty, 0), !,
    tile_score(Player, (Line,Color), Score),
    find_dict(score, Player, PlayerScore),
    Sum is PlayerScore + Score,
    update_board(Player, (Line,Color), CurrPlayer),
    set_dict(score, CurrPlayer, Sum, NewPlayer).
update_score(Player, _, Player).

update_line(Player, Game, Line:Factory:Color, NewPlayer, TilesOut, TilesPenalty) :-
    find_dict(factories, Game, Factories),
    find_dict(Factory, Factories, FactoryData),
    find_dict(board, Player, Board),
    find_dict(Line, Board, LineData),
    find_dict(stocks, LineData, Stocks),
    count(Stocks, empty, EmptyCount),
    count(FactoryData, Color, ColorCount),
    replace(Stocks, ColorCount, empty, Color, NewStocks),
    TilesOut is min(EmptyCount-ColorCount, 0),
    TilesPenalty is min(NewEmpty-1, 0)* -(Line -1),
    % TODO print Player Preparation zone
    set_dict(stocks, LineData, NewStocks, NewLineData),
    set_dict(valid, NewLineData, [Color], ValidLine),
    set_dict(Line, Board, ValidLine, NewBoard),
    set_dict(board, Player, NewBoard, NewPlayer).

clean_line(Player, Line, NewPlayer) :-
    find_dict(board, Player, Board),
    find_dict(Line, Board, LineData),
    find_dict(all, LineData, Colors),
    find_dict(stocks, LineData, Stocks),
    find_dict(valid, LineData, [Color]),

    add([],Line, Color, CurrentStocks),
    append(A, [Color | B], Colors),
    append(A, B, List),
    set_dict(all, LineData, List, NewLineData),
    set_dict(valid, NewLineData, List, ValidLine),

    get_column(Line, Color, Column),
    update_score(Player, (L,Column), TempPlayer).

    add([], L, empty, Stocks),
    set_dict(stocks, ValidLine, Stocks, ValidLine2),
    set_dict(Line, Board, ValidLine2, NewBoard),
    set_dict(board, TempPlayer, NewBoard, NewPlayer).

% Revisa Las lineas completadas en la zona de preparación 
verify_lines(Player, [], Player).
verify_lines(Player, [_:Line|Lines], NewPlayer) :-
    clean_line(Player, Line, CurrPlayer),
    verify_lines(CurrPlayer, Lines, NewPlayer).
verify_lines(Player, [_|Lines], NewPlayer) :-
    verify_lines(Player, Lines, NewPlayer).
verify_lines(Player, Lines:unordered, NewPlayer) :-
    sort_by_index(Lines, SortedLines),
    verify_lines(Player, SortedLines, NewPlayer).

% Players

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

valid_moves(Game, Player, Moves) :-
    find_dict(factories, Game, Factories),
    find_dict(board, Player, Board),
    findall(LineId:FactoryId:Color, (
        find_dict(LineId, Board, Line),
        find_dict(stocks, Line, Stocks),
        member(empty, Stocks),
        find_dict(valid, Line, ValidColors),
        find_dict(FactoryId, Factories, ThisFactory),
        member(Color, ValidColors),
        member(Color, ThisFactory)
        ), Moves
    ),
    not(length(Moves,0)).

available_colors(Game, Moves) :-
    find_dict(factories, Game, Factories),
    findall(Count:FactoryId:Color, (
        find_dict(FactoryId, Factories, ThisFactory),
        member(Color, ThisFactory),
        Color \= empty,
        count(ThisFactory, Color, Count)
    ), Moves),
    not(length(Moves,0)).

update_player(Player, Game, Line:Factory:Color, NewPlayer, OutTiles, FinalPlayer) :-
    update_line(Player, Game, Line:Factory:Color, TempPlayer, Diff, Ammount),
    find_dict(board, TempPlayer, Board),
    % TODO Print Player Mode
    verify_lines(TempPlayer, Board:unordered, FinalPlayer),
    OutTiles is Ammount-Diff,
    penalize(TempPlayer, Diff, NewPlayer).

update_game(Game, _:Factory:Color, NewGame, OutTiles) :-
    find_dict(factories, Game, Factories),
    find_dict(Factory, Factories, FactoryData),
    findall(X, (
        member(X, FactoryData),
        not(member(X, [empty, first,Color]))
    ), CenterTiles),
    add([], 4, empty, NewFactory),
    set_dict(Factory, Factories, NewFactory, TemporalFactories),
    find_dict(center, TemporalFactories, Center),
    append(CenterTiles, Center, NewCenter),
    set_dict(center, TemporalFactories, NewCenter, FinalFactories),
    % TODO Print State of center
    set_dict(factories, Game, FinalFactories, TempGame),
    find_dict(outs, Game, Outs),
    find_dict(Color, Outs, Ammount),
    Sum is Ammount+OutTiles,
    set_dict(Color, Outs, Sum, NewOuts),
    set_dict(outs, TempGame, NewOuts, NewGame).

basic(Game, Player, NewGame, NewPlayer, A) :-
    valid_moves(Game, Player, [A|_]),
    update_player(Player, Game, A, NewPlayer, Return, _),
    update_game(Game, A, NewGame, Return).
basic(Game, Player, NewGame, NewPlayer, none:Id:Color) :-
    available_colors(Game, [Ammount:Id:Color| _ ]), !,
    update_game(Game, none:Id:Color, NewGame, Ammount),
    Penalization is Ammount * -1,
    penalize(Player, Penalization, NewPlayer).
basic(Game, Player, Game, Player, none:none:none).

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
