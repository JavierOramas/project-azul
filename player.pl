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

consecutive([(X, B)|L], (X,Y), C, R) :-
    % verifica si el segundo punto de la lista es adyacente al primero
    B is Y+1, !,
    % verifica el resto
    consecutive(L, (X,B), A,R),
    % Añade a la lista el punto que fue adyacente
    append([(X,B)], A,C).
% Si no tiene más adyacentes, retorna una lista vacía
consecutive(L,_,[],L).

% Encuentra todos los elementos en adyacentes en la lista, si no hay elementos, devuelve una lista vacía
adj([],[]).
adj([X|L], I) :-
    % Busca los consecutivos al principio
    consecutive(L,X,C,R),
    % los añade a la lista de retorno
    append([X],C,B),
    % busca el proximo bloque de adyacentes
    adj(R,K),
    % añade el resto de los adyacentes a la lista actual
    append([B], K, I).

% busca los elementos adyacentes a un elemento en la lista
find_adj(L, I) :-
    % si es un solo elemento no hay adyacentes
    is_list(L),
    % ordena los elementos
    sort(L,S),
    % genera la lista de todos los adyacentes
    adj(S,I).

% gets index
find_index(V,L,I) :-
    % Obtiene la lista hasta el elemento V
    append(A, [V|_], L),
    % Retorna el largo de esa lista que coincide con el index de v
    length(A,I).
% si no está e elemento, retorna -1
find_index(_,_,-1).

get_column(Line, Color, Column) :-
    % genera la lista de colores
    list_colors(Colors),
    % encuentra el indice en la lista de colores
    find_index(Color, Colors, Idx),
    % el nuevo indice en el muro será el resultado de la formula siguiente
    % se suma 1 al final para referirse a la primera columna (0 en la lista)
    Column is (Idx+Line-1) mod 5 +1.

% % % Others
count(L, X, N) :-
    findall(1,member(X,L), K),
    length(K, N).

% Sort list by index
sort_by_index(L, R) :-
    % invierte el diccionario
    findall(X:Y, find_dict(X, L, Y), L1),
    % ordena por el index
    sort(L1, R1),
    % vuelve a invertir el xiccionario
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
generate_board(Data:board):-
    % Genera una lista de listas de longitud 5
    add([], 5, 1, List),
    enumerate(List, 1, Enum),
    list_colors(C),

    % asigna los colores válidos a cada fila
    findall([New:stocks, C:valid, C:all]:Sz,
        (find_dict(Sz, Enum, _),
    add([], Sz, empty, New)
    ), Data:board).

% Game

% Penaliza al jugador
penalize(Player, Penalization, NewPlayer) :-
    % la penalización tiene que ser negativa
    Penalization < 0,
    % se obtienen las penalizaciones del jugador
    find_dict(penalizations, Player, Penalizations),
    length(Penalizations, N),
    % se obtiene el nuevo valor de las penalizaciones
    N > 0, !,
    % tomamos la primera
    append([Penalization], R, Penalizations),
    % TODO Print penalization for Player
    % se elimina la penalizacion de la lista de penalizaciones
    set_dict(penalizations, Player, R, TempPlayer),
    % se obtiene el score
    find_dict(score, Player, Score),
    % se afecta el score con la penalizacion
    UpdatedScore is Score + Penalization,
    % se actualiza el score
    set_dict(score, TempPlayer, UpdatedScore, TempPlayer2),
    PnalizationTimes is Penalization+1,
    % se llama recursivo a penlizar`
    penalize(TempPlayer2, PnalizationTimes, NewPlayer).
% si no quedan penalizaciones pendientes se retorna el jugador
penalize(Player, _, Player).

% transpone el muro
transpose_wall(L,R) :-
    findall((Y,X), member((X,Y), L), R).

% calcula la puntuación horizontal y vertical de poner una pieza
tile_score(Player, (Row, Column), Score) :-
    % Obtiene el muro del jugador y coloca la pieza
    find_dict(wall, Player, Wall),
    append(Wall, [(Row, Column)], Wall2),
    % calcula el score horizontal y verticla, transponiendo la lista
    line_score(Wall2, [(Row, Column)], RowScore),
    transpose_wall(Wall2, TransposedWall),
    line_score(TransposedWall, [(Row, Column)], ColumnScore),
    % suma los scores 
    Score is RowScore + ColumnScore.

% Calcula el score de una linea
line_score(List, Tile, Score) :-
    % obtiene la lista de los adyacentes
    find_adj(List, Interval),
    % busca en todos los intervalos si el tile esta en alguno
    findall(X, (
        member(X, Interval),
        member(Tile, X)
        ), [Adj]), 
        % obtiene la longitud de estos
        length(Adj, Score).

% Coloca una pieza en el muro
update_wall(Player, Tile, NewPlayer) :-
    % TODO Print tile for Player
    % obtiene el muro del jugador
    find_dict(wall, Player, Wall),
    % coloca la pieza en el muro
    add(Wall, 1, Tile, NewWall),
    % actualiza el muro del jugador
    set_dict(wall, Player, NewWall, NewPlayer).

% 
update_score(Player, (Line,Color), NewPlayer) :-
    % obtiene la zona de preparación del jugador
    find_dict(board, Player, Board),
    find_dict(Line, Board, LineData),
    find_dict(stocks, LineData, Stocks),
    % si tiene espacios vacíos en la linea 
    count(Stocks, empty, 0), !,
    % obtiene la puntuación de la pieza 
    tile_score(Player, (Line,Color), Score),
    find_dict(score, Player, PlayerScore),
    % actualiza la puntuación del jugador
    Sum is PlayerScore + Score,
    % actualiza el muro y asigna la puntuacion al jugador
    update_wall(Player, (Line,Color), CurrPlayer),
    set_dict(score, CurrPlayer, Sum, NewPlayer).
% si no pudo colocar la pieza se queda igual
update_score(Player, _, Player).


update_line(Player, Game, Line:Factory:Color, NewPlayer, TilesOut, TilesPenalty) :-
    % obtiene la zona de preparación del jugador asociada a una linea
    find_dict(factories, Game, Factories),
    find_dict(Factory, Factories, FactoryData),
    find_dict(board, Player, Board),
    find_dict(Line, Board, LineData),
    find_dict(stocks, LineData, Stocks),
    % cuenta los espacios vacíos en la linea
    count(Stocks, empty, EmptyCount),
    % cuenta cuantas piezas del color seleccionado hay en la factory seleccionada
    count(FactoryData, Color, ColorCount),
    % reemplaza las piezas vacías en la linea de preparación
    replace(Stocks, ColorCount, empty, Color, NewStocks),
    count(NewStocks,empty, NewEmpty),
    % Si sobraron piezas, penaliza al jugador con esa cantidad de piezas
    TilesOut is min(EmptyCount-ColorCount, 0),
    TilesPenalty is min(NewEmpty-1, 0)* -(Line -1),
    % TODO print Player Preparation zone
    % Actualiza todos los tableros en el jugador
    set_dict(stocks, LineData, NewStocks, NewLineData),
    set_dict(valid, NewLineData, [Color], ValidLine),
    set_dict(Line, Board, ValidLine, NewBoard),
    set_dict(board, Player, NewBoard, NewPlayer).

clean_line(Player, Line, NewPlayer) :-
    % obtiene toda la información de los tableros del jugador
    find_dict(board, Player, Board),
    find_dict(Line, Board, LineData),
    find_dict(all, LineData, Colors),
    find_dict(stocks, LineData, Stocks),
    find_dict(valid, LineData, [Color]),

    % verifica si la linea está completa
    add([],Line, Color, Stocks),
    append(A, [Color | B], Colors),
    append(A, B, List),

    % si la linea está completa actualiza los colores validos para la linea
    set_dict(all, LineData, List, NewLineData),
    set_dict(valid, NewLineData, List, ValidLine),

    % actualiza el score del jugador
    get_column(Line, Color, Column),
    update_score(Player, (Line,Column), TempPlayer),

    % Limpia la linea (añade Line veces empty)
    % Actualiza tods los tableros en Player
    add([], Line, empty, Stocks),
    set_dict(stocks, ValidLine, Stocks, ValidLine2),
    set_dict(Line, Board, ValidLine2, NewBoard),
    set_dict(board, TempPlayer, NewBoard, NewPlayer).

% Revisa Las lineas completadas en la zona de preparación
% Si no hya lineas que limpiar retorna el jugador sin cambios
verify_lines(Player, [], Player).
verify_lines(Player, [_:Line|Lines], NewPlayer) :-
    % Limpia la linea y manda a limpiar recursivamente
    clean_line(Player, Line, CurrPlayer),
    verify_lines(CurrPlayer, Lines, NewPlayer).
verify_lines(Player, [_|Lines], NewPlayer) :-
    verify_lines(Player, Lines, NewPlayer).
verify_lines(Player, Lines:unordered, NewPlayer) :-
    % ordena las lineas y manda a limpiar recursivamente
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
    add([], N, [Board, Floor, []:wall, 0:score], List),
    findall(P, (
        member(X, List),
        select_strategy(S),
        set_dict(strategy, X,S,P)
    ), UnorderedPlayers),
    % Ordena los jugadores
    enumerate(UnorderedPlayers, 1, Players).

valid_moves(Game, Player, Moves) :-
    % obtiene el tablero del jugador y las factories
    find_dict(factories, Game, Factories),
    find_dict(board, Player, Board),
    % genera todos los posibles movimientos validos
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

% Obtiene las formas de las cuales se puede tomar piezas de las factories
available_colors(Game, Moves) :-
    % Obtiene todas las factories
    find_dict(factories, Game, Factories),
    % Obtiene todas las formas de tomar piezas de una factory
    findall(Count:FactoryId:Color, (
        find_dict(FactoryId, Factories, ThisFactory),
        member(Color, ThisFactory),
        Color \= empty,
        count(ThisFactory, Color, Count)
    ), Moves),
    % garantiza no devolver una cantidad vacía de moves
    not(length(Moves,0)).

update_player(Player, Game, Line:Factory:Color, NewPlayer, OutTiles, FinalPlayer) :-
    % Actualiza la linea en la zona de preparación del jugador
    update_line(Player, Game, Line:Factory:Color, TempPlayer, Diff, Ammount),
    % Obtiene el Tablero del jugador
    find_dict(board, TempPlayer, Board),
    % TODO Print Player Mode
    % Verifica si alguna de las filas de preparación se llenó
    verify_lines(TempPlayer, Board:unordered, FinalPlayer),

    % penaliza al jugador con la cantidad de piezas que no pudo poner en la zona de preparación
    OutTiles is Ammount-Diff,
    penalize(TempPlayer, Diff, NewPlayer).

% Actualiza todo el tablero
update_game(Game, _:Factory:Color, NewGame, OutTiles) :-
    % Obtiene la información de las fabricas
    find_dict(factories, Game, Factories),
    find_dict(Factory, Factories, FactoryData),
    % Mueve las piezas no tomadas al centro
    findall(X, (
        member(X, FactoryData),
        not(member(X, [empty, first,Color]))
    ), CenterTiles),

    % generamos una factory nueva para sustituir la antigua
    add([], 4, empty, NewFactory),
    % sustituye los nuevos valores
    set_dict(Factory, Factories, NewFactory, TemporalFactories),
    find_dict(center, TemporalFactories, Center),
    append(CenterTiles, Center, NewCenter),
    set_dict(center, TemporalFactories, NewCenter, FinalFactories),
    % TODO Print State of center
    % actualiza la cantidad de piezas fuera del jeugo
    set_dict(factories, Game, FinalFactories, TempGame),
    find_dict(outs, Game, Outs),
    find_dict(Color, Outs, Ammount),
    Sum is Ammount+OutTiles,
    set_dict(Color, Outs, Sum, NewOuts),
    set_dict(outs, TempGame, NewOuts, NewGame).

% Estrategia básica de jugador:
% Obtiene todas las jugadas válidas y ejecuta la primera en la lista
basic(Game, Player, NewGame, NewPlayer, Move) :-
    % Obtiene la lista de movimientos válidos
    valid_moves(Game, Player, [Move|_]),
    % Ejecuta la primera jugada en la lista
    update_player(Player, Game, Move, NewPlayer, Return, _),
    % actualiza el tablero del Jugador
    update_game(Game, Move, NewGame, Return).
% Si no puede tomar ninguna para el tablero
basic(Game, Player, NewGame, NewPlayer, none:Id:Color) :-
    % toma la primera jugada igualmente
    available_colors(Game, [Ammount:Id:Color| _ ]), !,
    % actualiza el juego
    update_game(Game, none:Id:Color, NewGame, Ammount),
    % calcula la penalización
    Penalization is Ammount * -1,
    % penaliza al jugador
    penalize(Player, Penalization, NewPlayer).
% Si no puede hacer nada pasa el turno
basic(Game, Player, Game, Player, none:none:none).

% Estrategia Greedy de Jugador:
% Obtiene todos los movimientos validos
% Escoge el que maximice el score
% Si no existe, el que minimice la penalización
greedy(Game, Player, NewGame, NewPlayer, Move) :-
    % Jugadas validas
    valid_moves(Game, Player, Moves), !,
    % log mode ID
    % genera una lista con todas las jugadas y sus scores
    findall(Score:Move, (
        member(Move, Moves),
        update_player(Player, Game, Move, _, _, TempPlayer),
        find_dict(score, TempPlayer, Score)
        ), Options
    ),
    % ordena la lista de menor a mayor
    sort(Options, Sorted),
    % toma la ultima jugada
    append(_, [_:Move], Sorted),
    % Set Mode ID

    % Ejecuta la jugada y actualiza el score
    update_player(Player, Game, Move, NewPlayer, Return, _),
    update_game(Game, Move, NewGame, Return).

% Si no puede tomar ninguna para el tablero
greedy(Game, Player, NewGame, NewPlayer, none:Id:Color) :-
    % Obtiene todas las jugadas válidas
    available_colors(Game, Moves), !,
    % Ordena la lista de mayor a menor por la cantidad de piezas
    sort(Moves, [Ammount:Id:Color|_]),
    % Toma la que tenga menor cantidad de piezas
    update_game(Game, none:Id:Color, NewGame, Ammount),
    %  Calcula la penlización y lo penaliza
    Neg is Ammount * -1,
    penalize(Player, Neg, NewPlayer).
% Si no puede jugar pasa el turno
greedy(Game, Player, Game, Player, none:none:none).


% Si no quedan Jugadores por Jugar termina la ronda
run_round(Game, [], Game, []).
run_round(Game, [Player:Id| Players], NewGame, [Id:FId| Events]) :-
    % Detecta estrategia del jugador
    find_dict(strategy, Player, Strategy),
    % TODO Print Player info

    % Ejecuta la estrategia utilizando el tablero del jugador y las fabricas que hay disponibles 
    Move =..[Strategy, Game, Player, TempGame, NewPlayer, LId:FId:Color],
    % TODO Print Move
    Move,
    writeln(LId),
    writeln(Color),
    % Obtiene el estado de las fabricas despues de la jugada
    find_dict(factories, TempGame, Factories),
    % TODO Print Factories info and Preparation zone for Player
    writeln(Factories),

    % Actualiza el tablero del jugador
    find_dict(players, TempGame, PrevPlayers),
    set_dict(Id, PrevPlayers, NewPlayer, CurrPlayers),
    set_dict(players,TempGame, CurrPlayers, NewTempGame),

    % Cede el turno al siguiente jugador
    run_round(NewTempGame, Players, NewGame, Events).

clean_players(Game, NewGame) :-
    % obtiene los jugadores
    find_dict(players, Game, Players),
    findall(Player:Id, (
        % toma el tablero de cada jugador
        member(X:Id, Players),
        find_dict(board, X, Board),
        verify_lines(X, Board:unsorted, CleanedPlayer),
        % info_log([[CleanedPlayer:player, Id:id]:player]),
        % genera el piso y lo asigna al jugador
        generate_floor(Penalizations),
        set_dict(penalization, CleanedPlayer, Penalizations, NewPlayer),
        % asigna score 0 al jugador
        find_dict(score, NewPlayer, CurScore),
        Score is max(CurScore, 0),
        set_dict(score, NewPlayer, Score, Player)
    ), NewPlayers),
    % asigna los nuevos jugadores al nuevo juego
    set_dict(players, Game, NewPlayers, NewGame).
