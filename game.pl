% Game valid colors
get_colors([blue, red, yellow, black, white]).

% prints Text if the first param is 1
if_print(1, Text):-
    open('log.txt', append, Stream),
    write(Stream, Text),
    nl(Stream),
    close(Stream).
if_print(_, _).

% Adds L to X K times, result in L1
add(L, 0, _, L).
add(L, K, X, R) :-
    K>0,
    P is K-1,
    append(L, [X], L1),
    add(L1, P, X, R).

% true if the param is list
isList([]).
isList([_|_]).

% true if list has any element
any(true).
any(L) :-
    isList(L),
    member(true, L).

% returns list of all tiles in the same row/column in C
% returns list rest of the tiles in R
get_adj([(X, B)|L],  (X, Y), C, R) :-
    B is Y+1, !,
    get_adj(L,  (X, B), A, R),
    append([(X, B)], A, C).
get_adj(L, _, [], L).

% returns list of all tiles ajdacent in list
adjacent([], []).
adjacent([X|L], I) :-
    get_adj(L, X, C, R),
    append([X], C, B),
    adjacent(R, K),
    append([B], K, I).

% sorts adjacents lists and returns it
get_all_adjacent(L, I) :-
    isList(L),
    sort(L, S),
    adjacent(S, I).

% gets Ith element in L
% returnsnvalue in V
find_index(V, L, I) :-
    append(A, [V|_], L),
    length(A, I).
find_index(_, _, -1).

% Find value:property
find_dict(P, O, V) :-
    member(V:P, O).

% find value:property, if not found returns Default
if_find_else_default(P, O, V, _) :-
    find_dict(P, O, V).
if_find_else_default(_, _, D, D).

% deletes value:property 
del_dict(_, [], []).
del_dict(P, [_:P|R], L) :- !,
    del_dict(P, R, L). 
del_dict(P, [X:Y|R], [X:Y|L]) :-
    Y\=P,
    del_dict(P, R, L).

% updates value:property
set_dict(P, O, V, N) :-
    del_dict(P, O, C),
    append([V:P], C, N).

% replaces T first occurrences of V with N in R
replace(L, 0, _, _, L) :- !.
replace(L, _, V, _, L) :-
    not(member(V, L)), !.
replace(L, T, V, N, R) :-
    T>0,
    Z is T-1,
    append(A, [V|B], L), !,
    replace(B, Z, V, N, K),
    append(A, [N|K], R).

% joins a list of lists/elements in a single list
append_all([], []).
append_all([X|Y], R) :-
    append_all(Y, L),
    append(X, L, R).

% returns index of the column in the wall given line and color
find_column(Line, Color, Column) :-
    get_colors(Colors),
    find_index(Color, Colors, Idx),
    Column is ((Idx+Line-1)mod 5 ) +1.

% Counts the occurences of V elements in L
count(L, V, R) :-
    findall(1, member(V, L), K),
    length(K, R).

enumerate([], _, []).
enumerate([E1|List], Number, [E1:Number|Enum]) :-
    Next is Number+1,
    enumerate(List, Next, Enum).

% transposes wall _ => | to find full rows/columns
transpose_wall(L, R) :-
    findall((Y, X), member((X, Y), L), R).

% sorts list by index
indexed_sort(L, R) :-
    % reverses X:Y -> Y:X
    findall(X:Y, find_dict(X, L, Y), I),
    % sorts
    sort(I, O),
    % reverses Y:X -> X:Y
    findall(X:Y, find_dict(X, O, Y), R).

% dynamic variable that stores the current first player
:- (dynamic initial_player/1).
initial_player(1).

% Fills factory with Tiles, returns it in Factories
fill_fact([], _, []).
fill_fact(Factories, [], Factories).
fill_fact([[]|Factories], Tiles, [[]|Result]) :-
    fill_fact(Factories, Tiles, Result).
fill_fact([[_|Fac1]|Factories], [Tile1|Tiles], [[Tile1|Res1]|Result]) :-
    fill_fact([Fac1|Factories], Tiles, [Res1|Result]).

% prepares the game for next round
% checks if its possible to run next round with the remaining tiles
populate(Game, NewGame) :-
    find_dict(ammounts, Game, Ammounts),
    find_dict(factories, Game, Factories),
    length(Factories, FacSz),
    findall(X, member(X:_, Ammounts), Quantities),
    sum_list(Quantities, Sum),
    % check if the tiles could full the factories
    Sum<FacSz*4, !,
    find_dict(outs, Game, Outs),
    open("log.txt", append, FD),
    write(FD,["Añadiendo piezas a la bolsa.\n\t", Ammounts:ammounts, "\n\t", Outs:outs]),
    nl(FD),
    close(FD),
    % adding tiles to the bag
    findall(RealAmmount:Color, (
        find_dict(Color, Ammounts, QAmmount),
        find_dict(Color, Outs, QOut),
        RealAmmount is QOut+QAmmount
    ), NewAmmounts),
    set_dict(ammounts, Game, NewAmmounts, TempGame),
    % Saving that 0 tiles are out
    findall(0:Color, member(_:Color, Outs), NewOuts),
    set_dict(outs, TempGame, NewOuts, NewGame).
populate(Game, Game).

% checks the factories
% prepares new round
% counts the tiles
new_round(Game, NewGame) :-
    open("log.txt", append, FD),
    write(FD,"Starts New Round\n"),
    close(FD),
    % Check if more tiles are needed
    populate(Game, TempGame1),
    %Select the random tiles to add
    find_dict(ammounts, TempGame1, Ammounts),
    findall(List, ( 
        find_dict(Color, Ammounts, Quantity),
        add([], Quantity, Color, List)
    ), ColorGroups),
    append_all(ColorGroups, ColorsList),
    random_permutation(ColorsList, ColorsOrder),
    % Saving the selected tiles
    find_dict(factories, TempGame1, GameFac),
    del_dict(center, GameFac, SimpleFac),
    findall(Fac, member(Fac:_, SimpleFac), RawFac),
    fill_fact(RawFac, ColorsOrder, TempFac),
    append_all(TempFac, UsedTiles),
    % Update the ammount of tiles, and create the new game
    findall(NewQ:Color, ( 
        find_dict(Color, Ammounts, QOld),
        count(UsedTiles, Color, Used),
        NewQ is QOld-Used
    ), NewAmmounts),
    set_dict(ammounts, TempGame1, NewAmmounts, TempGame2),
    enumerate(TempFac, 1, EnumFac),
    set_dict(center, EnumFac, [first], AllFac),
    set_dict(factories, TempGame2, AllFac, NewGame).
    % split_fac(2,0,AllFac:factories,[],[], _).

% checks for full rows in the board
any_full_row(Player, RowsQ) :-
    find_dict(wall, Player, Wall),
    findall(true, ( 
        bagof(Column, member((_, Column), Wall), Columns),
        length(Columns, 5)
    ), Rows),
    length(Rows, RowsQ),
    any(Rows).

% if a player has full rows, the game ends 
ending_condition(Game) :-
    find_dict(players, Game, P),
    member(X:_, P),
    any_full_row(X, _).

full_rows(Player, RowsQ) :-
    any_full_row(Player, RowsQ), !.
full_rows(_, 0).

% verify full color in the wall
% verifies i+1 pos in the next line
% if in the last line, checks the first 
cascade((5, Col), Wall) :-
    member((5, Col), Wall).
cascade((Row, Col), Wall) :-
    member((Row, Col), Wall),
    NewRow is Row+1,
    NewCol is max((Col+1)mod 6, 1),
    cascade((NewRow, NewCol), Wall).

% checks in cascade all colors of the wall
full_colors(Player, Ammount) :-
    find_dict(wall, Player, Wall),
    findall(true, ( 
        member((1, Col), Wall),
        cascade((1, Col), Wall)
    ), List),
    length(List, Ammount).    

% calculates score for the wall status of the player
wall_score(P, S) :-
    full_rows(P, RS),
    % rows
    find_dict(wall, P, T),
    % transposes rows/cols and calculates columns
    transpose_wall(T, RT),
    full_rows([RT:wall], CS),
    % colors
    full_colors(P, DS),
    % score for each one
    S is RS*2+CS*7+10*DS.

% prepares the new game for the number of players and factories
new_game(Players, Factories, [P, A:ammounts, O:outs, F:factories]) :-
    get_colors(C),
    new_players(Players, P),

    % filling bag and factories
    findall(20:X, member(X, C), A),
    findall(0:X, member(X, C), O),
    add([], 4, empty, E),
    add([], Factories, E, EF),
    enumerate(EF, 1, NF),
    set_dict(center, NF, [], F).

% reorders the players for next round
new_order(Game, NewPlayers) :-
    find_dict(players, Game, Players),
    indexed_sort(Players, OriginalOrder),
    sort_players(OriginalOrder, NewPlayers).

% sorts players for new round
sort_players(Players, NewPlayers) :-
    initial_player(Pid),
    append(A, [Player:Pid|B], Players),
    append([Player:Pid|B], A, NewPlayers).

% gets the players in the order and runs the round, 
% once ended validates the board to see if new round has to start
run(Game, Events, NewGame) :-
    new_order(Game, Players),
    run_round(Game, Players, TempGame, CurEvents),
    append(Events, CurEvents, NewEvents),
    validate(TempGame, NewEvents, NewGame).

% checks if the game can continue or has to end
% either for a player filling a column or because there are no more tiles
validate(Game, Events, NewGame) :-
    find_dict(factories, Game, Factories),
    findall(Fac, member(Fac:_, Factories), FacList),
    append_all(FacList, AllTiles),
    length(AllTiles, Sz),
    count(AllTiles, empty, Sz), !,
    refresh_players(Game, TempGame),
    end_or_continue(TempGame, Events, NewGame).
validate(Game, Events, NewGame) :-
    run(Game, Events, NewGame).

% checks if a full row exists
% if not, then finds new first player and prepares new player turn an sends him to play
end_or_continue(Game, _, NewGame) :-
    ending_condition(Game), !,
    calculate_scores(Game, NewGame).
end_or_continue(Game, Events, NewGame) :-
    initial_player(Id),
    if_find_else_default(center, Events, NewId, Id),
    retract(initial_player(Id)),
    asserta(initial_player(NewId)),
    find_dict(players, Game, Players),
    find_dict(NewId, Players, FirstPlayer),
    penalize(FirstPlayer, -1, NewFirstP,1),
    open("log.txt", append, FD),
    write(FD,["Player ", NewId, " gets penalized for being the first to play next turn"]),
    nl(FD),
    close(FD),
    set_dict(NewId, Players, NewFirstP, NewPlayers),
    set_dict(players, Game, NewPlayers, TempGame1),
    new_round(TempGame1, TempGame2),
    run(TempGame2, [], NewGame).

% for each player calculates the score at the end of the game
calculate_scores(Game, NewGame) :-
    find_dict(players, Game, Players),
    findall(NewPlayer:Id, ( 
        find_dict(Id, Players, Player),
        wall_score(Player, WallScore),
        find_dict(score, Player, Score),
        NewScore is Score+WallScore,
        set_dict(score, Player, NewScore, NewPlayer)
    ), NewPlayers),
    set_dict(players, Game, NewPlayers, NewGame).

% predicate to print in a "beautiful?" way the players and their scores
% sorted from the highest score to the lowest
print_scores(EndedGame:scores) :-
    open("log.txt", append, FD),
    find_dict(players, EndedGame, Players),
    nl(FD),
    findall([Id:id, Strategy:strategy]:Score, ( 
        member(X:Id, Players),
        find_dict(score, X, Score),
        find_dict(strategy, X, Strategy)
    ), PlayersInverted),
    indexed_sort(PlayersInverted, PlayersSorted),
    reverse(PlayersSorted, P),
    findall(P, (
        member([Id:_|St]:Score, P),
        write(FD,"Player "),
        write(FD, Id),
        write(FD, " ("),
        write(FD, St),
        write(FD, ")"),
        write(FD," => "),
        write(FD,Score),
        nl(FD)
        ),_),
    close(FD).

% initial predicate, creates the game, starts it and outputs the results
start_game(Players, Factories) :-
    open("log.txt", write, FD),
    write(FD,["Preparing a ", Players, " players Game"]),
    nl(FD),
    close(FD),
    new_game(Players, Factories, Game),
    new_round(Game, NewGame),
    run(NewGame, [], EndedGame), !,
    open("log.txt", append, Fd),
    write(Fd, "The game ends. And the Winner is... \n"),
    write(Fd, "🥁🥁🥁🥁🥁🥁🥁🥁🥁🥁🥁🥁🥁🥁🥁🥁🥁🥁🥁🥁🥁\n"),
    close(Fd),
    print_scores(EndedGame:scores).
    % writeln("true.").
% if error occurs, notify
start_game(_, _) :- 
    open("log.txt", append, FD),
    write(FD,"An unexpected failure occur\n"),
    close(FD).
    % writeln("fail.").

% floor for penalizations
generate_floor([-1, -1, -2, -2, -2, -3, -3]:penalties).

% list of strategies
get_strategy([first, greedy, random]).

% gets a random strategy to assign to the player
random_strategy(S) :-
    get_strategy(St),
    random_permutation(St, [S|_]).

% counts the number of tiles in a row
line_score(List, Tile, Score) :-
    get_all_adjacent(List, Interval),
    findall(X, ( 
        member(X, Interval),
        member(Tile, X)
    ), [Adyacents]),
    length(Adyacents, Score).

% get se score obtained for placing a tile in the board
tile_score(Player,  (Row, Column), Score) :-
    find_dict(wall, Player, Wall),
    append(Wall, [(Row, Column)], NewWall),
    % row score
    line_score(NewWall,  (Row, Column), RowScore),
    transpose_wall(NewWall, InvertedAxis),
    % column score
    line_score(InvertedAxis,  (Column, Row), ColumnScore),
    Score is RowScore+ColumnScore.

% gets all possible ways of taking tiles from the factories
valid_moves(Game, Player, Moves) :-
    find_dict(factories, Game, Fac),
    find_dict(board, Player, Board),
    findall(Lid:Fid:Color,( 
        find_dict(Lid, Board, Line),
        find_dict(prep, Line, Prep),
        member(empty, Prep),
        find_dict(valid, Line, ValidColors),
        find_dict(Fid, Fac, CurFac),
        member(Color, ValidColors),
        member(Color, CurFac)
    ), Moves),
    not(length(Moves, 0)).    

% gives all the options for selecting a color from a factory
available_colors(Game, Moves) :-
    find_dict(factories, Game, Fac),
    findall(Count:Fid:Color, ( 
        find_dict(Fid, Fac, F),
        member(Color, F),
        Color \= empty,
        count(F, Color, Count)
    ), Moves),
    not(length(Moves, 0)).  

% if a line in the prep is full, clean it and update the board
clean_line(Player, L, NewPlayer) :-
    find_dict(board, Player, Board),
    find_dict(L, Board, Line),
    find_dict(all, Line, Colors),
    find_dict(valid, Line, [C]),
    find_dict(prep, Line, CurPrep),

    add([], L, C, CurPrep),
    append(A, [C | B], Colors),
    append(A, B, List),
    set_dict(all, Line, List, TempLine0),
    set_dict(valid, TempLine0, List, TempLine1),
    % update the player
    find_column(L, C, Column),
    update_score(Player, (L, Column), TempPlayer),
    % cleaning the line
    add([], L, empty, Prep),
    set_dict(prep, TempLine1, Prep, TempLine2),
    set_dict(L, Board, TempLine2, NewBoard),
    set_dict(board, TempPlayer, NewBoard, NewPlayer).

% if a tile was added to the board, update the score of the player
update_score(Player,  (L, C), NewPlayer) :-
    find_dict(board, Player, Board),
    find_dict(L, Board, Line),
    find_dict(prep, Line, Prep),
    count(Prep, empty, 0), !,
    tile_score(Player,  (L, C), Score),
    find_dict(score, Player, PScore),
    Sum is Score+PScore,
    update_wall(Player,  (L, C), CurPlayer),
    set_dict(score, CurPlayer, Sum, NewPlayer).
update_score(P, _, P).


% updates line on the preparation zone of the player, Line L recieves the color Color from the factory F
update_line(Player, Game, L:F:Color, NewPlayer, Diff, Tiles, Test) :-
    find_dict(factories, Game, Factories),
    find_dict(F, Factories, Fac),
    find_dict(board, Player, Board),
    find_dict(L, Board, Line),
    find_dict(prep, Line, Prep),
    count(Prep, empty, Empty),
    count(Fac, Color, Ammount),
    replace(Prep, Ammount, empty, Color, NewPrep),
    count(NewPrep, empty, NewEmpty),
    Diff is min(Empty-Ammount, 0),
    Tiles is (min(NewEmpty - 1, 0) * -(L - 1)),

    if_print(Test, ["Player modified Line ", L, " -> ", NewPrep]),

    set_dict(prep, Line, NewPrep, NewLine),
    set_dict(valid, NewLine, [Color], ValidLine),
    set_dict(L, Board, ValidLine, NewBoard),
    set_dict(board, Player, NewBoard, NewPlayer).


update_wall(Player, Tile, NewPlayer) :-
    find_dict(wall, Player, Wall),
    add(Wall, 1, Tile, NewWall),
    set_dict(wall, Player, NewWall, NewPlayer).

penalize(Player, Ammount, NewPlayer, Test) :-
    Ammount<0,
    find_dict(penalties, Player, Penalties),
    length(Penalties, Sz),
    Sz>0, !,
    append([P1], R, Penalties),

    if_print(Test,["Player recive ", P1, " of penalization"]),
    set_dict(penalties, Player, R, TempPlayer1),
    find_dict(score, Player, Score),
    NewScore is Score+P1,
    set_dict(score, TempPlayer1, NewScore, TempPlayer2),
    Times is Ammount+1,
    penalize(TempPlayer2, Times, NewPlayer, Test).
penalize(Player, _, Player, _).

update_player(Player, Game, L:F:Color, NewPlayer, Return, FinalPlayer, Test) :-
    update_line(Player, Game, L:F:Color, TempPlayer0, Diff, Ammount, Test),
    find_dict(board, TempPlayer0, Board),
    verify_player_lines(TempPlayer0, Board:unsorted, FinalPlayer),
    Return is Ammount-Diff,
    penalize(TempPlayer0, Diff, NewPlayer,Test).

update_game(Game, _:F:C, NewGame, ReturnedTiles) :-

    find_dict(factories, Game, GameFac),
    find_dict(F, GameFac, Fac),
    findall(X, (
        member(X, Fac),
        not(member(X, [empty, first, C]))
    ), ToCenter),
    add([], 4, empty, NewFac),
    set_dict(F, GameFac, NewFac, TempFacs),
    find_dict(center, TempFacs, Center),
    append(ToCenter, Center, NewCenter),
    set_dict(center, TempFacs, NewCenter, NewFacs),
    % writeln(["New factories center is -> ", NewCenter]),
    set_dict(factories, Game, NewFacs, Temp),
    find_dict(outs, Game, Outs),
    find_dict(C, Outs, Number),
    Sum is Number+ReturnedTiles,
    set_dict(C, Outs, Sum, NewOuts),
    set_dict(outs, Temp, NewOuts, NewGame).

first(Game, Player, NewGame, NewPlayer, A) :-
    valid_moves(Game, Player, [A|_]), !,
    update_player(Player, Game, A, NewPlayer, Return, _, 1),
    update_game(Game, A, NewGame, Return).
first(Game, Player, NewGame, NewPlayer, none:Id:Color) :-
    available_colors(Game, [Ammount:Id:Color | _]), !,
    update_game(Game, none:Id:Color, NewGame, Ammount),
    Neg is Ammount* -1,
    penalize(Player, Neg, NewPlayer,1).
first(Game, Player, Game, Player, none:none:none).

random(Game, Player, NewGame, NewPlayer, A) :-
    valid_moves(Game, Player, Moves), !,
    random_permutation(Moves, [A|_]),
    update_player(Player, Game, A, NewPlayer, Return, _, 1),
    update_game(Game, A, NewGame, Return).
random(Game, Player, NewGame, NewPlayer, none:Id:Color) :-
    available_colors(Game, [Ammount:Id:Color | _]), !,
    update_game(Game, none:Id:Color, NewGame, Ammount),
    Neg is Ammount* -1,
    penalize(Player, Neg, NewPlayer,1).
random(Game, Player, Game, Player, none:none:none).


greedy(Game, Player, NewGame, NewPlayer, A) :-
    valid_moves(Game, Player, Moves), !,
    findall(Score:Move, (
        member(Move, Moves),
        update_player(Player, Game, Move, _, _, TempPlayer, 0),
        find_dict(score, TempPlayer, Score)
    ), Options),
    sort(Options, Sorted),
    append(_, [_:A], Sorted),
    update_player(Player, Game, A, NewPlayer, Return, _, 1),
    update_game(Game, A, NewGame, Return).
greedy(Game, Player, NewGame, NewPlayer, none:Id:Color) :-
    available_colors(Game, Moves), !,
    sort(Moves, [Ammount:Id:Color|_]),
    update_game(Game, none:Id:Color, NewGame, Ammount),
    Neg is Ammount* -1,
    penalize(Player, Neg, NewPlayer,1).
greedy(Game, Player, Game, Player, none:none:none).

generate_board(Data:board) :-
    add([], 5, 1, List),
    enumerate(List, 1, Enum),
    get_colors(C),
    findall([New:prep, C:valid, C:all]:Sz, (
        find_dict(Sz, Enum, _),
        add([], Sz, empty, New)
    ), Data).

new_players(Ammount, Players:players) :-
    generate_board(Board),
    generate_floor(Penalties),
    add([], Ammount, [Board, Penalties, []:wall, 0:score], List),
    findall(P, (
        member(X, List),
        random_strategy(S),
        set_dict(strategy, X, S, P)
    ), RawPlayers),
    enumerate(RawPlayers, 1, Players).

run_round(G, [], G, []).
run_round(Game, [P1:Id|Players], NewGame, [Id:Fid|Events]) :-
    find_dict(strategy, P1, St),
    open("log.txt", append, Fd),
    write(Fd,["Player ", Id, " with Strategy ",St, " turn start --------------------"]),
    nl(Fd),
    close(Fd),
    Move=..[St, Game, P1, TempGame1, NewP1, Lid:Fid:Color],
    Move,
    open("log.txt", append, FD),
    write(FD, [
        "Player choose all type ",
        Color,
        " from expositor ",
        Fid,
        " and add them to the ",
        Lid,
        " line"
    ]),
    nl(FD),
    close(FD),
    find_dict(factories, TempGame1, Facs),
    % writeln([Facs:factories]),
    print_preparation(NewP1:preparation),

    find_dict(players, TempGame1, OldPlayers),
    set_dict(Id, OldPlayers, NewP1, CurPlayers),
    set_dict(players, TempGame1, CurPlayers, TempGame2),

    print_wall(NewP1:wall),
    run_round(TempGame2, Players, NewGame, Events).

print_preparation(Player:preparation) :-
    open("log.txt", append, Fd),
    write(Fd, "Preparation Zone of Player: \n"),
    nl(Fd),
    find_dict(board, Player, Board),
    findall(Line:Id, (
        member(X:Id, Board),
        find_dict(prep, X, Prep),
        Times is 5 - Id,
        add(Prep, Times, ' - ', Line)
        ), Lines),
    indexed_sort(Lines, Sorted),
    findall(Line, (
            member(X:_, Sorted),
            write(Fd, X),
            nl(Fd)
        ), _) ,
    nl(Fd),
    close(Fd).


generate_wall((6,1), _, Wall, Wall).
generate_wall((Row,6), Board, Ac, [Ac|R]) :-
    NewRow is Row+1,
    generate_wall((NewRow,1),Board, [], R),!.
generate_wall((X,Y), Table, Ac, R) :-
    member((X,Y), Table), !,
    find_column(X,C,Y),
    append(Ac, [C], NewAc),
    NewY is Y+1,
    generate_wall((X,NewY), Table, NewAc, R).
generate_wall((X,Y), Table, Ac, R) :-
    append(Ac, [' - '], NewAc),
    NewY is Y+1,
    generate_wall((X,NewY), Table, NewAc, R).

% predicate to print wall
print_wall(Player:wall) :-
    open("log.txt", append, Fd),
    write(Fd, "Wall of Player: \n"),
    nl(Fd),
    find_dict(wall, Player, Wall),
    sort(Wall, SortedWall),
    generate_wall((1,1), SortedWall ,[], FinalWall),
    findall(FinalWall, (
        member(X, FinalWall),
        write(Fd,X),
        nl(Fd)
    ), _),
    nl(Fd),
    close(Fd).

% prepares players for new round, checks if a player has finished game and if not, 
% cleans board, floor and sets score 
refresh_players(Game, NewGame) :-
    find_dict(players, Game, Players),
    findall(Player:Id, (
        member(X:Id, Players),
        find_dict(board, X, Board),
        verify_player_lines(X, Board:unsorted, CleanedPlayer),
        generate_floor(Penalizations),
        set_dict(penalization, CleanedPlayer, Penalizations, NewPlayer),
        find_dict(score, NewPlayer, CurScore),
        Score is max(CurScore, 0),
        set_dict(score, NewPlayer, Score, Player)
    ), NewPlayers),
    set_dict(players, Game, NewPlayers, NewGame).

% checks if a player has a full lines in the prep zone
verify_player_lines(P, [], P).
verify_player_lines(Player, [_:Line|Lines], NewPlayer) :-
    clean_line(Player, Line, CurPlayer), !,
    verify_player_lines(CurPlayer, Lines, NewPlayer).
verify_player_lines(Player, [_|Lines], NewPlayer) :-
    verify_player_lines(Player, Lines, NewPlayer).
verify_player_lines(Player, Lines:unsorted, NewPlayer) :-
    indexed_sort(Lines, Sorted),
    verify_player_lines(Player, Sorted, NewPlayer).
