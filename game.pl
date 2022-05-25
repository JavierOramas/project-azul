
tiles_colors([blue, red, yellow, black, white]).

if_print(1, Text):-
    open('log.txt', append, Stream),
    write(Stream, Text),
    nl(Stream),
    close(Stream).
if_print(_, _).

add(L, 0, _, L).
add(L, K, X, R) :-
    K>0,
    P is K-1,
    append(L, [X], L1),
    add(L1, P, X, R).

isList([]).
isList([_|_]).

append_all([], []).
append_all([X|Y], R) :-
    append_all(Y, L),
    append(X, L, R).

any(true).
any(L) :-
    isList(L),
    member(true, L).

consecutive([(X, B)|L],  (X, Y), C, R) :-
    B is Y+1, !,
    consecutive(L,  (X, B), A, R),
    append([(X, B)], A, C).
consecutive(L, _, [], L).

blocks([], []).
blocks([X|L], I) :-
    consecutive(L, X, C, R),
    append([X], C, B),
    blocks(R, K),
    append([B], K, I).

make_intervals(L, I) :-
    isList(L),
    sort(L, S),
    blocks(S, I).

find_dict(P, O, V) :-
    member(V:P, O).

get_value_or_default(P, O, V, _) :-
    find_dict(P, O, V).
get_value_or_default(_, _, D, D).

del_dict(_, [], []).
del_dict(P, [_:P|R], L) :- !,
    del_dict(P, R, L). 
del_dict(P, [X:Y|R], [X:Y|L]) :-
    Y\=P,
    del_dict(P, R, L).

set_dict(P, O, V, N) :-
    del_dict(P, O, C),
    append([V:P], C, N).

invert_axis(L, R) :-
    findall((Y, X), member((X, Y), L), R).

replace(L, 0, _, _, L) :- !.
replace(L, _, V, _, L) :-
    not(member(V, L)), !.
replace(L, T, V, N, R) :-
    T>0,
    Z is T-1,
    append(A, [V|B], L), !,
    replace(B, Z, V, N, K),
    append(A, [N|K], R).

index_of(V, L, I) :-
    append(A, [V|_], L),
    length(A, I).
index_of(_, _, -1).

column_of(Line, Color, Column) :-
    tiles_colors(Colors),
    index_of(Color, Colors, Idx),
    Column is (Idx+Line-1)mod 5+1.

count(L, V, R) :-
    findall(1, member(V, L), K),
    length(K, R).

enumerate([], _, []).
enumerate([E1|List], Number, [E1:Number|Enum]) :-
    Next is Number+1,
    enumerate(List, Next, Enum).

indexed_sort(L, R) :-
    findall(X:Y, find_dict(X, L, Y), I),
    sort(I, O),
    findall(X:Y, find_dict(X, O, Y), R).

:- (dynamic initial_player/1).
initial_player(1).

use_fac([], _, []).
use_fac(Factories, [], Factories).
use_fac([[]|Factories], Tiles, [[]|Result]) :-
    use_fac(Factories, Tiles, Result).
use_fac([[_|Fac1]|Factories], [Tile1|Tiles], [[Tile1|Res1]|Result]) :-
    use_fac([Fac1|Factories], Tiles, [Res1|Result]).

populate(Game, NewGame) :-
    find_dict(amounts, Game, Amounts),
    find_dict(factories, Game, Factories),
    length(Factories, FacSz),
    findall(X, member(X:_, Amounts), Quantities),
    sum_list(Quantities, Sum),
    % check if the tiles could full the factories
    Sum<FacSz*4, !,
    find_dict(outs, Game, Outs),
    open("log.txt", append, FD),
    write(FD,["Añadiendo piezas a la bolsa.\n\t", Amounts:amounts, "\n\t", Outs:outs]),
    nl(FD),
    close(FD),
    % adding tiles to the bag
    findall(RealAmount:Color, (
        find_dict(Color, Amounts, QAmount),
        find_dict(Color, Outs, QOut),
        RealAmount is QOut+QAmount
    ), NewAmounts),
    set_dict(amounts, Game, NewAmounts, TempGame),
    % Saving that 0 tiles are out
    findall(0:Color, member(_:Color, Outs), NewOuts),
    set_dict(outs, TempGame, NewOuts, NewGame).
populate(Game, Game).

% split_fac(_, _, [], Top, Bottom, [Top, Bottom]):-
%     open("log.txt", append, FD),
%     write(Top),
%     write(Bottom),
%     close(FD).
% split_fac(Len, Cur, [X|Data], Acum, Bottom, R) :-
%     NewCur is Cur+1,
%     NewCur=<Len,
%     concat(Acum, [X], NewAcum),
%     split_fac(Len, NewCur, Data, NewAcum, Bottom, R), !.
% split_fac(Len, _, Data, Top, Bottom, [T, B]) :-
%     split_fac(Len, 0, Data, Bottom, Top, [B, T]).

new_round(Game, NewGame) :-
    open("log.txt", append, FD),
    write(FD,"Starts New Round\n"),
    close(FD),
    % Check if more tiles are needed
    populate(Game, TempGame1),
    %Select the random tiles to add
    find_dict(amounts, TempGame1, Amounts),
    findall(List, ( 
        find_dict(Color, Amounts, Quantity),
        add([], Quantity, Color, List)
    ), ColorGroups),
    append_all(ColorGroups, ColorsList),
    random_permutation(ColorsList, ColorsOrder),
    % Saving the selected tiles
    find_dict(factories, TempGame1, GameFac),
    del_dict(center, GameFac, SimpleFac),
    findall(Fac, member(Fac:_, SimpleFac), RawFac),
    use_fac(RawFac, ColorsOrder, TempFac),
    append_all(TempFac, UsedTiles),
    % Update the amount of tiles, and create the new game
    findall(NewQ:Color, ( 
        find_dict(Color, Amounts, QOld),
        count(UsedTiles, Color, Used),
        NewQ is QOld-Used
    ), NewAmounts),
    set_dict(amounts, TempGame1, NewAmounts, TempGame2),
    enumerate(TempFac, 1, EnumFac),
    set_dict(center, EnumFac, [first], AllFac),
    set_dict(factories, TempGame2, AllFac, NewGame).
    % split_fac(2,0,AllFac:factories,[],[], _).

any_full_row(Player, RowsQ) :-
    find_dict(wall, Player, Wall),
    findall(true, ( 
        bagof(Column, member((_, Column), Wall), Columns),
        length(Columns, 5)
    ), Rows),
    length(Rows, RowsQ),
    any(Rows).

ending_condition(Game) :-
    find_dict(players, Game, P),
    member(X:_, P),
    any_full_row(X, _).

full_rows(Player, RowsQ) :-
    any_full_row(Player, RowsQ), !.
full_rows(_, 0).

cascade((5, Col), Wall) :-
    member((5, Col), Wall).
cascade((Row, Col), Wall) :-
    member((Row, Col), Wall),
    NewRow is Row+1,
    NewCol is max((Col+1)mod 6, 1),
    cascade((NewRow, NewCol), Wall).

full_colors(Player, Amount) :-
    find_dict(wall, Player, Wall),
    findall(true, ( 
        member((1, Col), Wall),
        cascade((1, Col), Wall)
    ), List),
    length(List, Amount).    

wall_score(P, S) :-
    full_rows(P, RS),
    find_dict(wall, P, T),
    invert_axis(T, RT),
    full_rows([RT:wall], CS),
    full_colors(P, DS),
    S is RS*2+CS*7+10*DS.

new_game(Players, Factories, [P, A:amounts, O:outs, F:factories]) :-
    tiles_colors(C),
    new_players(Players, P),
    findall(20:X, member(X, C), A),
    findall(0:X, member(X, C), O),
    add([], 4, empty, E),
    add([], Factories, E, EF),
    enumerate(EF, 1, NF),
    set_dict(center, NF, [], F).

order_players(Game, NewPlayers) :-
    find_dict(players, Game, Players),
    indexed_sort(Players, OriginalOrder),
    sort_players(OriginalOrder, NewPlayers).

sort_players(Players, NewPlayers) :-
    initial_player(Pid),
    append(A, [Player:Pid|B], Players),
    append([Player:Pid|B], A, NewPlayers).

run(Game, Events, NewGame) :-
    order_players(Game, Players),
    run_round(Game, Players, TempGame, CurEvents),
    append(Events, CurEvents, NewEvents),
    validate(TempGame, NewEvents, NewGame).

validate(Game, Events, NewGame) :-
    find_dict(factories, Game, Factories),
    findall(Fac, member(Fac:_, Factories), FacList),
    append_all(FacList, AllTiles),
    length(AllTiles, Sz),
    count(AllTiles, empty, Sz), !,
    clean_players(Game, TempGame),
    end_or_continue(TempGame, Events, NewGame).
validate(Game, Events, NewGame) :-
    run(Game, Events, NewGame).

end_or_continue(Game, _, NewGame) :-
    ending_condition(Game), !,
    calculate_scores(Game, NewGame).
end_or_continue(Game, Events, NewGame) :-
    initial_player(Id),
    get_value_or_default(center, Events, NewId, Id),
    retract(initial_player(Id)),
    asserta(initial_player(NewId)),
    find_dict(players, Game, Players),
    find_dict(NewId, Players, FirstPlayer),
    penalize(FirstPlayer, -1, NewFirstP,1),
    % writeln([
    %     "Player ", 
    %     NewId, 
    %     " will be the first at the next round. ",
    %     "Cause that recive a penalization\n"
    % ]),
    set_dict(NewId, Players, NewFirstP, NewPlayers),
    set_dict(players, Game, NewPlayers, TempGame1),
    new_round(TempGame1, TempGame2),
    run(TempGame2, [], NewGame).

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
        member([Id:_|_]:Score, P),
        write(FD,"Player "),
        write(FD, Id),
        write(FD," => "),
        write(FD,Score),
        nl(FD)
        ),_),
    close(FD).

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
start_game(_, _) :- 
    open("log.txt", append, FD),
    write(FD,"An unexpected failure occur\n"),
    close(FD).
    % writeln("fail.").

penalization_list([-1, -1, -2, -2, -2, -3, -3]:penalties).

strategies([basic, greedy]).

random_strategy(S) :-
    strategies(St),
    random_permutation(St, [S|_]).    
line_score(List, Tile, Score) :-
    make_intervals(List, Interval),
    findall(X, ( 
        member(X, Interval),
        member(Tile, X)
    ), [Adyacents]),
    length(Adyacents, Score).

tile_score(Player,  (Row, Column), Score) :-
    find_dict(wall, Player, Wall),
    append(Wall, [(Row, Column)], NewWall),
    % row score
    line_score(NewWall,  (Row, Column), RowScore),
    invert_axis(NewWall, InvertedAxis),
    % column score
    line_score(InvertedAxis,  (Column, Row), ColumnScore),
    Score is RowScore+ColumnScore.

valid_choices(Game, Player, Choices) :-
    find_dict(factories, Game, Fac),
    find_dict(board, Player, Board),
    findall(Lid:Fid:Color,( 
        find_dict(Lid, Board, Line),
        find_dict(stocks, Line, Stocks),
        member(empty, Stocks),
        find_dict(valid, Line, ValidColors),
        find_dict(Fid, Fac, CurFac),
        member(Color, ValidColors),
        member(Color, CurFac)
    ), Choices),
    not(length(Choices, 0)).    

available_colors(Game, Choices) :-
    find_dict(factories, Game, Fac),
    findall(Count:Fid:Color, ( 
        find_dict(Fid, Fac, F),
        member(Color, F),
        Color \= empty,
        count(F, Color, Count)
    ), Choices),
    not(length(Choices, 0)).  

clean_line(Player, L, NewPlayer) :-
    find_dict(board, Player, Board),
    find_dict(L, Board, Line),
    find_dict(all, Line, Colors),
    find_dict(valid, Line, [C]),
    find_dict(stocks, Line, CurStocks),
    % cheking that the line is full
    add([], L, C, CurStocks),
    append(A, [C | B], Colors),
    append(A, B, List),
    set_dict(all, Line, List, TempLine0),
    set_dict(valid, TempLine0, List, TempLine1),
    % update the player
    column_of(L, C, Column),
    update_score(Player, (L, Column), TempPlayer),
    % cleaning the line
    add([], L, empty, Stocks),
    set_dict(stocks, TempLine1, Stocks, TempLine2),
    set_dict(L, Board, TempLine2, NewBoard),
    set_dict(board, TempPlayer, NewBoard, NewPlayer).

update_score(Player,  (L, C), NewPlayer) :-
    find_dict(board, Player, Board),
    find_dict(L, Board, Line),
    find_dict(stocks, Line, Stocks),
    count(Stocks, empty, 0), !,
    tile_score(Player,  (L, C), Score),
    find_dict(score, Player, PScore),
    Sum is Score+PScore,
    update_wall(Player,  (L, C), CurPlayer),
    set_dict(score, CurPlayer, Sum, NewPlayer).
update_score(P, _, P).

update_line(Player, Game, L:F:Color, NewPlayer, Diff, Tiles, Test) :-
    find_dict(factories, Game, Factories),
    find_dict(F, Factories, Fac),
    find_dict(board, Player, Board),
    find_dict(L, Board, Line),
    find_dict(stocks, Line, Stocks),
    count(Stocks, empty, Empty),
    count(Fac, Color, Amount),
    replace(Stocks, Amount, empty, Color, NewStocks),
    count(NewStocks, empty, NewEmpty),
    Diff is min(Empty-Amount, 0),
    Tiles is (min(NewEmpty - 1, 0) * -(L - 1)),

    if_print(Test, ["Player new preparation line ", L, " is -> ", NewStocks]),
    % writeln(["Player new preparation line ", L, " is -> ", NewStocks]),

    set_dict(stocks, Line, NewStocks, NewLine),
    set_dict(valid, NewLine, [Color], ValidLine),
    set_dict(L, Board, ValidLine, NewBoard),
    set_dict(board, Player, NewBoard, NewPlayer).

update_wall(Player, Tile, NewPlayer) :-
    % writeln(["Adding ", Tile, " to the player wall"]),
    find_dict(wall, Player, Wall),
    add(Wall, 1, Tile, NewWall),
    set_dict(wall, Player, NewWall, NewPlayer).

penalize(Player, Amount, NewPlayer, Test) :-
    Amount<0,
    find_dict(penalties, Player, Penalties),
    length(Penalties, Sz),
    Sz>0, !,
    append([P1], R, Penalties),

    if_print(Test,["Player recive ", P1, " of penalization"]),
    set_dict(penalties, Player, R, TempPlayer1),
    find_dict(score, Player, Score),
    NewScore is Score+P1,
    set_dict(score, TempPlayer1, NewScore, TempPlayer2),
    Times is Amount+1,
    penalize(TempPlayer2, Times, NewPlayer, Test).
penalize(Player, _, Player, _).

update_player(Player, Game, L:F:Color, NewPlayer, Return, FinalPlayer, Test) :-
    update_line(Player, Game, L:F:Color, TempPlayer0, Diff, Amount, Test),
    find_dict(board, TempPlayer0, Board),
    verify_lines(TempPlayer0, Board:unsorted, FinalPlayer),
    Return is Amount-Diff,
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

basic(Game, Player, NewGame, NewPlayer, A) :-
    valid_choices(Game, Player, [A|_]), !,
    update_player(Player, Game, A, NewPlayer, Return, _, 1),
    update_game(Game, A, NewGame, Return).
basic(Game, Player, NewGame, NewPlayer, none:Id:Color) :-
    available_colors(Game, [Amount:Id:Color | _]), !,
    update_game(Game, none:Id:Color, NewGame, Amount),
    Neg is Amount* -1,
    penalize(Player, Neg, NewPlayer,1).
basic(Game, Player, Game, Player, none:none:none).

greedy(Game, Player, NewGame, NewPlayer, A) :-
    valid_choices(Game, Player, Choices), !,
    findall(Score:Choice, (
        member(Choice, Choices),
        update_player(Player, Game, Choice, _, _, TempPlayer, 0),
        find_dict(score, TempPlayer, Score)
    ), Options),
    sort(Options, Sorted),
    append(_, [_:A], Sorted),
    update_player(Player, Game, A, NewPlayer, Return, _, 1),
    update_game(Game, A, NewGame, Return).
greedy(Game, Player, NewGame, NewPlayer, none:Id:Color) :-
    available_colors(Game, Choices), !,
    sort(Choices, [Amount:Id:Color|_]),
    update_game(Game, none:Id:Color, NewGame, Amount),
    Neg is Amount* -1,
    penalize(Player, Neg, NewPlayer,1).
greedy(Game, Player, Game, Player, none:none:none).

empty_board(Data:board) :-
    add([], 5, 1, List),
    enumerate(List, 1, Enum),
    tiles_colors(C),
    findall([New:stocks, C:valid, C:all]:Sz, (
        find_dict(Sz, Enum, _),
        add([], Sz, empty, New)
    ), Data).

new_players(Amount, Players:players) :-
    empty_board(Board),
    penalization_list(Penalties),
    add([], Amount, [Board, Penalties, []:wall, 0:score], List),
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
    write(Fd,["Player ", Id, " turn start --------------------"]),
    nl(Fd),
    close(Fd),
    Choice=..[St, Game, P1, TempGame1, NewP1, Lid:Fid:Color],
    Choice,
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
        find_dict(stocks, X, Stocks),
        Times is 5 - Id,
        add(Stocks, Times, ' - ', Line)
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
    column_of(X,C,Y),
    append(Ac, [C], NewAc),
    NewY is Y+1,
    generate_wall((X,NewY), Table, NewAc, R).
generate_wall((X,Y), Table, Ac, R) :-
    append(Ac, [' - '], NewAc),
    NewY is Y+1,
    generate_wall((X,NewY), Table, NewAc, R).

print_wall(Player:wall) :-
    open("log.txt", append, Fd),
    write(Fd, "Wall of Player: \n"),
    nl(Fd),
    % write(Fd, Player),
    find_dict(wall, Player, Wall),
    sort(Wall, SortedWall),
    generate_wall((1,1), SortedWall ,[], FinalWall),
    findall(FinalWall, (
        member(X, FinalWall),
        write(Fd,X),
        nl(Fd)
    ), _),
    % write(Fd, FinalWall),
    nl(Fd),
    close(Fd).

clean_players(Game, NewGame) :-
    find_dict(players, Game, Players),
    findall(Player:Id, (
        member(X:Id, Players),
        find_dict(board, X, Board),
        verify_lines(X, Board:unsorted, CleanedPlayer),
        % writeln([[CleanedPlayer:player, Id:id]:player]),
        penalization_list(Penalizations),
        set_dict(penalization, CleanedPlayer, Penalizations, NewPlayer),
        find_dict(score, NewPlayer, CurScore),
        Score is max(CurScore, 0),
        set_dict(score, NewPlayer, Score, Player)
    ), NewPlayers),
    set_dict(players, Game, NewPlayers, NewGame).

verify_lines(P, [], P).
verify_lines(Player, [_:Line|Lines], NewPlayer) :-
    clean_line(Player, Line, CurPlayer), !,
    verify_lines(CurPlayer, Lines, NewPlayer).
verify_lines(Player, [_|Lines], NewPlayer) :-
    verify_lines(Player, Lines, NewPlayer).
verify_lines(Player, Lines:unsorted, NewPlayer) :-
    indexed_sort(Lines, Sorted),
    verify_lines(Player, Sorted, NewPlayer).
