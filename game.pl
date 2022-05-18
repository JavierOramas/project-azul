:- module(game, [
    fill_factory/3
    ]).

% import modules
:- use_module([tools]).

% predicate for initial player
:- (dynamic initial_player/1).

% Fill the factories with the tiles
% if there are not factories to fill, filled factories is [].
% if there are not tiles to fill, filled factories stay the same.
% if the fisrt facory is empty, and the first set of tiles is empty, skip to the next factory.
% else take the first tile, put it in the first factory, and continue to the next tile.
fill_factory([], _, []).
fill_factory(_, [], _).
fill_factory([[]|Factories], [[]|Tiles], [[]|NewFactories]):-
    fill_factory(Factories, Tiles, NewFactories).
fill_factory([[_|F1]|Factories], [Tile1|Tiles], [[Tile1|Rest]|NewFactories]):-
    fill_factory([F1|Factories], Tiles, [Rest|NewFactories]).

% Verify that the factories are full
verify_game(Game, NewGame):-
    find_dict(ammounts, Game, Ammounts),
    find_dict(factories, Fame, Factories),
    length(Factories, FacCapacity),
    findall(X, member(X:_, Ammounts), Quantities),
    sum_list(Quantities, Sum),

    % Check if there are enough Tiles to fill th Factories
    Sum < FacCapacity * 4, !,
    find_dict(outs, Game, Outs),

    findall(RealAmmount:Color,
        (
            find_dict(Color, Ammounts, QAmmount),
            find_dict(Color, Outs, QOut),
            RealAmmount is QOut+QAmmount
            ),
        NewAmmounts
    ),
    alter_dict(ammounts, Game, NewAmmounts, TempGame),

    %
