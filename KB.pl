:- dynamic position/3.  % position(Type, X, Y)

init_state(Grid) :-
    retractall(position(_, _, _)),
    grid_to_positions(Grid, 0).

grid_to_positions([], _).
grid_to_positions([Row|Rest], RowNum) :-
    process_row(Row, RowNum, 0),
    NextRow is RowNum + 1,
    grid_to_positions(Rest, NextRow).

process_row([], _, _).
process_row([Cell|Rest], RowNum, ColNum) :-
    atom_string(CellAtom, Cell),
    add_position(CellAtom, RowNum, ColNum),
    NextCol is ColNum + 1,
    process_row(Rest, RowNum, NextCol).

add_position('B', X, Y) :- assertz(position(bird, X, Y)).
add_position('P', X, Y) :- assertz(position(pig, X, Y)).
add_position('R', X, Y) :- assertz(position(rock, X, Y)).
add_position('T', _, _).

move(right, 0, 1, 3).
move(up, -1, 0, 0).
move(down, 1, 0, 1).
move(left, 0, -1, 2).

valid_position(X, Y) :-
    X >= 0, X < 8,
    Y >= 0, Y < 8.

manhattan_distance(X1, Y1, X2, Y2, Distance) :-
    DX is abs(X2 - X1),
    DY is abs(Y2 - Y1),
    Distance is DX + DY.

valid_move(X, Y, NewX, NewY) :-
    valid_position(NewX, NewY),
    \+ position(rock, NewX, NewY).

choose_best_action(X, Y, Action) :-
    position(pig, PigX, PigY),
    findall(
        (Dist, A),
        (move(_, DX, DY, A),
         NewX is X + DX,
         NewY is Y + DY,
         valid_move(X, Y, NewX, NewY),
         manhattan_distance(NewX, NewY, PigX, PigY, Dist)),
        Moves
    ),
    sort(Moves, [(_, Action)|_]),
    !.

get_next_action(Grid, Action) :-
    init_state(Grid),
    position(bird, BirdX, BirdY),
    choose_best_action(BirdX, BirdY, Action),
    !.