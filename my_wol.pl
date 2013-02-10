:- ensure_loaded('war_of_life.pl').

% PART 1

% Run N tests with the given strategies, and collate and print the results.

test_strategy(N, P1, P2) :-
    statistics(runtime, [Start|_]),

    run_tests(N, P1, P2, ResultList),

    statistics(runtime, [End|_]),
    Time is End - Start,
    format('Testing finished in ~3d seconds.\n', [Time]),

    AverageTime is Time / N,
    format('Average game time is ~3d seconds.\n', [AverageTime]),

    findall(Elem, nth1(Index, ResultList, [draw, _]), Draws),
    length(Draws, NumDraws),
    format('There were ~d draws.\n', [NumDraws]),

    findall(Elem, nth1(Index, ResultList, [r, _]), Reds),
    length(Reds, NumReds),
    format('There were ~d red wins.\n', [NumReds]),

    findall(Elem, nth1(Index, ResultList, [b, _]), Blues),
    length(Blues, NumBlues),
    format('There were ~d blue wins.\n', [NumBlues]),

    get_second(ResultList, NumMoves),
    delete(NumMoves, 250, NonExaustiveMoves),

    max_member(LongestGame, NonExaustiveMoves),
    format('The longest game was ~d moves.\n', [LongestGame]),

    min_member(ShortestGame, NonExaustiveMoves),
    format('The shortest game was ~d moves.\n', [ShortestGame]),

    sumlist(NumMoves, TotalMoves),
    length(NumMoves, LengthNumMoves),
    AverageLength is TotalMoves / LengthNumMoves,
    format('The average game length was ~3f moves.\n', [AverageLength]).


% Run N games and return the game results in a list

run_tests(0, _, _, []) :- !.
run_tests(N, P1, P2, ResultList) :-
    play(quiet, P1, P2, NumMoves, Winner),
    M is N - 1,
    run_tests(M, P1, P2, OldList),
    append(OldList, [[Winner, NumMoves]], ResultList).


% Get all X from [[_, X], ..., [_, X]]

get_second([], []).
get_second([[_, S] | Rest], [S | ResRest]) :-
    get_second(Rest, ResRest).



% PART 2

% Return a list of valid moves for player Player in state Board. The list is
% given in the form [[OldX, OldY, NewX, NewY],...]

get_valid_moves(Player, Board, PossMoves) :-
    findall(
        [OldX, OldY, NewX, NewY],
        (
            what_in_cell(Board, OldX, OldY, Player),
            neighbour_position(OldX, OldY, [NewX, NewY]),
            is_empty(NewX, NewY, Board)
        ),
        PossMoves
    ).

is_empty(X, Y, [Blues, Reds]) :-
    \+ member([X, Y], Blues),
    \+ member([X, Y], Reds).
