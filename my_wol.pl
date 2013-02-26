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

    findall(Elem, nth1(Index, ResultList, [b, _]), Blues),
    length(Blues, NumBlues),
    format('There were ~d blue wins.\n', [NumBlues]),

    findall(Elem, nth1(Index, ResultList, [r, _]), Reds),
    length(Reds, NumReds),
    format('There were ~d red wins.\n', [NumReds]),

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


run(N) :-
    test_strategy(N, random, minimax),
    write('\n'),
    test_strategy(N, bloodlust, minimax),
    write('\n'),
    test_strategy(N, self_preservation, minimax),
    write('\n'),
    test_strategy(N, land_grab, minimax),
    write('\n'),
    test_strategy(N, minimax, random),
    write('\n'),
    test_strategy(N, minimax, bloodlust),
    write('\n'),
    test_strategy(N, minimax, self_preservation),
    write('\n'),
    test_strategy(N, minimax, land_grab),
    write('\n'),
    test_strategy(N, minimax, minimax).

% PART 2

% Return a list of valid moves for player Player in state Board. The list is
% given in the form [[OldX, OldY, NewX, NewY],...]

get_valid_moves(Player, Board, PossMoves) :-
    findall(
        [OldX, OldY, NewX, NewY],
        (
            cell(OldX, OldY),
            what_in_cell(Board, OldX, OldY, Player),
            neighbour_position(OldX, OldY, [NewX, NewY]),
            is_empty(NewX, NewY, Board)
        ),
        PossMoves
    ).

is_empty(X, Y, [Blues, Reds]) :-
    \+ member([X, Y], Blues),
    \+ member([X, Y], Reds).


% simulate a move on the given board

simulate_move(b, Move, [Blues, Reds], [NewBlues, Reds]) :-
    alter_board(Move, Blues, NewBlues).

simulate_move(r, Move, [Blues, Reds], [Blues, NewReds]) :-
    alter_board(Move, Reds, NewReds).


% Factored out the middle of multiple strategies, as they're all the same

simulate_move_and_crank(Player, PossMoves, Move, Orig, AfterMove, Result) :-
    member(Move, PossMoves),
    simulate_move(Player, Move, Orig, AfterMove),
    next_generation(AfterMove, Result).


% Comparison predicate for comparing MoveOptions and DiffOptions

comparsion([NumOp1, _, _], [NumOp2, _, _]) :-
    NumOp1 < NumOp2.


% Get the cells we are interested in depending on the strategy

get_interesting(Player, bloodlust, [BlueState, RedState], Op) :-
    (Player == b) -> (Op = RedState) ; (Op = BlueState).

get_interesting(Player, self_preservation, [BlueState, RedState], Ours) :-
    (Player == b) -> (Ours = BlueState) ; (Ours = RedState).

get_interesting(Player, land_grab, [BlueState, RedState], [Ours, Ops]) :-
    (Player = b) -> (Ours = BlueState, Ops = RedState) ;
                    (Ours = RedState, Ops = BlueState).


% evaluate the heuristic depending on the strategy

evaluate_heuristic(bloodlust, Op, Num) :- length(Op, Num).
evaluate_heuristic(self_preservation, Ours, Num) :- length(Ours, Num).
evaluate_heuristic(land_grab, Both, Diff) :- land_grab_heuristic(Both, Diff).

land_grab_heuristic([Ours, Ops], Diff) :-
    length(Ours, NumOurs),
    length(Ops, NumOps),
    Diff is NumOurs - NumOps.

% Cut out the code duplication

get_move_options(Player, State, Strategy, MoveOptions) :-
    get_valid_moves(Player, State, PossMoves),

    findall(
        [Num, Move, AfterMove],
        (
            simulate_move_and_crank(Player, PossMoves, Move, State, AfterMove,
                                    NewState),
            (
                get_interesting(Player, Strategy, NewState, Op)
            ),
            evaluate_heuristic(Strategy, Op, Num)
        ),
        MoveOptions
    ).


% Make a move using the bloodlust strategy
% Find all valid moves, put the results in MoveOptions, and pick the best

bloodlust(Player, State, New, NewMove) :-
    get_move_options(Player, State, bloodlust, MoveOptions),
    min_member(comparsion, [_, NewMove, New], MoveOptions).


% Make a move using the self preservation strategy, very similar to bloodlust

self_preservation(Player, State, New, NewMove) :-
    get_move_options(Player, State, self_preservation, MoveOptions),
    max_member(comparsion, [_, NewMove, New], MoveOptions).


% Make a move using the land grab strategy. Slightly more complex heuristic,
% wins well against the two strategies above.

land_grab(Player, State, FinalState, FinalMove) :-
    get_move_options(Player, State, land_grab, MoveOptions),
    max_member(comparsion, [_, FinalMove, FinalState], MoveOptions).


op(b, r).
op(r, b).


% Play a move using the minimax strategy. The minimax function gets all the
% possible moves for Player, then the minimize function checks all those moves
% from the perpective of Op and returns a list with the differences.

minimax(Player, State, FinalState, FinalMove) :-
    get_move_options(Player, State, land_grab, MoveOptions),
    op(Player, Op),
    minimize(Op, MoveOptions, Result),
    max_member(comparsion, [_, FinalMove, FinalState], Result).


minimize(_, [], []).
minimize(Player, [[OldDiff, Move, AfterMove] | Rest], [Res | Results]) :-
    get_move_options(Player, AfterMove, land_grab, MoveOptions),
    max_member(comparsion, [WorstDiff, _, _], MoveOptions),

    NewDiff is OldDiff - WorstDiff,
    Res = [NewDiff, Move, AfterMove],
    minimize(Player, Rest, Results).
