:- ensure_loaded('war_of_life.pl').


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
    delete(NumMoves, 250, NonExaustiveMoves).

    %max_list(NonExaustiveMoves, LongestGame),
    %format('The longest game was ~d moves', [LongestGame]),

    %min_list(NonExaustiveMoves, ShortestGame),
    %format('The shortest game was ~d moves', [ShortestGame]).



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
