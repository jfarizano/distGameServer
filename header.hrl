-define(PORT, 8000).
-define(CLEANBOARD, {empty, empty, empty,
                     empty, empty, empty,
                     empty, empty, empty}).
-record(game, {p1, p2, obs, board, turn, moves, updaters}).