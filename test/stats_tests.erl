-module(stats_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include_lib("include/katas.hrl").

minimum_1_function_test() ->
  {"minimum/1 returns the smallest numeric element in a list", [
      ?assert(stats:minimum([4, 1, 7, -17, 8, 2, 5]) == -17),
      ?assert(?delta(stats:minimum([52.46]), 52.46) =< 000.1),
      ?assert(stats:minimum([1, 2, 3, 4, 5, 6, 7]) == 1),
      ?assert(?delta(stats:minimum([10.0, 9.0, 8.0, 7.0, 6.0, 5.0]), 5.0) =< 000.1),
      ?assert(?delta(stats:minimum([10, 2.0, 9, 3.0, 8, 4.0, 7, 5.0]), 2.0) =< 0.001),
      ?assert(stats:minimum([10.0, 2, 9.0, 3, 8.0, 4, 7.0, 5]) == 2)
      ]}.

-endif.
