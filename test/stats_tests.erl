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

maximum_1_function_test() ->
  {"maximum/1 returns the largest numeric element in a list", [
      ?assert(stats:maximum([4, 1, 7, -17, 8, 2, 5]) == 8),
      ?assert(?delta(stats:maximum([52.46]), 52.46) =< 000.1),
      ?assert(stats:maximum([1, 2, 3, 4, 5, 6, 7]) == 7),
      ?assert(?delta(stats:maximum([10.0, 9.0, 8.0, 7.0, 6.0, 5.0]), 10.0) =< 000.1),
      ?assert(?delta(stats:maximum([10.0, 2, 9.0, 3, 8.0, 4, 7.0, 5]), 10.0) =< 0.001),
      ?assert(stats:maximum([10, 2.0, 9, 3.0, 8, 4.0, 7, 5.0]) == 10)
      ]}.

range_1_function_test() ->
  {"range/1 returns a list with the minimum and maximum values", [
      ?assert(stats:range([4, 1, 7, -17, 8, 2, 5]) == [-17, 8]),
      ?assert(stats:range([52.46]) == [52.46, 52.46]),
      ?assert(stats:range([52]) == [52, 52]),
      ?assert(stats:range([1, 2, 3, 4, 5, 6, 7]) == [1, 7]),
      ?assert(stats:range([10.0, 9.0, 8.0, 7.0, 6.0, 5.0]) == [5.0, 10.0]),
      ?assert(stats:range([10, 2.0, 9, 3.0, 8, 4.0, 7, 5.0]) == [2.0, 10]),
      ?assert(stats:range([10.0, 2, 9.0, 3, 8.0, 4, 7.0, 5]) == [2, 10.0])
      ]}.

mean_1_function_test() ->
  {"mean/1 returns the statistical mean of a set of numbers", [
      ?assert(?delta(stats:mean([7, 2, 9]), 6.0) =< 0.001),
      ?assert(?delta(stats:mean([7.0, 2.0, 9.0]), 6.0) =< 0.001),
      ?assert(?delta(stats:mean([7.0]), 7.0) =< 0.001)
      ]}.

stddev_1_function_test() ->
  {"stddev/1 returns the statistical stddev of a set of numbers", [
      ?assert(?delta(stats:stddev([7, 2, 9]), 3.605551275463989) =< 0.001),
      ?assert(?delta(stats:stddev([7.0, 2.0, 9.0]), 3.605551275463989) =< 0.001)
      ]}.

error_handling_test() ->
  {"stats functions properly handle bad arguments", [
      ?assertEqual(stats:minimum([]), {error, badarg}),
      ?assertEqual(stats:mean([]), {error, badarith}),
      %?assertEqual(stats:mean(["123", 456]), {error, badarith}),
      ?assertEqual(stats:stddev([]), {error, badarith})
      ]}.

-endif.
