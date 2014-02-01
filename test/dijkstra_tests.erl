-module(dijkstra_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include_lib("include/katas.hrl").

gcd_2_function_test() ->
  {"gcd/2 returns the greatest common denominator of the two given numbers", [
      ?assert(dijkstra:gcd(12, 8) == 4),
      ?assert(dijkstra:gcd(14, 21) == 7),
      ?assert(dijkstra:gcd(125, 46) == 1),
      ?assert(dijkstra:gcd(120, 36) == 12)
      ]}.

-endif.
