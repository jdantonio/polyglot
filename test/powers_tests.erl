-module(powers_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include_lib("include/katas.hrl").

raise_2_function_test() ->
  {"raise/2 raises the given number to the given exponent", [
      ?assert(powers:raise(5, 1) == 5),
      ?assert(?delta(powers:raise(2, 3), 8) =< 0.1),
      ?assert(?delta(powers:raise(1.2, 3), 1.728) =< 0.1),
      ?assert(powers:raise(2, 0) == 1),
      ?assert(?delta(powers:raise(2, -3), 0.125) =< 0.1)
      ]}.

nth_root_2_function_test() ->
  {"nth_root/2 returns the given root of the given number", [
      ?assert(?delta(powers:nth_root(27, 3), 3.0) =< 0.00000000000001)
      ]}.

-endif.
