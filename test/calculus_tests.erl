-module(calculus_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include_lib("include/katas.hrl").

derivative_0_function_test() ->
  F1 = fun(X) -> X * X end,
  {"derivative/0 tests", [
      ?assert(?delta(calculus:derivative(F1, 3), 6.000000496442226) =< 0.0001),
      ?assert(?delta(calculus:derivative(fun(X) -> 3 * X * X + 2 * X + 1 end, 5), 32.00000264769187) =< 0.0001),
      ?assert(?delta(calculus:derivative(fun math:sin/1, 0), 1.0) =< 0.0001)
      ]}.

-endif.
