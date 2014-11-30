-module(people_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include_lib("include/katas.hrl").

male_and_over_forty_1_function_test() ->
  People = [{"Federico", $M, 22},
            {"Kim", $F, 45},
            {"Hansa", $F, 30},
            {"Tran", $M, 47},
            {"Cathy", $F, 32},
            {"Elias", $M, 50}],
  {"male_and_over_forty/1 tests", [
      ?assertEqual(people:male_and_over_forty(People), ["Tran", "Elias"])
      ]}.

male_or_over_forty_1_function_test() ->
  People = [{"Federico", $M, 22},
            {"Kim", $F, 45},
            {"Hansa", $F, 30},
            {"Tran", $M, 47},
            {"Cathy", $F, 32},
            {"Elias", $M, 50}],
  {"male_or_over_forty/1 tests", [
      ?assertEqual(people:male_or_over_forty(People), ["Federico", "Kim", "Tran", "Elias"])
      ]}.

-endif.
