-module(dates_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include_lib("include/katas.hrl").

date_parts_1_function_test() ->
  {"date_parts/1 returns a list of three integers: Year, Month, Day", [
      ?assert(dates:date_parts("2014-01-31") == [2014, 1, 31]),
      ?assert(dates:date_parts("1970-01-01") == [1970, 1, 1]),
      ?assert(dates:date_parts("1999-12-31") == [1999, 12, 31])
      ]}.

-endif.
