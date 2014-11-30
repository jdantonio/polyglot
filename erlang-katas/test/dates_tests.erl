-module(dates_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include_lib("include/katas.hrl").

date_parts_1_function_test() ->
  {"date_parts/1 returns a list of three integers: Year, Month, Day", [
      ?assertEqual(dates:date_parts("2014-01-31"), [2014, 1, 31]),
      ?assertEqual(dates:date_parts("1970-01-01"), [1970, 1, 1]),
      ?assertEqual(dates:date_parts("1999-12-31"), [1999, 12, 31])
      ]}.

is_leap_year_1_function_test() ->
  {"is_leap_year/1 returns true or false indicating whether the given year is a leap year", [
      ?assert(dates:is_leap_year(2000)),
      ?assertNot(dates:is_leap_year(2001)),
      ?assertNot(dates:is_leap_year(2002)),
      ?assertNot(dates:is_leap_year(2003)),
      ?assert(dates:is_leap_year(2004)),
      ?assertNot(dates:is_leap_year(2005)),
      ?assertNot(dates:is_leap_year(2006)),
      ?assertNot(dates:is_leap_year(2007)),
      ?assert(dates:is_leap_year(2008)),
      ?assertNot(dates:is_leap_year(2009)),
      ?assertNot(dates:is_leap_year(2010)),
      ?assertNot(dates:is_leap_year(2011)),
      ?assert(dates:is_leap_year(2012)),
      ?assertNot(dates:is_leap_year(2013)),
      ?assertNot(dates:is_leap_year(2014))
      ]}.

julian_1_function_test() ->
  {"julian/1 returns the julian date when given a string in YYYY-MM-DD format", [
      ?assertEqual(dates:julian("2012-12-31"), 366),
      ?assertEqual(dates:julian("2013-12-31"), 365),
      ?assertEqual(dates:julian("2012-02-05"), 36),
      ?assertEqual(dates:julian("2013-02-05"), 36),
      ?assertEqual(dates:julian("1900-03-01"), 60),
      ?assertEqual(dates:julian("2000-03-01"), 61),
      ?assertEqual(dates:julian("2013-01-01"), 1)
      ]}.

julian_3_function_test() ->
  {"julian/3 returns the julian date when given year, month, and date as integers", [
      ?assertEqual(dates:julian(2012, 12, 31), 366),
      ?assertEqual(dates:julian(2013, 12, 31), 365),
      ?assertEqual(dates:julian(2012, 2, 5), 36),
      ?assertEqual(dates:julian(2013, 2, 5), 36),
      ?assertEqual(dates:julian(1900, 3, 1), 60),
      ?assertEqual(dates:julian(2000, 3, 1), 61),
      ?assertEqual(dates:julian(2013, 1, 1), 1)
      ]}.

-endif.
