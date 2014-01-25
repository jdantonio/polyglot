-module(geom_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include_lib("include/katas.hrl").

area_1_function_test() ->
  {"area/1 function tests", [
      ?assert(geom:area({rectangle, 7, 3}) == 21),
      ?assert(?delta(geom:area({triangle, 7, 3}), 10.5) =< 0.1),
      ?assert(?delta(geom:area({ellipse, 7, 3}), 65.97344572538566) =< 0.001)
      ]}.

area_2_function_test() ->
  {"area/2 function tests", [
      ?assert(geom:area(3, 4) == 12),
      ?assert(geom:area(12, 7) == 84),
      ?assert(geom:area(3.0, 4.0) == 12.0),
      ?assert(geom:area(12.0, 7.0) == 84.0)
      ]}.

area_3_function_test() ->
  {"area/3 rectangle tests", [
      ?assert(geom:area(rectangle, 3, 4) == 12)
      ]},
  {"area/3 triangle tests", [
      ?assert(?delta(geom:area(triangle, 3, 5), 7.5) =< 0.1)
      ]},
  {"area/3 ellipse tests", [
      ?assert(?delta(geom:area(ellipse, 2, 4), 25.132741228718345) =< 0.001)
      ]},
  {"area/3 catch-all tests", [
      ?assert(geom:area(pentagon, 3, 4) == 0),
      ?assert(geom:area(hexagon, -1, 5) == 0),
      ?assert(geom:area(rectangle, 1, -3) == 0)
      ]}.

-endif.
