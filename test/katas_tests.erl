-module(katas_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include_lib("include/katas.hrl").

delta_function_test() ->
  {"delta/2 function tests", [
      ?assert(katas:delta(nil, nil) == 0),
      ?assert(katas:delta(1, nil) == 1),
      ?assert(katas:delta(-1, nil) == 1),
      ?assert(katas:delta(nil, 1) == 1),
      ?assert(katas:delta(nil, -1) == 1),
      ?assert(katas:delta(5, 5) == 0),
      ?assert(katas:delta(5, 10) == 5),
      ?assert(katas:delta(-5, 5) == 10),
      ?assert(katas:delta(-5, -5) == 0),
      ?assert(katas:delta(-5, -10) == 5),
      ?assert(katas:delta(5, -5) == 10)
      ]}.

type_of_test() ->
  {"type_of/1 function tests", [
      ?assert(katas:type_of(an_atom) == atom),
      ?assert(katas:type_of(1) == integer),
      ?assert(katas:type_of(1.0) == float),
      ?assert(katas:type_of(true) == boolean),
      ?assert(katas:type_of([]) == list),
      ?assert(katas:type_of({}) == tuple),
      ?assert(katas:type_of(<<1>>) == binary),
      ?assert(katas:type_of(<<1:1>>) == bitstring),
      ?assert(katas:type_of(spawn(fun() -> nil end)) == pid),
      ?assert(katas:type_of(make_ref()) == reference),
      %?assert(katas:type_of(1) == port),
      %?assert(katas:type_of(1) == unknown),
      ?assert(katas:type_of(fun() -> nil end) == function)
      ]}.

delta_macro_test() ->
  {"delta/2 macro tests", [
      ?assert(?delta(nil, nil) == 0),
      ?assert(?delta(1, nil) == 1),
      ?assert(?delta(-1, nil) == 1),
      ?assert(?delta(nil, 1) == 1),
      ?assert(?delta(nil, -1) == 1),
      ?assert(?delta(5, 5) == 0),
      ?assert(?delta(5, 10) == 5),
      ?assert(?delta(-5, 5) == 10),
      ?assert(?delta(-5, -5) == 0),
      ?assert(?delta(-5, -10) == 5),
      ?assert(?delta(5, -5) == 10)
      ]}.

-endif.
