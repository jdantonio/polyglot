-module(katas_tests).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-include_lib("include/katas.hrl").

switch_macro_true_test() ->
  [Low, High] = [1, 2],
  {"?switch/3 returns the true clause when the condition is true", [
      ?assert(?switch(Low < High, foo, bar) == foo)
      ]}.

switch_macro_false_test() ->
  [Low, High] = [1, 2],
  {"?switch/3 returns the false clause when the condition is false", [
      ?assert(?switch(Low > High, foo, bar) == bar)
      ]}.

delta_function_test() ->
  {"?delta/2 returns the absolute value of the difference between two numbers", [
      ?assert(katas:delta(undefined, undefined) == 0),
      ?assert(katas:delta(1, undefined) == 1),
      ?assert(katas:delta(-1, undefined) == 1),
      ?assert(katas:delta(undefined, 1) == 1),
      ?assert(katas:delta(undefined, -1) == 1),
      ?assert(katas:delta(5, 5) == 0),
      ?assert(katas:delta(5, 10) == 5),
      ?assert(katas:delta(-5, 5) == 10),
      ?assert(katas:delta(-5, -5) == 0),
      ?assert(katas:delta(-5, -10) == 5),
      ?assert(katas:delta(5, -5) == 10)
      ]}.

type_of_test() ->
  {"type_of/1 returns an atom representing the type of the given variable", [
      ?assert(katas:type_of(an_atom) == atom),
      ?assert(katas:type_of(1) == integer),
      ?assert(katas:type_of(1.0) == float),
      ?assert(katas:type_of(true) == boolean),
      ?assert(katas:type_of([]) == list),
      ?assert(katas:type_of({}) == tuple),
      ?assert(katas:type_of(<<1>>) == binary),
      ?assert(katas:type_of(<<1:1>>) == bitstring),
      ?assert(katas:type_of(spawn(fun() -> undefined end)) == pid),
      ?assert(katas:type_of(make_ref()) == reference),
      %?assert(katas:type_of(1) == port),
      %?assert(katas:type_of(1) == undefined),
      ?assert(katas:type_of(fun() -> undefined end) == function)
      ]}.

delta_macro_test() ->
  {"?delta/2 returns the absolute value of the difference between two numbers", [
      ?assert(?delta(undefined, undefined) == 0),
      ?assert(?delta(1, undefined) == 1),
      ?assert(?delta(-1, undefined) == 1),
      ?assert(?delta(undefined, 1) == 1),
      ?assert(?delta(undefined, -1) == 1),
      ?assert(?delta(5, 5) == 0),
      ?assert(?delta(5, 10) == 5),
      ?assert(?delta(-5, 5) == 10),
      ?assert(?delta(-5, -5) == 0),
      ?assert(?delta(-5, -10) == 5),
      ?assert(?delta(5, -5) == 10)
      ]}.

-endif.
