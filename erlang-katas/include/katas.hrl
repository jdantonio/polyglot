-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(switch(Condition, True, False),
        case Condition of true -> True; false -> False end).

-define(delta(A, B), katas:delta(A, B)).

-define(type_of(X), katas:type_of(X)).
