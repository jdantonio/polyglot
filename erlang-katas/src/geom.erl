%% @author Jerry D'Antonio
%% @doc Geometry functions.
%% @version 0.1

-module(geom).

-export([
  area/1,
  area/2,
  area/3
  ]).

%% @doc Calculates the area of a shape from the two relevant dimensions.
%% Returns the area of known shapes else returns zero.

-spec(area({atom(), number(),number()}) -> number()).

area({Shape, A, B}) ->
  area(Shape, A, B).

%% @doc Calculates the area of a rectangle from the length and width.
%% Returns the area.

-spec(area(number(),number()) -> number()).

area(Length, Width) when is_number(Length), is_number(Width) ->
  Length * Width.

%% @doc Calculates the area of a shape from the two relevant dimensions.
%% Returns the area of known shapes else returns zero.

-spec(area(atom(), number(),number()) -> number()).

area(rectangle, L, W) when is_number(L), is_number(W), L >= 0, W >= 0 ->
  area(L, W);
area(triangle, B, H) when is_number(B), is_number(H), B >= 0, H >= 0 ->
  B * H / 2.0;
area(ellipse, Major, Minor) when is_number(Major), is_number(Minor), Major >= 0, Minor >= 0 ->
  math:pi() * Major * Minor;
area(_, _, _) -> 0.

% alternate implementation of area/3 using a case statement

%area(Shape, A, B) ->
  %case Shape of
    %rectangle when A >= 0, B >= 0 ->
      %area(A, B);
    %triangle when A >= 0, B >= 0 ->
      %A * B / 2.0;
    %ellipse when A >= 0, B >= 0 ->
      %math:pi() * A * B;
    %_Else -> 0
  %end.
