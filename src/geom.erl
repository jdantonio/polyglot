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

area(Length, Width) when is_integer(Length), is_integer(Width) ->
  Length * Width;
area(Length, Width) when is_float(Length), is_float(Width) ->
  Length * Width.

%% @doc Calculates the area of a shape from the two relevant dimensions.
%% Returns the area of known shapes else returns zero.

-spec(area(atom(), number(),number()) -> number()).

area(rectangle, Length, Width) when Length >= 0, Width >= 0 ->
  area(Length, Width);
area(triangle, Base, Height) when Base >= 0, Height >= 0 ->
  Base * Height / 2.0;
area(ellipse, MajorRadius, MinorRadius) when MajorRadius >= 0, MinorRadius >= 0 ->
  math:pi() * MajorRadius * MinorRadius;
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
