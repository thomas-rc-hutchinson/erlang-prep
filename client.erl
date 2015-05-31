-module(client).
-export([perms/1, task/3, nth_root/2, floor/1]).
-import(io, [format/1, format/2]).
-import(lists, [seq/2]).


task(End, End, Acc) -> Acc;
task(Pos, End, Acc) -> task(Pos+1, End, [perms(Pos)|Acc]).


valid(V) -> float_to_list(V).


floor(X) when X < 0 ->
    T = trunc(X),
    case X - T == 0 of
        true -> T;
        false -> T - 1
    end;
floor(X) -> 
    trunc(X).


perms(V) when is_integer(V) -> [list_to_integer(L) || L <- perms(integer_to_list(V))];
perms([]) -> [[]];
perms(L)  -> [[H|T] || H <- L, T <- perms(L--[H])].


nth_root(N, X) -> nth_root(N, X, 1.0e-5).
nth_root(N, X, Precision) ->
    F = fun(Prev) -> ((N - 1) * Prev + X / math:pow(Prev, (N-1))) / N end,
    fixed_point(F, X, Precision).

fixed_point(F, Guess, Tolerance) ->
    fixed_point(F, Guess, Tolerance, F(Guess)).
fixed_point(_, Guess, Tolerance, Next) when abs(Guess - Next) < Tolerance ->
    Next;
fixed_point(F, _, Tolerance, Next) ->
    fixed_point(F, Next, Tolerance, F(Next)).



% [1,2,3]-[2,3]-[3]













