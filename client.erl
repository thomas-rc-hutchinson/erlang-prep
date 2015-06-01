-module(client).
-export([perms/1, nth_root/2, floor/1, t/0, 
    is_whole/1, t/1, task/2, d/1, ld/1, s/1, ar/3, arit/2]).
-import(io, [format/1, format/2]).
-import(lists, [seq/2]).





t() -> [{X, nth_root(3,X)} || X <- remove_dups(perms(41063625)), is_whole(nth_root(3, X)) ].
t(N) -> [{X, nth_root(3,X)} || X <- remove_dups(perms(N)), is_whole(nth_root(3, X))].


ld(N) -> [d(X) || X <- N].
d(N) -> io:format("~B~n", [N]).


% {100025,[{512,8.0},{125000,50.0},{512000,80.0}]}


ar(Finish, Finish, Array) -> d(array:size(Array)), arit(Array, 1);
ar(C, Finish, Array) -> ar(C+1, Finish, array:set(C, false, Array)).

arit(Array, 41063625) -> ok;
arit(Array, Counter) -> array:get(Counter, Array), arit(Array, Counter+1).


task(End, End) -> exit(1);
task(Counter, End) ->
    d(Counter),
    Cubes = t(Counter),
    case length(Cubes) == 3 of 
        true -> {Counter, Cubes};
        false -> task(Counter + 1, End)
    end.



s(I) when is_integer(I) -> length(integer_to_list(I)). 


is_whole(N) -> N == floor(N).
remove_dups(L) -> sets:to_list(sets:from_list(L)).




floor(X) when X < 0 ->
    T = trunc(X),
    case X - T == 0 of
        true -> T;
        false -> T - 1
    end;
floor(X) -> 
    trunc(X).







perms(V) when is_integer(V) -> 
    [list_to_integer(L) || L <- perms(integer_to_list(V)), s(list_to_integer(L)) == s(V)];

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













