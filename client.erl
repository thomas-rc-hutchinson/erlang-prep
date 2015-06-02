-module(client).
-export([permutations/1, nth_root/2, floor/1, 
    no_decimal_places/1, task/4, print/1, printList/1, number_size/1, ar/3, arit/2, start_task/4,
    start_multiple_task/6, remove_dups/1,cubes_from_permutations/1]).
-import(io, [format/1, format/2]).
-import(lists, [seq/2]).


%41063625
% 41063621 410636256
% {100025,[{512,8.0},{125000,50.0},{512000,80.0}]}

%client:cubes_from_permutations(41063625).
cubes_from_permutations(Number) -> 
    [{PermNum,  nth_root(3,PermNum)} || PermNum <- permutations(Number), no_decimal_places(nth_root(3,PermNum))].

permutations(Number) when is_integer(Number) -> 
    NumberSize = number_size(Number),
    [list_to_integer(Perm) || Perm <- remove_dups(permutations(integer_to_list(Number))), number_size(list_to_integer(Perm)) == NumberSize];

permutations([]) -> [[]];
permutations(L)  -> [[H|T] || H <- L, T <- permutations(L--[H])].


task(End, End, Id, ParentPid) -> print(Id), ParentPid ! {Id, false};  
task(Counter, End, Id, ParentPid) ->
    %Cubes = cubes_from_permutations(Counter),   %% TRY MAKING TAIL RECURSIVE
    Switch = no_decimal_places(nth_root(3,Counter)),
    case Switch of
        false -> task(Counter + 1, End, Id, ParentPid);
        true ->
            case length(cubes_from_permutations(Counter)) == 5 of 
                true -> print(Id), ParentPid ! {Id, true, Counter, cubes_from_permutations(Counter)};
                false -> task(Counter + 1, End, Id, ParentPid)
        end        
    end.

%client:start_task(41063625, 41063626, 1, self())
start_task(Start, Finish, Id, ParentPid) -> spawn(?MODULE, task, [Start, Finish, Id, ParentPid]).

%Shell got {411,true,41063625,
%           [{56623104,384.0},{41063625,345.0},{66430125,405.0}]}

%client:start_multiple_task(1,100,1,41063626,self(),[]).
%LONGER -> client:start_multiple_task(41062603,10,1,41063626,self(),[]).
%client:start_multiple_task(41063603,10,1,41063626,self(),[]).
start_multiple_task(Counter, Amount, Id, Limit, ParentPid, TaskPids) when Counter >= Limit -> print(Id), TaskPids;
start_multiple_task(Counter, Amount, Id, Limit, ParentPid, TaskPids) 
    -> print(Id), start_multiple_task(Counter+Amount, Amount, Id+1, Limit, ParentPid, 
        [start_task(Counter, Counter+Amount, Id, ParentPid)|TaskPids]).


%utils
number_size(I) when is_integer(I) -> length(integer_to_list(I)). 
no_decimal_places(N) -> N == floor(N).
remove_dups(L) -> sets:to_list(sets:from_list(L)).

floor(X) when X < 0 ->
    T = trunc(X),
    case X - T == 0 of
        true -> T;
        false -> T - 1
    end;
floor(X) -> 
    trunc(X).



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


printList(N) -> [print(X) || X <- N].
print(N) -> io:format("~B~n", [N]).

ar(Finish, Finish, Array) -> print(array:size(Array)), arit(Array, 1);
ar(C, Finish, Array) -> ar(C+1, Finish, array:set(C, false, Array)).
arit(Array, 41063625) -> ok;
arit(Array, Counter) -> array:get(Counter, Array), arit(Array, Counter+1).




% [1,2,3]-[2,3]-[3]













