-module(client).
-export([permutations/1, nth_root/2, floor/1, 
    no_decimal_places/1, task/4, print/1, printList/1, number_size/1, ar/3, arit/2, start_task/4,
    start_multiple_task/6, remove_dups/1,cubes_from_permutations/1,test_answers_found/0, go/0, answer_found/2, results_listener/1]).
-import(io, [format/1, format/2]).
-import(lists, [seq/2]).





%41063625
% 41063621 410636256
% {100025,[{512,8.0},{125000,50.0},{512000,80.0}]}

%client:cubes_from_permutations(41063625).
cubes_from_permutations(Number) -> 
    [{PermNum, nth_root(3,PermNum)} || PermNum <- permutations(Number), no_decimal_places(nth_root(3,PermNum))].

permutations(Number) when is_integer(Number) -> 
    NumberSize = number_size(Number),
    [list_to_integer(Perm) || Perm <- remove_dups(permutations(integer_to_list(Number))), number_size(list_to_integer(Perm)) == NumberSize];

permutations([]) -> [[]];
permutations(L)  -> [[H|T] || H <- L, T <- permutations(L--[H])].


task(End, End, Id, ParentPid) -> ParentPid ! {Id, false};  
task(Counter, End, Id, ParentPid) ->
    %Cubes = cubes_from_permutations(Counter),   %% TRY MAKING TAIL RECURSIVE
    Switch = no_decimal_places(nth_root(3,Counter)),
    case Switch of
        false -> task(Counter + 1, End, Id, ParentPid);
        true ->
            case length(cubes_from_permutations(Counter)) == 3 of 
                true -> ParentPid ! {Id, true, Counter, cubes_from_permutations(Counter)};
                false -> task(Counter + 1, End, Id, ParentPid)
        end        
    end.

%client:start_task(41063625, 41063626, 1, self())
start_task(Start, Finish, Id, ParentPid) -> spawn(?MODULE, task, [Start, Finish, Id, ParentPid]).

%Shell got {411,true,41063625,
%           [{56623104,384.0},{41063625,345.0},{66430125,405.0}]}


%41063625 -> client:start_multiple_task(1,500000,1,50000000,self(),[]).
%client:start_multiple_task(41063603,10,1,41063626,self(),[]).
start_multiple_task(Counter, Amount, Id, Limit, ParentPid, TaskPids) when Counter >= Limit -> start_task_println(Id), TaskPids;
start_multiple_task(Counter, Amount, Id, Limit, ParentPid, TaskPids) 
    -> start_task_println(Id), start_multiple_task(Counter+Amount, Amount, Id+1, Limit, ParentPid, 
        [start_task(Counter, Counter+Amount, Id, ParentPid)|TaskPids]).


results_listener(ResultsArray) ->
    case answer_found(1, ResultsArray) of
        {true, Id, Counter, Results} -> answer_print(Counter,Results), exit(found);
        {false} -> 
            receive 
               {Id, false} -> results_listener(array:set(Id, {false}, ResultsArray));
               {Id, true, Counter, Cubes} -> results_listener(array:set(Id, {Id, true, Counter, Cubes}, ResultsArray))
            end    
    end.


 go() ->
    RListenerPid = spawn(?MODULE, results_listener, [array:new()]),
    start_multiple_task(40000000,500000,1,42000000,RListenerPid,[]).
    


test_answers_found() ->
    Array = array:new(10),
    Array2 = array:set(1, {99, false}, Array),
    Array3 = array:set(2, {99, false}, Array2),
    Array4 = array:set(2, {66, true, 66, results}, Array3),
    answer_found(1, Array4).


answer_found(Index, ResultsArray) ->
    case array:get(Index, ResultsArray) of 
        undefined -> {false};
        {false} -> answer_found(Index+1, ResultsArray);
        {Id, true, Counter, Results} -> {true, Id, Counter, Results}
    end.




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

here() -> io:format("Here~n").

start_task_println(N) -> io:format("Starting task ~B~n", [N]).
answer_print(N, Cubes) -> io:format("Answer is ~B~n", [N]), [io:format("Number ~B Root ~B ~n", [N,floor(R)]) || {N,R} <- Cubes].
winner_print(N) -> io:format("We have a winner Id ~B Number ~B ~n", N).
noanswer_print(N) -> io:format("No answer ~B~n", [N]).

ar(Finish, Finish, Array) -> print(array:size(Array)), arit(Array, 1);
ar(C, Finish, Array) -> ar(C+1, Finish, array:set(C, false, Array)).
arit(Array, 41063625) -> ok;
arit(Array, Counter) -> array:get(Counter, Array), arit(Array, Counter+1).




% [1,2,3]-[2,3]-[3]













