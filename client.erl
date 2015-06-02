-module(client).
-export([permutations/1, nth_root/2, floor/1, 
    no_decimal_places/1, task/4, print/1, printList/1, number_size/1, start_process/4, task_await_start/4,
    start_perm_cube_processes/6, remove_duplicates/1,cubes_from_permutations/1, run/0, answer_found/2, results_listener/2]).
-import(io, [format/1, format/2]).
-import(lists, [seq/2]).


 run() ->
    %TODO ADD STDIO
    ResultsListenerPid = spawn(?MODULE, results_listener, [array:new(), 0]),
    % Create proceses and then start them all
    [Pid ! start || Pid <- start_perm_cube_processes(41063625,100000,1,42063625,ResultsListenerPid,[])].



start_perm_cube_processes(Counter, Amount, Id, Limit, ParentPid, TaskPids) when Counter >= Limit -> TaskPids;
start_perm_cube_processes(Counter, Amount, Id, Limit, ParentPid, TaskPids) 
    -> start_perm_cube_processes(Counter+Amount, Amount, Id+1, Limit, ParentPid, 
        [start_process(Counter, Counter+Amount, Id, ParentPid)|TaskPids]).

start_process(Start, Finish, Id, ParentPid) -> spawn(?MODULE, task_await_start, [Start, Finish, Id, ParentPid]).

task_await_start(Counter, End, Id, ParentPid) -> 
    receive
        start -> task(Counter, End, Id, ParentPid)
    end.

task(End, End, Id, ParentPid) -> ParentPid ! {Id, false}; %Solution not found  
task(Counter, End, Id, ParentPid) ->
    case has_nth_root(3,Counter) of
        % Counter doesn't have nth root, skip
        false -> task(Counter+1, End, Id, ParentPid);
        true ->
            % To make this tail recursive we may need to compute this twice.
            % However this is not expected to happen often.  
            case length(cubes_from_permutations(Counter)) == 3 of 
                true -> ParentPid ! {Id, true, Counter, cubes_from_permutations(Counter)};
                false -> task(Counter + 1, End, Id, ParentPid)
        end        
    end.

cubes_from_permutations(Num) -> 
    [{PermNum, nth_root(3,PermNum)} || PermNum <- permutations(Num), has_nth_root(3,PermNum)].

permutations(Num) when is_integer(Num) ->
    % Build permutations and remove duplicates to reduce processing 
    PermList = remove_duplicates(permutations(integer_to_list(Num))),
    % Some permutations have less digits than Num when the number starts with 0. Filter these out.
    NumSize = number_size(Num),
    [list_to_integer(Perm) || Perm <- PermList, length(Perm) == NumSize];

permutations([]) -> [[]];
permutations(L)  -> [[H|T] || H <- L, T <- permutations(L--[H])].


% Each cube permutation process launched has an Id associated with it.
% When the task completes a message is sent to the results_listener process.
% The results of the task are added to index Id in ResultsArray 
%   e.g. ResultsArray[1] = {Id, false} OR ResultsArray[455] = {Id, true, Counter, Cubes} 
%
% When an answer is found e.g. Id 445, the previous entries will be checked to see if they have been resolved (see answer_found).
% If so and none of them contained an answer then the answer produced by Id 445 will be the one selected.  
results_listener(ResultsArray, Count) ->
    io:format("Received ~B results so far ~n", [Count]),
    case answer_found(1, ResultsArray) of
        {true, Id, Counter, Results} -> answer_print(Counter,Results), exit(found);
        {false} -> 
            receive 
               {Id, false} -> results_listener(array:set(Id, {false}, ResultsArray), Count+1);
               {Id, true, Counter, Cubes} -> results_listener(array:set(Id, {Id, true, Counter, Cubes}, ResultsArray), Count+1)
            end    
    end.

% Used to see if there is an answer. 
% Continues whilst results have been reported for number ranges (e.g. 1-10000, 10001-20000).
% If an array entry is undefined then the function returns false.
answer_found(Index, ResultsArray) ->
    case array:get(Index, ResultsArray) of 
        undefined -> {false};
        {false} -> answer_found(Index+1, ResultsArray);
        {Id, true, Counter, Results} -> {true, Id, Counter, Results}
    end.




%utils
digits_count(Num) when is_integer(Num) -> length(integer_to_list(Num)).

has_nth_root(N, Value) -> no_decimal_places(nth_root(N,Value)).
number_size(I) when is_integer(I) -> length(integer_to_list(I)). 
no_decimal_places(N) -> N == floor(N).
remove_duplicates(L) -> sets:to_list(sets:from_list(L)).

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


start_process_println(N) -> io:format("Starting task ~B~n", [N]).
answer_print(N, Cubes) -> io:format("Answer is ~B~n", [N]), [io:format("Number ~B Root ~B ~n", [N,floor(R)]) || {N,R} <- Cubes].
winner_print(N) -> io:format("We have a winner Id ~B Number ~B ~n", N).
noanswer_print(N) -> io:format("No answer ~B~n", [N]).





% [1,2,3]-[2,3]-[3]













