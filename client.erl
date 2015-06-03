-module(client).
-export([permutations/1, digits_count/1, start_process/4, results_listener_begin/2, task_await_start/4,
    start_perm_cube_processes/6, remove_duplicates/1,cubes_from_permutations/1, run/0, answer_found/2, results_listener/3]).
-import(io, [format/1, format/2]).
-import(lists, [seq/2]).
-define(NUM_CUBES_FROM_PERM, 3). % Number of permutations that must have a cube root.
 
% Exercise
 run() ->   
    %For the example answer try 1, 50000000 and 500000. To try 5 permutations change NUM_CUBES_FROM_PERM to 5.
    {ok, Start} = io:read("What number do you want to start with? "),
    {ok, End} = io:read("What number will you go up to? "),
    {ok, NumProc} = io:read("How many numbers should each proccess check? "),

    %Receives results and decides upon an answer
    ResultsListenerPid = spawn(?MODULE, results_listener_begin, [array:new(), 0]),

    % Processes that given a range of numbers see if there is an answer
    TaskPids = start_perm_cube_processes(Start,NumProc,1,End,ResultsListenerPid,[]),
    ResultsListenerPid ! pids_to_array(TaskPids, array:new()),
    
    % Start procces
    [Pid ! start || {_, Pid} <- TaskPids].



% Starts perm cubes procceses. Once started they will await for a start message.
% The function returns a List of {Id, Pid}
start_perm_cube_processes(Counter, _, _, Limit, _, TaskPids) when Counter >= Limit -> TaskPids;
start_perm_cube_processes(Counter, Amount, Id, Limit, ParentPid, TaskPids) 
    -> start_process_println(Id), 
        start_perm_cube_processes(Counter+Amount, Amount, Id+1, Limit, ParentPid, 
            [{Id, start_process(Counter, Counter+Amount, Id, ParentPid)}|TaskPids]).

start_process(Start, Finish, Id, ParentPid) -> spawn(?MODULE, task_await_start, [Start, Finish, Id, ParentPid]).

% When a start message is received the task will begin. Allows us to spawn all the processes quickly. Then start 
% all the tasks at once
task_await_start(Counter, End, Id, ParentPid) -> 
    receive
        start -> task(Counter, End, Id, ParentPid)
    end.

task(End, End, Id, ParentPid) -> ParentPid ! {Id, false}; % Solution not found, inform results listener 
task(Counter, End, Id, ParentPid) ->
    case has_nth_root(3,Counter) of
        % Counter doesn't have nth root, skip
        false -> task(Counter+1, End, Id, ParentPid);
        true ->
            % To make this tail recursive we may need to compute this twice.
            % However this is not expected to happen often.  
            case length(cubes_from_permutations(Counter)) == ?NUM_CUBES_FROM_PERM of 
                true -> ParentPid ! {Id, true, Counter, cubes_from_permutations(Counter)};  % Solution found, inform results listener 
                false -> task(Counter + 1, End, Id, ParentPid) % No solution, try next number
        end        
    end.

% Given a number returns all permutations which are the result of a cube operation
cubes_from_permutations(Num) -> 
    [{PermNum, nth_root(3,PermNum)} || PermNum <- permutations(Num), has_nth_root(3,PermNum)].

permutations(Num) when is_integer(Num) ->
    % Build permutations and remove duplicates to reduce processing 
    PermList = remove_duplicates(permutations(integer_to_list(Num))),
    % Some permutations have less digits than Num when the number starts with 0. Filter these out.
    NumSize = digits_count(Num),
    [list_to_integer(Perm) || Perm <- PermList, digits_count(Perm) == NumSize];

permutations([]) -> [[]];
permutations(L)  -> [[H|T] || H <- L, T <- permutations(L--[H])].



% Begins when an Array of Pids belonging to the tasks is received
results_listener_begin(ResultsArray, Count) ->
    receive
        TaskPidsArray -> results_listener(ResultsArray, Count, TaskPidsArray)
    end.

% Each cube permutation process launched has an Id associated with it.
% When the task completes a message is sent to the results_listener process.
% The results of the task are added to index Id in ResultsArray 
%   e.g. ResultsArray[1] = {Id, false} OR ResultsArray[455] = {Id, true, Counter, Cubes} 
%
% When an answer is found e.g. Id 445, the previous entries will be checked to see if they have been resolved (see answer_found).
% If so and none of them contained an answer then the answer produced by Id 445 will be the one selected.  
% Also when an answer is found e.g. Id 445 the tasks preceding that (e.g. 446,447...) are killed as we know that they
% will not contain an answer.

results_listener(ResultsArray, Count, TaskPidsArray) ->
    io:format("results_listener => received ~B results so far ~n", [Count]),
    case answer_found(1, ResultsArray) of
        {true, _, Counter, Results} -> answer_print(Counter,Results), exit(found); %We have an answer. Exit
        {false} -> 
            receive 
               {Id, false} -> results_listener(array:set(Id, {false}, ResultsArray), Count+1, TaskPidsArray);
               {Id, true, Counter, Cubes} -> 
                    kill(Id+1, TaskPidsArray),
                    results_listener(array:set(Id, {Id, true, Counter, Cubes}, ResultsArray), Count+1, TaskPidsArray)
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

%Sends exit signal to every Pid from location Index to the end of the Array.
kill(Index, Array) -> 
    case array:get(Index, Array) of
        undefined -> true;
        Pid -> exit_surpress_error(Pid, void), kill(Index+1, Array)        
    end.

% TODO Redesign to ensure this function isn't needed
exit_surpress_error(Pid, Reason) ->
    try exit(Pid, Reason) of
        _ -> ok
    catch
        _ -> ok
    end.

pids_to_array([], PidArray) -> PidArray;
pids_to_array([{Id, Pid}|T], PidArray) -> pids_to_array(T, array:set(Id, Pid, PidArray)).


%Flip to integer to remove trailing 0s and back to List to use BIF length
digits_count(List) when is_list(List) -> length(integer_to_list(list_to_integer(List))); 
digits_count(Num) when is_integer(Num) -> length(integer_to_list(Num)).

has_nth_root(N, Value) -> contains_decimal_places(nth_root(N,Value)).
contains_decimal_places(N) -> N == floor(N).
remove_duplicates(L) -> sets:to_list(sets:from_list(L)).


floor(X) when X < 0 ->
    T = trunc(X),
    case X - T == 0 of
        true -> T;
        false -> T - 1
    end;
floor(X) -> 
    trunc(X).


% Uses nth root algorithm
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


print(N) when is_list(N) -> [print(X) || X <- N];
print(N) when is_integer(N) -> io:format("~B~n", [N]).


start_process_println(N) -> io:format("Starting task with Id ~B~n", [N]).
answer_print(Num, Cubes) -> 
    io:format("~B is the smallest number in which ~B of its permutations are cubes. ~n", [Num, ?NUM_CUBES_FROM_PERM]), 
    [io:format("~B pow 3 = ~B ~n", [floor(R),N]) || {N,R} <- Cubes].

