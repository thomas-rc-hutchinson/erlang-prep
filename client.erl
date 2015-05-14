-module(client).
-export([add/2, greet_and_add_two/1, hello/0, 
	seq/0, macro/0, funMac/2, info/0,push/2,contains/2, pop/1, animals/0,
	dogs/1, even/0, colour/0, pixels/1, 
	testbit/0, old_enough/1, if_test/0, sound/1, insert/2,fib/1, 
	testadd/0, johndoe/0]).
-import(io, [format/1]).
-import(lists, [seq/2]).
-define(MACRO, 36).
-define(FUNMACRO(X,Y), X-Y).

-record(human, {name, age, powers=[fly]}).

-author("TRCH").
-include("client.hrl").



johndoe() -> #human{name="jondoe", age=37}.

add(X,Y) -> X + Y.

% hello world
%% two is more of a style
hello() -> format("Hello, world!~n").
seq() -> seq(1,10).
macro() -> ?MACRO.
funMac(X,Y) -> ?FUNMACRO(X,Y).
info() -> ?MODULE:module_info().
greet_and_add_two(X) -> hello(), add(X,2).


%%stack start
push(E, []) -> [E];
push(E, List) -> [E|List].

pop([]) -> [];
pop([H|T]) -> T.

contains(E, [E|T]) -> true;
contains(E, []) -> false;
contains(E, [H|T]) -> contains(E, T).
%%stack end

%%list comprehension start
animals() -> [{dog, "Pip"}, {dog, "Bandit"}, {cat, "Cleo"}].

dogs(Animals) -> [Name || {dog, Name} <- Animals].
even() -> [Value+1 || Value <- [1,2,3,4,5], Value rem 2 =:= 0].
%%list comprehension end

%%bit syntax start 
colour() -> 16#F09A29.
pixels(Color) -> <<Color:24>>.
rgb(<<R:8, G:8, B:8>>) -> {R,G,B}.

testbit() -> rgb(pixels(colour())).

bincom() -> [ X || <<X>> <= <<1,2,3,4,5>>, X rem 2 == 0]. 
%%bit syntax end

%guards start 
old_enough(X) when X >= 18; X =< 104 -> true;
old_enough(_) -> false.
%guards end

%if start   
if_test() -> 
	if 1 =:= 1 -> 
		one
	end.

sound(Animal) ->
	Talk = if Animal == dog -> "woof";
			  Animal == cat -> "meow"
		   end,
	{Animal, Talk}.
%if end 


%case start 
insert(X,[]) -> [X];
insert(X,Set) ->
	case lists:member(X,Set) of
		true  -> Set;
		false -> [X|Set]
	end.
%case end 


%fib 
fib(1) -> 1;
fib(2) -> 2;
fib(N) -> fib(N-2) + fib(N-1).


%high order functions
one() -> 1.
two() -> 2.
 
addition(X,Y) -> X() + Y().
testadd() -> addition(fun one/0, fun two/0).