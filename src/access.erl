%% @doc Functions for accessing and altering elements within nested data
%% structures
%%
%% This is a translation of Elixir's `Access' module and associated functions
%% from the Elixir `Kernel' module.
%%
%% It is usually very verbose to access values within deeply nested structures
%% in Erlang. For example, let's reach into a nested map:
%%
%% ```
%% Data = #{foo => #{bar => baz}},
%% #{foo := Bar} = Data,
%% #{bar := Baz} = Bar,
%% Baz.
%% %%=> baz
%% '''
%%
%% `access' provides functions that operate on deeply nested structures using
%% patterns that describe paths into the structure. The above example could
%% be rewritten like so:
%%
%% ```
%% access:get_in(Data, [foo, bar]).
%% %%=> baz
%% '''
%%
%% Or to put a value into the structure:
%%
%% ```
%% access:put_in(Data, [foo, bar], quiz).
%% %%=> #{foo => #{bar => quiz}}
%% '''
%%
%% Access patterns are especially useful when nested data contains lists.
%% We can filter the pattern to only access within certain lists on custom
%% predicates with {@link filter/1} or access all elements in a list with
%% {@link all/0}.
%%
%% ```
%% Data = #{elements => [#{a => b}, #{c => d}, #{a => e}, #{c => d}]}.
%% access:get_in(Data, [elements, access:filter(fun(M) -> is_map_key(a, M) end), a]).
%% %%=> [b,e]
%% access:put_in(Data, [elements, access:all(), f], g).
%% %%=> #{elements =>
%% %%=>    [#{a => b,f => g},
%% %%=>     #{c => d,f => g},
%% %%=>     #{a => e,f => g},
%% %%=>     #{c => d,f => g}]}
%% '''

-module(access).

-define(NIL, nil).

-export([%% functions that operate on structures
         get_in/2,
         put_in/3,
         update_in/3,
         get_and_update_in/3,
         pop_in/2,
         %% functions for building patterns
         all/0,
         filter/1,
         element/1]).

-export_type([pattern/0,
              structure/0,
              container/0,
              any_container/0,
              nil_container/0]).

-type key() :: term().
-type pattern() :: key() | access_fun(any(), any()).

-type container() :: [{atom(), any()}] | map().
-type any_container() :: any().
-type nil_container() :: ?NIL.

-type structure() :: container() | any_container() | nil_container().

-type get_fun(Data) ::
    fun((get, Data, fun((term()) -> term())) -> container()).

-type get_and_update_fun(Data, CurrentValue) ::
    fun((get_and_update, Data, fun((term()) -> term())) ->
        {CurrentValue, container()} | pop).

-type access_fun(Data, CurrentValue) ::
    get_fun(Data) | get_and_update_fun(Data, CurrentValue).

-spec get_in(structure(), pattern()) -> term().
%% @doc Gets a value within a data structure

get_in(?NIL, [_ | _]) -> ?NIL;
get_in(Data, [H]) when is_function(H) ->
    H(get, Data, fun(D) -> D end);
get_in(Data, [H | T]) when is_function(H) ->
    H(get, Data, fun(D) -> get_in(D, T) end);
get_in(Data, [H]) ->
    get(Data, H);
get_in(Data, [H | T]) ->
    get_in(get(Data, H), T).

-spec put_in(structure(), pattern(), term()) -> term().
%% @doc Puts a value into a nested structure

put_in(Data, [_ | _] = Keys, Value) ->
    element(2, get_and_update_in(Data, Keys, fun (_) -> {?NIL, Value} end)).

-spec update_in(structure(), pattern(), fun((term()) -> term())) -> term().
%% @doc Updates a value within a nested structure

update_in(Data, [_ | _] = Keys, UpdateFun) ->
    element(
      2,
      get_and_update_in(
        Data,
        Keys,
        fun (Value) -> {?NIL, UpdateFun(Value)} end)).

-spec get_and_update_in(structure(), pattern(), Fun) ->
    {CurrentValue, structure()} when
        Fun :: fun((term() | ?NIL) -> {CurrentValue, NewValue | pop}),
        CurrentValue :: term(),
        NewValue :: term().
%% @doc Gets the current value within a structure and returns a structure with
%% that value updated

get_and_update_in(Data, [Head], Fun) when is_function(Head, 3) ->
    Head(get_and_update, Data, Fun);
get_and_update_in(Data, [Head | Tail], Fun) when is_function(Head, 3) ->
    Head(get_and_update, Data, fun(D) -> get_and_update_in(D, Tail, Fun) end);
get_and_update_in(Data, [Head], Fun) when is_function(Fun, 1) ->
    get_and_update(Data, Head, Fun);
get_and_update_in(Data, [Head | Tail], Fun) when is_function(Fun, 1) ->
    get_and_update(Data, Head, fun(D) -> get_and_update_in(D, Tail, Fun) end).

-spec pop_in(container(), pattern()) -> term().
%% @doc Pops a value from a nested structure

pop_in(?NIL, [Key | _]) ->
    erlang:error(io_lib:format("could not pop key ~p on a ?NIL value", [Key]));
pop_in(Data, [_ | _] = Keys) ->
    pop_in_data(Data, Keys).

get(Container, Key) ->
    get(Container, Key, ?NIL).

get(Map, Key, Default) when is_map(Map) ->
    maps:get(Key, Map, Default);
get(List, Key, Default) when is_list(List) and is_atom(Key) ->
    case lists:keyfind(Key, 1, List) of
        {_, Value} -> Value;
        false -> Default
    end;
get(List, Key, _Default) when is_list(List) ->
    erlang:error(
      io_lib:format(
        "the access calls for proplists expect "
        "the key to be an atom, got: ~p", [Key]));
get(?NIL, _Key, Default) ->
    Default.

get_and_update(Map, Key, Fun) when is_map(Map) ->
    Current = maps:get(Key, Map, ?NIL),
    case Fun(Current) of
        {Get, Update} ->
            {Get, maps:put(Key, Update, Map)};
        pop ->
            {Current, maps:remove(Key, Map)};
        Other ->
            erlang:error(
              io_lib:format(
                "the given function must return a two-element tuple or `pop', "
                "got: ~p",
                [Other]))
    end;
get_and_update(List, Key, Fun) when is_list(List) ->
    get_and_update_list(List, [], Key, Fun);
get_and_update(?NIL, Key, _Fun) ->
    erlang:error(
      io_lib:format(
        "could not put/update key ~p on a ?NIL value", [Key])).

%% This is a polyfill of `Keyword.get_and_update/3'
get_and_update_list([{Key, Current} | Tail], Acc, Key, Fun) ->
    case Fun(Current) of
        {Get, Update} ->
            {Get, lists:reverse(Acc, [{Key, Update} | Tail])};
        pop ->
            {Current, lists:reverse(Acc, Tail)};
        Other ->
            erlang:error(
              io_lib:format(
                "the given function must return a two-element tuple or `pop', "
                "got: ~p",
                [Other]))
    end;
get_and_update_list([{_, _} = Head | Tail], Acc, Key, Fun) ->
    get_and_update_list(Tail, [Head | Acc], Key, Fun);
get_and_update_list([], Acc, Key, Fun) ->
    case Fun(?NIL) of
        {Get, Update} ->
            {Get, [{Key, Update} | lists:reverse(Acc)]};
        pop ->
            {?NIL, lists:reverse(Acc)};
        Other ->
            erlang:error(
              io_lib:format(
                "the given function must return a two-element tuple or `pop', "
                "got: ~p",
                [Other]))
    end.

pop_in_data(?NIL, [_ | _]) ->
    pop;
pop_in_data(Data, [Fun]) when is_function(Fun) ->
    Fun(get_and_update, Data, fun(_) -> pop end);
pop_in_data(Data, [Key]) ->
    pop(Data, Key);
pop_in_data(Data, [Key | Tail]) ->
    get_and_update(Data, Key, fun(D) -> pop_in_data(D, Tail) end).

pop(Map, Key) when is_map(Map) ->
    %% This is a polyfill of `Map.pop/2'
    case maps:take(Key, Map) of
        {Value, Map2} -> {Value, Map2};
        false         -> {?NIL, Map}
    end;
pop(List, Key) when is_list(List) ->
    %% This is a polyfill of `Keyword.pop/2'
    case lists:keytake(Key, 1, List) of
        {value, Value, List2} -> {Value, List2};
        false                 -> {?NIL, List}
    end;
pop(?NIL, Key) ->
    erlang:error(
      io_lib:format(
        "could not pop key ~p on a ?NIL value", [Key])).

-spec all() -> access_fun(Data :: list(), CurrentValue :: list()).
%% @doc Returns a function that accessses all elements in a list

all() -> fun all/3.

all(get, Data, Next) when is_list(Data) ->
    lists:map(Next, Data);
all(get_and_update, Data, Next) when is_list(Data) ->
    all(Data, Next, _Gets = [], _Updates = []);
all(_Op, Data, _Next) ->
    erlang:error(
      io_lib:format("access:all/0 expected a list, got: ~p", [Data])).

all([Head | Tail], Next, Gets, Updates) ->
    case Next(Head) of
        {Get, Update} -> all(Tail, Next, [Get | Gets], [Update | Updates]);
        pop           -> all(Tail, Next, [Head | Gets], Updates)
    end;
all([], _Next, Gets, Updates) ->
    {lists:reverse(Gets), lists:reverse(Updates)}.

-spec filter(fun((term()) -> boolean())) ->
    access_fun(Data :: list(), CurrentValue :: list()).
%% @doc Returns a function that accesses elements in a list which return `true'
%% in the given predicate

filter(Predicate) when is_function(Predicate) ->
    fun(Op, Data, Next) -> filter(Op, Data, Predicate, Next) end.

filter(get, Data, Predicate, Next) when is_list(Data) ->
    lists:map(Next, lists:filter(Predicate, Data));
filter(get_and_update, Data, Predicate, Next) when is_list(Data) ->
    get_and_update_filter(Data, Predicate, Next, _Gets = [], _Updates = []);
filter(_Op, Data, _Predicate, _Next) ->
    erlang:error(
      io_lib:format("access:filter/1 expected a list, got: ~p", [Data])).

get_and_update_filter([Head | Tail], Predicate, Next, Gets, Updates) ->
    case Predicate(Head) of
        true ->
            case Next(Head) of
                {Get, Update} ->
                    get_and_update_filter(
                      Tail, Predicate, Next, [Get | Gets], [Update | Updates]);
                pop ->
                    get_and_update_filter(
                      Tail, Predicate, Next, [Head | Gets], Updates)
            end;
        false ->
            get_and_update_filter(
              Tail, Predicate, Next, Gets, [Head | Updates])
    end;
get_and_update_filter([], _Predicate, _Next, Gets, Updates) ->
    {lists:reverse(Gets), lists:reverse(Updates)}.

-spec element(non_neg_integer()) -> access_fun(Data :: tuple(), CurrentValue :: term()).
%% @doc Returns a function that accesses the element at the given index in a
%% tuple.
%%
%% == Examples ==
%%
%% ```
%% Data = #{a => {b, b, b}},
%% access:put_in(Data, [a, access:element(1)], e).
%% %%=> #{a => {c, b, b}}
%% '''

element(Index) ->
    fun
        (get, Data, Next) when is_tuple(Data) ->
            Next(erlang:element(Index, Data));
        (get_and_update, Data, Next) when is_tuple(Data) ->
            Value = erlang:element(Index, Data),
            case Next(Value) of
                {Get, Update} ->
                    {Get, erlang:setelement(Index, Data, Update)};
                pop ->
                    erlang:error("cannot pop data from a tuple")
            end;
        (_Op, Data, _Next) ->
            erlang:error(io_lib:format("access:elem/1 expected a tuple, got: ~p", [Data]))
    end.
