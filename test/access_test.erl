-module(access_test).

-include_lib("eunit/include/eunit.hrl").

get_in_map_test() ->
    Data = #{a => #{b => c}, d => e},
    ?assertEqual(c, access:get_in(Data, [a, b])),
    ?assertEqual(e, access:get_in(Data, [d])),
    ok.

put_in_map_test() ->
    Data = #{a => #{b => c}, d => e},
    Data1 = access:put_in(Data, [a, b], f),
    Data2 = access:put_in(Data1, [d], g),
    ?assertEqual(#{a => #{b => f}, d => g}, Data2),
    ok.

update_in_map_test() ->
    Data = #{a => #{b => c}, d => e},
    Update = fun(Value) -> ?assertEqual(c, Value), f end,
    ?assertEqual(#{a => #{b => f}, d => e}, access:update_in(Data, [a, b], Update)),
    ok.

get_and_update_in_map_test() ->
    Data = #{a => #{b => c}, d => e},
    GetAndUpdate = fun(Value) -> ?assertEqual(c, Value), {c, f} end,
    {Current, Updated} = access:get_and_update_in(Data, [a, b], GetAndUpdate),
    ?assertEqual(c, Current),
    ?assertEqual(#{a => #{b => f}, d => e}, Updated),
    ok.
