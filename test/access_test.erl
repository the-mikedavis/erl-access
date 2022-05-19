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

get_in_with_strings_and_binaries_test() ->
    Data = #{"a" => #{b => #{"c" => d}}},
    ?assertEqual(d, access:get_in(Data, ["a", b, "c"])),
    Data1 = #{<<"a">> => #{b => #{<<"c">> => d}}},
    ?assertEqual(d, access:get_in(Data1, [<<"a">>, b, <<"c">>])),
    ok.

all_test() ->
    Data = #{foo => [#{a => 1}, #{a => 2}, #{a => 3}]},
    ?assertEqual([1, 2, 3], access:get_in(Data, [foo, access:all(), a])),
    ok.

filter_test() ->
    Data = #{foo => [#{a => 1}, #{b => 2}, #{a => 3}, #{a => 4}]},
    AIsMapKey = fun(M) -> is_map_key(a, M) end,
    ?assertEqual([1, 3, 4], access:get_in(Data, [foo, access:filter(AIsMapKey), a])),
    ok.

at_test() ->
    Data = #{foo => [#{a => b}, #{b => c}, #{c => d}]},
    ?assertEqual(b, access:get_in(Data, [foo, access:at(0), a])),
    ?assertEqual(c, access:get_in(Data, [foo, access:at(1), b])),
    ok.

element_test() ->
    Data = #{foo => {a, b, c, d}},
    ?assertEqual(a, access:get_in(Data, [foo, access:element(1)])),
    ?assertEqual(b, access:get_in(Data, [foo, access:element(2)])),
    ok.
