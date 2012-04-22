%% The MIT License

%% Copyright (c) 2012 Alisdair Sullivan <alisdairsullivan@yahoo.ca>

%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:

%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.

%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.

-module(frequency).

-export([profile/1, profile/2]).
-export([run_normal/2]).


%% fake function for eunit tests
-ifdef(TEST).
-export([fake/0, fake/1, fake/3]).
-endif.


-record(result, {
    name,
    function,
    line,
    result
}).


-record(config, {
    name,
    run = fun run_normal/2
}).


-type test() :: [test()]
    | function()
    | {function(), list()}
    | {module(), atom()}
    | {module(), atom(), list()}
    | {list(), test()}
    | {repeat, integer(), test()}
    | {sum, test()}
    | {list(), sum, test()}
    | {average, test()}
    | {list(), average, test()}.


-type result() :: [#result{}].


-spec profile(Fs::test()) -> result().
-spec profile(Fs::test(), Opts::[]) -> result().

profile(Fs) -> profile(Fs, []).

profile(Fs, Opts) ->
    Results = profile(Fs, #config{}, []),
    report(Results, Opts).


report(Results, Opts) ->
    report(Results, Opts, []).

report([], _Opts, Acc) -> lists:reverse(Acc);
report([Result|Rest], Opts, Acc) ->
    report(Rest, Opts, [[
        {name, Result#result.name},
        {function, Result#result.function},
        {line, Result#result.line},
        time_or_error(Result)
    ] | Acc]).


time_or_error(#result{result=Time}) when is_integer(Time) -> {time, Time};
time_or_error(#result{result=Exit}) -> {error, Exit}.


profile([], _Config, Acc) ->
    lists:reverse(Acc);
%% name the next test, note that you can only name individual tests, and not
%%  groups of tests. if a test in a list of tests is not specifically named
%%  it is considered unnamed, there's no name inheritance
profile({Name, Test}, Config, []) when is_list(Name) ->
    profile(Test, Config#config{name=Name}, []);
%% control fixtures
profile({repeat, N, Tests}, Config, []) when is_integer(N), N > 0 ->
    profile(repeat(Tests, N), Config, []);
%% simple test representations
profile(F, Config, _) when is_function(F, 0) ->
    run(F, Config);
profile({F, Args}, Config, _) when is_function(F), is_list(Args) ->
    run({F, Args}, Config);
profile({Mod, Fun}, Config, _) when is_atom(Mod), is_atom(Fun) ->
    run({Mod, Fun}, Config);
profile({Mod, Fun, Args}, Config, _) when is_atom(Mod), is_atom(Fun), is_list(Args) ->
    run({Mod, Fun, Args}, Config);
%% complex tests
profile({sum, Tests}, Config, []) ->
    [reduce({sum, profile(Tests, Config, [])}, Config)];
profile({average, Tests}, Config, []) ->
    [reduce({average, profile(Tests, Config, [])}, Config)];
%% complex tests with inline names, equivalent to `{name, {foo, Tests}}`
profile({Name, sum, Tests}, Config, []) when is_list(Name) ->
    [reduce({sum, profile(Tests, Config, [])}, Config#config{name=Name})];
profile({Name, average, Tests}, Config, []) when is_list(Name) ->
    [reduce({average, profile(Tests, Config, [])}, Config#config{name=Name})];
%% list of tests, unset name
profile([F|Fs], Config, Acc) ->
    profile(
        Fs,
        Config#config{name=undefined},
        profile(F, Config#config{name=undefined}, []) ++ Acc
    ).


%% reduce any `sum` or `average` controls to singular results
reduce(Results, Config) when is_list(Results) ->
    [ reducer(Element, Config) || Element <- Results ];
reduce(Results, Config) ->
    reducer(Results, Config).

reducer(Result, _Config) when is_record(Result, result) -> Result;
reducer({sum, Results}, Config) ->
    lists:foldl(fun sum/2, #result{name=Config#config.name, result=0}, Results);
reducer({average, Results}, Config) ->
    Average = lists:foldl(fun sum/2, #result{name=Config#config.name, result=0}, Results),
    Average#result{result=(Average#result.result div length(Results))}.

sum(Test = #result{result=Time}, Acc = #result{result=Total}) when is_record(Test, result) ->
    Acc#result{result=Total + Time};
sum(Else, Acc = #result{result=Total}) ->
    Time = (reduce(Else, #config{}))#result.result,
    Acc#result{result=Total + Time}.


repeat(Tests, N) when is_list(Tests) -> lists:flatten(lists:duplicate(N, Tests));
repeat(Test, N) -> lists:duplicate(N, Test).


run(Test, Config = #config{run=Run}) ->
    Run(#result{name = Config#config.name, function = Test}, Config).


%% run one test in it's own process, shimmed from the main process
run_normal(Test, Config) -> run_test(Test, Config).


run_test(Test, Config) ->
    Parent = self(),
    _ShimPid = spawn_opt(fun() -> shim_test(Parent, Test, Config) end, []),
    case rec_loop() of
        {test_result, Result} -> Result;
        {error, Exit} -> Test#result{result = Exit}
    end.


shim_test(Parent, Test, _Config) ->
    {TestPid, _} = spawn_opt(fun() -> test_wrapper() end, [monitor]),
    %% set shim as target for io from test
    group_leader(self(), TestPid),
    TestPid ! {test, self(), Test},
    Parent ! rec_loop().


test_wrapper() ->
    receive {test, From, Test} ->
        From ! {test_result, try time(Test) catch Type:Error -> [Test#result{result={Type,Error}}] end}
    end.


time(Result = #result{function={Mod, Fun, Args}}) ->
    {T, _} = timer:tc(fun() -> apply(Mod, Fun, Args) end),
    [Result#result{result=T}];
time(Result = #result{function={Mod, Fun}}) when is_atom(Mod), is_atom(Fun) ->
    {T, _} = timer:tc(fun() -> apply(Mod, Fun, []) end),
    [Result#result{result=T}];
time(Result = #result{function={Fun, Args}}) ->
    {T, _} = timer:tc(fun() -> apply(Fun, Args) end),
    [Result#result{result=T}];
time(Result = #result{function=Fun}) ->
    {T, _} = timer:tc(Fun),
    [Result#result{result=T}].


rec_loop() ->
    receive
        {test_result, Result} ->
            {test_result, Result};
        {'DOWN', _, process, _, Exit} ->
            {error, Exit};
        {io_request, From, Reply, Request} ->
            io_request(From, Reply, Request),
            rec_loop()
    end.


io_request(From, Replier, Request) -> From ! {io_reply, Replier, io_reply(Request)}.


io_reply({put_chars, _}) -> ok;
io_reply({put_chars, _, _}) -> ok;
io_reply({put_chars, _, _, _}) -> ok;
io_reply({put_chars, _, _, _, _}) -> ok;
io_reply({set_opts, _}) -> ok;
io_reply({requests, _}) -> ok;
io_reply({get_chars, _, _, _}) -> eof;
io_reply({get_chars, _, _}) -> eof;
io_reply({get_line, _}) -> eof;
io_reply({get_line, _, _}) -> eof;
io_reply({get_until, _, _, _, _}) -> eof.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


fake() -> ok.
fake(_) -> ok.
fake(_, _, _) -> ok.


p(Tests) -> p(Tests, []).
p(Tests, _Opts) -> profile(Tests, #config{}, []).


basic_profiling_test_() ->
    Fun = fun() -> ok end,
    FunWithArgs = {fun(_, _) -> ok end, [foo, bar]},
    [{foreach,
        fun() ->
            ok = meck:new(timer, [unstick]),
            ok = meck:expect(timer, tc, fun(F) when is_function(F, 0) -> {100, ok} end)
        end,
        fun(_) ->
            ?assert(meck:validate(timer)),
            ok = meck:unload(timer)
        end,
        [
            {"anon fun", ?_assertEqual(
                p(Fun),
                [#result{function=Fun, result=100}]
            )},
            {"anon fun with args", ?_assertEqual(
                p(FunWithArgs),
                [#result{function=FunWithArgs, result=100}]
            )},
            {"mod/fun", ?_assertEqual(
                p({?MODULE, fake}),
                [#result{function={?MODULE, fake}, result=100}])},
            {"mod/fun with arg", ?_assertEqual(
                p({?MODULE, fake, [foo, bar, baz]}),
                [#result{function={?MODULE, fake, [foo, bar, baz]}, result=100}])},
            {"mixed test representations", ?_assertEqual(
                p([Fun, FunWithArgs, {?MODULE, fake}, {?MODULE, fake, [foo, bar, baz]}]),
                [
                    #result{function=Fun, result=100},
                    #result{function=FunWithArgs, result=100},
                    #result{function={?MODULE, fake}, result=100},
                    #result{function={?MODULE, fake, [foo, bar, baz]}, result=100}
                ]
            )}
        ]
    }].


named_basic_test_() ->
    Fun = fun() -> ok end,
    FunWithArgs = {fun(_, _) -> ok end, [foo, bar]},
    [{foreach,
        fun() ->
            ok = meck:new(timer, [unstick]),
            ok = meck:expect(timer, tc, fun(F) when is_function(F, 0) -> {100, ok} end)
        end,
        fun(_) ->
            ?assert(meck:validate(timer)),
            ok = meck:unload(timer)
        end,
        [
            {"anon fun", ?_assertEqual(
                p({"anon fun", Fun}),
                [#result{name="anon fun", function=Fun, result=100}]
            )},
            {"anon fun with args", ?_assertEqual(
                p({"anon fun with args", FunWithArgs}),
                [#result{name="anon fun with args", function=FunWithArgs, result=100}]
            )},
            {"mod/fun", ?_assertEqual(
                p({"mod/fun", {?MODULE, fake}}),
                [#result{name="mod/fun", function={?MODULE, fake}, result=100}])},
            {"mod/fun with arg", ?_assertEqual(
                p({"mod/fun with arg", {?MODULE, fake, [foo, bar, baz]}}),
                [#result{name="mod/fun with arg", function={?MODULE, fake, [foo, bar, baz]}, result=100}])},
            {"mixed test representations", ?_assertEqual(
                p([
                    {"anon fun", Fun},
                    {"anon fun with args", FunWithArgs},
                    {"mod/fun", {?MODULE, fake}},
                    {"mod/fun with args", {?MODULE, fake, [foo, bar, baz]}}
                ]),
                [
                    #result{name="anon fun", function=Fun, result=100},
                    #result{name="anon fun with args", function=FunWithArgs, result=100},
                    #result{name="mod/fun", function={?MODULE, fake}, result=100},
                    #result{name="mod/fun with args", function={?MODULE, fake, [foo, bar, baz]}, result=100}
                ]
            )}
        ]
    }].

sum_test_() ->
    Fun = fun() -> ok end,
    [{foreach,
        fun() ->
            ok = meck:new(timer, [unstick]),
            ok = meck:expect(timer, tc, fun(F) when is_function(F, 0) -> {100, ok} end)
        end,
        fun(_) ->
            ?assert(meck:validate(timer)),
            ok = meck:unload(timer)
        end,
        [
            {"no sum", ?_assertEqual(p({sum, Fun}), [#result{result=100}])},
            {"named sum", ?_assertEqual(
                p({"sum", {sum, [Fun, Fun, Fun]}}),
                [#result{name="sum", result=300}]
            )},
            {"named sum", ?_assertEqual(
                p({"sum", sum, [Fun, Fun, Fun]}),
                [#result{name="sum", result=300}]
            )},
            {"sum", ?_assertEqual(
                p({sum, [Fun, Fun, Fun]}),
                [#result{result=300}]
            )},
            {"sum of sums", ?_assertEqual(
                p({sum, [{sum, [Fun, Fun, Fun]}, {sum, [Fun, Fun, {sum, [Fun]}]}]}),
                [#result{result=600}]
            )}
        ]
    }].


average_test_() ->
    Fun = fun() -> ok end,
    [{foreach,
        fun() ->
            ok = meck:new(timer, [unstick]),
            ok = meck:sequence(
                timer,
                tc,
                1,
                [{100, ok}, {300, ok}, {500, ok}, {200, ok}, {600, ok}, {1000, ok}]
            )
        end,
        fun(_) ->
            ?assert(meck:validate(timer)),
            ok = meck:unload(timer)
        end,
        [
            {"no average", ?_assertEqual(p({average, Fun}), [#result{result=100}])},
            {"average", ?_assertEqual(
                p({average, [Fun, Fun, Fun]}),
                [#result{result=300}]
            )},
            {"named average", ?_assertEqual(
                p({"average", {average, [Fun, Fun, Fun]}}),
                [#result{name="average", result=300}]
            )},
            {"inline named average", ?_assertEqual(
                p({"average", average, [Fun, Fun, Fun]}),
                [#result{name="average", result=300}]
            )},
            {"average of averages", ?_assertEqual(
                p({average, [
                    {average, [Fun, Fun, Fun]},
                    {average, [Fun, Fun, {average, [Fun]}]}
                ]}),
                [#result{result=450}]
            )}
        ]
    }].


repeat_test_() ->
    Fun = fun() -> ok end,
    FunWithArgs = {fun(_, _) -> ok end, [foo, bar]},
    [{foreach,
        fun() ->
            ok = meck:new(timer, [unstick]),
            ok = meck:expect(timer, tc, fun(F) when is_function(F, 0) -> {100, ok} end)
        end,
        fun(_) ->
            ?assert(meck:validate(timer)),
            ok = meck:unload(timer)
        end,
        [
            {"repeat", ?_assertEqual(
                p({repeat, 3, Fun}),
                lists:flatten(lists:duplicate(3, [#result{function=Fun, result=100}]))
            )},
            {"compound repeat", ?_assertEqual(
                p({repeat, 3, [Fun, FunWithArgs, {?MODULE, fake}, {?MODULE, fake, [foo, bar, baz]}]}),
                lists:flatten(lists:duplicate(3, [
                    #result{function=Fun, result=100},
                    #result{function=FunWithArgs, result=100},
                    #result{function={?MODULE, fake}, result=100},
                    #result{function={?MODULE, fake, [foo, bar, baz]}, result=100}
                ]))
            )}
        ]
    }].


exit_test_() ->
    Fun = fun() -> erlang:exit(badarg) end,
    [
        {"exit", ?_assertEqual(p(Fun), [#result{function=Fun, result={exit, badarg}}])}
    ].


-endif.