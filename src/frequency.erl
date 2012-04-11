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
-export([fake/0, fake/1]).
-endif.


-record(result, {
    name,
    function,
    line,
    time,
    error
}).


-record(config, {
    name,
    run = fun run_normal/2
}).


-type test() :: function()
    | {function(), list()}
    | {module(), atom()}
    | {module(), atom(), list()}.


-spec profile(Fs::([test()] | test())) -> ok | {error, term()}.
-spec profile(Fs::([test()] | test()), Opts::[]) -> ok | {error, term()}.

profile(Fs) -> profile(Fs, []).

profile(Fs, Opts) ->
    Results = profile(Fs, #config{}, []),
    report(Results, Opts),
    ok.


profile([], _, Acc) ->
    lists:reverse(Acc);
profile({Name, Test}, Config, Acc) when is_list(Name) ->
    profile(Test, Config#config{name=Name}, Acc);
profile(F, Config, _) when is_function(F, 0) ->
    run(F, Config);
profile({F, Args}, Config, _) when is_function(F), is_list(Args) ->
    run({F, Args}, Config);
profile({Mod, Fun}, Config, _) when is_atom(Mod), is_atom(Fun) ->
    run({Mod, Fun}, Config);
profile({Mod, Fun, Args}, Config, _) when is_atom(Mod), is_atom(Fun), is_list(Args) ->
    run({Mod, Fun, Args}, Config);
profile([F|Fs], Config, Acc) ->
    profile(Fs, Config, profile(F, Config, []) ++ Acc).


report(Results, _Opts) -> io:format("~p~n", [Results]).


run(Test, Config = #config{run=Run}) ->
    Run(#result{
            name = Config#config.name,
            function = Test
        },
        Config
    ).


run_normal(Test, _Config) -> time(Test).


time(Result = #result{function={Mod, Fun, Args}}) ->
    {T, _} = timer:tc(fun() -> apply(Mod, Fun, Args) end),
    [Result#result{time=T}];
time(Result = #result{function={Mod, Fun}}) when is_atom(Mod), is_atom(Fun) ->
    {T, _} = timer:tc(fun() -> apply(Mod, Fun, []) end),
    [Result#result{time=T}];
time(Result = #result{function={Fun, Args}}) ->
    {T, _} = timer:tc(fun() -> apply(Fun, Args) end),
    [Result#result{time=T}];
time(Result = #result{function=Fun}) ->
    {T, _} = timer:tc(Fun),
    [Result#result{time=T}].


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


fake() -> ok.
fake(_) -> ok.
fake(_, _, _) -> ok.


tprofile(Tests) -> tprofile(Tests, []).
tprofile(Tests, Opts) -> profile(Tests, #config{}, []).


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
                tprofile(Fun),
                [#result{function=Fun, time=100}]
            )},
            {"anon fun with args", ?_assertEqual(
                tprofile(FunWithArgs),
                [#result{function=FunWithArgs, time=100}]
            )},
            {"mod/fun", ?_assertEqual(
                tprofile({?MODULE, fake}),
                [#result{function={?MODULE, fake}, time=100}])},
            {"mod/fun with arg", ?_assertEqual(
                tprofile({?MODULE, fake, [foo, bar, baz]}),
                [#result{function={?MODULE, fake, [foo, bar, baz]}, time=100}])},
            {"mixed test representations", ?_assertEqual(
                tprofile([Fun, FunWithArgs, {?MODULE, fake}, {?MODULE, fake, [foo, bar, baz]}]),
                [
                    #result{function=Fun, time=100},
                    #result{function=FunWithArgs, time=100},
                    #result{function={?MODULE, fake}, time=100},
                    #result{function={?MODULE, fake, [foo, bar, baz]}, time=100}
                ]
            )}
        ]
    }].


named_test_() ->
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
                tprofile({"anon fun", Fun}),
                [#result{name="anon fun", function=Fun, time=100}]
            )},
            {"anon fun with args", ?_assertEqual(
                tprofile({"anon fun with args", FunWithArgs}),
                [#result{name="anon fun with args", function=FunWithArgs, time=100}]
            )},
            {"mod/fun", ?_assertEqual(
                tprofile({"mod/fun", {?MODULE, fake}}),
                [#result{name="mod/fun", function={?MODULE, fake}, time=100}])},
            {"mod/fun with arg", ?_assertEqual(
                tprofile({"mod/fun with arg", {?MODULE, fake, [foo, bar, baz]}}),
                [#result{name="mod/fun with arg", function={?MODULE, fake, [foo, bar, baz]}, time=100}])},
            {"mixed test representations", ?_assertEqual(
                tprofile([
                    {"anon fun", Fun},
                    {"anon fun with args", FunWithArgs},
                    {"mod/fun", {?MODULE, fake}},
                    {"mod/fun with args", {?MODULE, fake, [foo, bar, baz]}}
                ]),
                [
                    #result{name="anon fun", function=Fun, time=100},
                    #result{name="anon fun with args", function=FunWithArgs, time=100},
                    #result{name="mod/fun", function={?MODULE, fake}, time=100},
                    #result{name="mod/fun with args", function={?MODULE, fake, [foo, bar, baz]}, time=100}
                ]
            )}
        ]
    }].

-endif.