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


-record(test, {
    name,
    function,
    line,
    time,
    error
}).


-spec profile(Fs::([#test{}] | #test{})) -> ok | {error, term()}.
-spec profile(Fs::([#test{}] | #test{}), Opts::[]) -> ok | {error, term()}.

profile(Fs) -> profile(Fs, []).

profile(Fs, Opts) ->
    Results = profile(Fs, Opts, [], fun run_normal/2),
    report(Results, Opts),
    ok.


profile([], _, Acc, _) ->
    lists:reverse(Acc);
profile(F, Opts, _, Run) when is_function(F, 0) ->
    Run(F, Opts);
profile({F, Args}, Opts, _, Run) when is_function(F, 1), is_list(Args) ->
    Run(fun() -> F(Args) end, Opts);
profile({Mod, Fun}, Opts, _, Run) when is_atom(Mod), is_atom(Fun) ->
    Run(fun() -> Mod:Fun() end, Opts);
profile({Mod, Fun, Args}, Opts, _, Run) when is_atom(Mod), is_atom(Fun), is_list(Args) ->
    Run(fun() -> Mod:Fun(Args) end, Opts);
profile([F|Fs], Opts, Acc, Run) ->
    profile(Fs, Opts, profile(F, Opts, [], Run) ++ Acc, Run).


report(Results, _Opts) -> io:format("~p~n", [Results]).


run_normal(Test, _Opts) -> {T, _} = timer:tc(Test), [T].


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


fake() -> ok.
fake(_) -> ok.


basic_profiling_test_() ->
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
                profile(fun() -> ok end, [], [], fun run_normal/2),
                [100]
            )},
            {"anon fun with arg", ?_assertEqual(
                profile({fun(_) -> ok end, [ok]}, [], [], fun run_normal/2),
                [100]
            )},
            {"mod/fun", ?_assertEqual(
                profile({?MODULE, fake}, [], [], fun run_normal/2),
                [100]
            )},
            {"mod/fun with arg", ?_assertEqual(
                profile({?MODULE, fake, [ok]}, [], [], fun run_normal/2),
                [100]
            )},
            {"mixed test representations", ?_assertEqual(
                profile([fun() -> ok end, {fun(ok) -> ok end, [ok]}, {?MODULE, fake}, {?MODULE, fake, [ok]}], [], [], fun run_normal/2),
                [100, 100, 100, 100]
            )}
        ]
    }].

-endif.