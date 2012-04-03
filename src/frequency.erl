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

-export([time/1, time/2]).

%% fake function for eunit tests
-ifdef(TEST).
-export([faketimer/0]).
-endif.


-type freq_opts() :: verbose.

-spec time(Timers::term()) -> ok | {error, term()}.
-spec time(Timers::term(), Opts::[freq_opts()]) -> ok | {error, term()}.

time(Timers) -> time(Timers, []).

time(Timers, Opts) ->
        Specs = create_timers(Timers, Opts),
        Results = run_timers(Specs, Opts),
        report_results(Results, Opts).


-record(timer, {
    name,
    function,
    line,
    average = 1,
    concurrent = 1
}).


create_timers(Timers, Opts) -> create_timers(Timers, #timer{}, Opts, []).

create_timers([], _, _, Acc) -> lists:reverse(Acc);
create_timers([Timer|Rest], Spec, Opts, Acc) ->
    MoreTimers = create_timer(Timer, Spec, Opts),
    create_timers(Rest, Spec, Opts, MoreTimers ++ Acc);
create_timers(Timer, Spec, Opts, _Acc) ->
    create_timer(Timer, Spec, Opts).


create_timer({Name, Timers}, Spec, Opts) when is_list(Name) ->
    create_timers(Timers, Spec#timer{name=Name}, Opts, []);
%% controls
create_timer({average, N, Timers}, Spec, Opts) when is_integer(N), N > 0 ->
    create_timers(Timers, Spec#timer{average=N}, Opts, []);
create_timer({concurrent, N, Timers}, Spec, Opts) when is_integer(N), N > 0 ->
    create_timers(Timers, Spec#timer{concurrent=N}, Opts, []);
%% line / simple test pair
create_timer({Line, Timer}, Spec, Opts) when is_integer(Line), Line > 0 ->
    create_timer(Timer, Spec#timer{line=Line}, Opts);
%% simple test representations
create_timer(Timer, Spec, _Opts) when is_function(Timer, 0) ->
    [Spec#timer{function=Timer}];
create_timer({Timer, Args}, Spec, _Opts) when is_function(Timer, 1), is_list(Args) ->
    [Spec#timer{function={Timer, Args}}];
create_timer({Mod, Fun}, Spec, _Opts) ->
    [Spec#timer{function={Mod, Fun, []}}];
create_timer({Mod, Fun, Args}, Spec, _Opts) when is_list(Args) ->
    [Spec#timer{function={Mod, Fun, Args}}].


run_timers(_Timers, _Opts) -> [].

report_results(_Results, _Opts) -> ok.



-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


%% function for eunit tests
faketimer() -> ok.


timer_representation_test_() ->
    %% anon fun for timers
    F = fun() -> ok end,
    G = fun(_) -> ok end,
    [
        {"anon timer", ?_assertEqual(
            create_timers(F, []),
            [#timer{function=F}]
        )},
        {"anon timer with args", ?_assertEqual(
            create_timers({G, [foo]}, []),
            [#timer{function={G, [foo]}}]
        )},
        {"mf timer", ?_assertEqual(
            create_timers({?MODULE, faketimer}, []),
            [#timer{function={?MODULE, faketimer}}]
        )},
        {"mf timer with args", ?_assertEqual(
            create_timers({?MODULE, faketimer, [foo]}, []),
            [#timer{function={?MODULE, faketimer, [foo]}}]
        )},
        {"line annotated timer", ?_assertEqual(
            create_timers({1, F}, []),
            [#timer{function=F,line=1}]
        )},
        {"named timer", ?_assertEqual(
            create_timers({"name", F}, []),
            [#timer{name="name", function=F}]
        )},
        {"nested named timer", ?_assertEqual(
            create_timers({"oldname", {"newname", F}}, []),
            [#timer{name="newname", function=F}]
        )},
        {"two anon timers", ?_assertEqual(
            create_timers([F, F], []),
            [#timer{function=F}, #timer{function=F}]
        )},
        {"average test", ?_assertEqual(
            create_timers({average, 5, {"average", F}}, []),
            [#timer{name="average", function=F, average=5}]
        )},
        {"concurrent test", ?_assertEqual(
            create_timers({concurrent, 3, {"concurrent", F}}, []),
            [#timer{name="concurrent", function=F, concurrent=3}]
        )},
        {"average and concurrent test", ?_assertEqual(
            create_timers({concurrent, 3, [{average, 5, F}, {average, 1, F}]}, []),
            [
                #timer{function=F, average=5, concurrent=3},
                #timer{function=F, average=1, concurrent=3}
            ]
        )}
    ].


-endif.