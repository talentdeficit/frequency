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
    line
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
create_timer({repeat, N, Timers}, Spec, Opts) when is_integer(N), N > 0 ->
    create_timers(lists:flatten(lists:duplicate(N, Timers)), Spec, Opts, []);
create_timer({average, Timers}, Spec, Opts) ->
    [{average, create_timers(Timers, Spec, Opts, [])}];
%% the default, included for completesness
create_timer({sum, Timers}, Spec, Opts) ->
    create_timers(Timers, Spec, Opts, []);
create_timer({concurrent, Timers}, Spec, Opts) ->
    [{concurrent, create_timers(Timers, Spec, Opts, [])}];
%% also default
create_timer({sequential, Timers}, Spec, Opts) ->
    create_timers(Timers, Spec, Opts, []);
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
        {"repeat test", ?_assertEqual(
            create_timers({repeat, 3, {"repeated", F}}, []),
            [
                #timer{name="repeated", function=F},
                #timer{name="repeated", function=F},
                #timer{name="repeated", function=F}
            ]
        )},
        {"average test", ?_assertEqual(
            create_timers({average, [{"one", F}, {"two", F}]}, []),
            [{average, [#timer{name="one", function=F}, #timer{name="two", function=F}]}]
        )},
        {"sum test", ?_assertEqual(
            create_timers({sum, [{"one", F}, {"two", F}]}, []),
            [#timer{name="one", function=F}, #timer{name="two", function=F}]
        )},
        {"concurrent test", ?_assertEqual(
            create_timers({concurrent, [{"one", F}, {"two", F}]}, []),
            [{concurrent, [#timer{name="one", function=F}, #timer{name="two", function=F}]}]
        )},
        {"sequential test", ?_assertEqual(
            create_timers({sequential, [{"one", F}, {"two", F}]}, []),
            [#timer{name="one", function=F}, #timer{name="two", function=F}]
        )},
        {"average repeat test", ?_assertEqual(
            create_timers({average, {repeat, 3, {"repeated", F}}}, []),
            [{average, [
                #timer{name="repeated", function=F},
                #timer{name="repeated", function=F},
                #timer{name="repeated", function=F}
            ]}]
        )},
        {"concurrent average test", ?_assertEqual(
            create_timers({concurrent, [
                {average, [{"one", F}, {"two", F}]},
                {average, [{"three", F}, {"four", F}]}
            ]}, []),
            [{concurrent, [
                {average, [#timer{name="one", function=F}, #timer{name="two", function=F}]},
                {average, [#timer{name="three", function=F}, #timer{name="four", function=F}]}
            ]}]
        )}
    ].


-endif.