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


-type freq_opts() :: verbose.

-spec time(Timers::term()) -> ok | {error, term()}.
-spec time(Timers::term(), Opts::[freq_opts()]) -> ok | {error, term()}.

time(Timers) -> time(Timers, []).

time(Timers, Opts) ->
    try
        Timers = create_timers(Timers, Opts),
        Results = run_timers(Timers, Opts),
        report_results(Results, Opts)
    catch error:Error -> {error, Error}
    end.


-record(timer, {
    function = null
}).


create_timers(Timers, Opts) -> create_timers(Timers, #timer{}, Opts, []).

create_timers([], _, _, Acc) -> lists:reverse(Acc);
create_timers([Timer|Rest], BaseTimer, Opts, Acc) when is_function(Timer, 0) ->
    create_timers(Rest, BaseTimer, Opts, [BaseTimer#timer{function=Timer}] ++ Acc).


run_timers(_Timers, _Opts) -> [].

report_results(_Results, _Opts) -> ok.

    

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

timer_representation_test_() ->
    %% anon fun for anon fun test
    F = fun() -> ok end,
    [
        {"anon fun", ?_assertEqual(
            create_timers([F], []),
            [#timer{function=F}]
        )}
    ].
    

-endif.    