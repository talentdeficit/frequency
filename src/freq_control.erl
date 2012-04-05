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

-module(freq_control).

-export([time/2]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


-spec time(Timers::[frequency:timer()], Opts::[frequency:opts()]) ->
    Results::[frequency:results()].

time(Timers, Opts) ->
    {ok, Pid} = gen_server:start(?MODULE, [Opts], []),
    gen_server:call(Pid, {time, Timers}).


-record(control_state, {
    runners = []
}).

init(Opts) ->
    {ok, #control_state{
        runners = proplists:get_value(max_concurrent, Opts, 8)
    }}.


handle_call({time, Timers}, _From, State) ->
    Results = case Timers of
        {concurrent, T} -> pmap(fun frequency:profile/1, T, State#control_state.runners);
        _ -> map(fun frequency:profile/1, Timers)
    end,
    {reply, Results, State}.


map(F, Timers) -> lists:map(F, Timers).

pmap(F, Timers, _) -> lists:map(F, Timers).


%% implemented to satisfy behaviour
handle_cast(_, _) -> erlang:error(function_clause).

handle_info(_, _) -> erlang:error(function_clause).

terminate(_, _) -> ok.

code_change(_, State, _) -> {ok, State}.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.   