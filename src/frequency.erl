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

%% fake function for eunit tests
-ifdef(TEST).
-export([faketimer/0]).
-endif.


-record(test, {
    name,
    function,
    line,
    time,
    error
}).

-type control() :: sum | average | sequential | concurrent.
-type test() :: #test{} | {control(), [test()]}.

-type opts() :: verbose | {max_concurrent, integer()}.


-spec profile(Fs::term()) -> ok | {error, term()}.
-spec profile(Fs::term(), Opts::[opts()]) -> ok | {error, term()}.

profile(Fs) -> profile(Fs, []).

profile(Fs, Opts) ->
        Specs = create_tests(Fs, Opts),
        Results = run_tests(Specs, Opts),
        report_results(Results, Opts).



create_tests(Fs, Opts) -> create_tests(Fs, #test{}, Opts, []).

create_tests([], _, _, Acc) -> lists:reverse(Acc);
create_tests([F|Rest], Spec, Opts, Acc) ->
    MoreFs = create_test(F, Spec, Opts),
    create_tests(Rest, Spec, Opts, MoreFs ++ Acc);
create_tests(F, Spec, Opts, _Acc) ->
    create_test(F, Spec, Opts).


create_test({Name, Fs}, Spec, Opts) when is_list(Name) ->
    create_tests(Fs, Spec#test{name=Name}, Opts, []);
create_test({Line, Fs}, Spec, Opts) when is_integer(Line), Line > 0 ->
    create_tests(Fs, Spec#test{line=Line}, Opts, []);
%% controls
create_test({repeat, N, Fs}, Spec, Opts) when is_integer(N), N > 0 ->
    create_tests(lists:flatten(lists:duplicate(N, Fs)), Spec, Opts, []);
create_test({average, Fs}, Spec, Opts) ->
    [{average, create_tests(Fs, Spec, Opts, [])}];
create_test({sum, Fs}, Spec, Opts) ->
    [{sum, create_tests(Fs, Spec, Opts, [])}];
create_test({concurrent, Fs}, Spec, Opts) ->
    [{concurrent, create_tests(Fs, Spec, Opts, [])}];
create_test({sequential, Fs}, Spec, Opts) ->
    [{sequential, create_tests(Fs, Spec, Opts, [])}];
%% simple test representations
create_test(F, Spec, _Opts) when is_function(F, 0) ->
    [Spec#test{function=F}];
create_test({F, Args}, Spec, _Opts) when is_function(F, 1), is_list(Args) ->
    [Spec#test{function={F, Args}}];
create_test({Mod, Fun}, Spec, _Opts) ->
    [Spec#test{function={Mod, Fun, []}}];
create_test({Mod, Fun, Args}, Spec, _Opts) when is_list(Args) ->
    [Spec#test{function={Mod, Fun, Args}}].


run_tests(Fs, Opts) -> ok.

report_results(Results, _Opts) -> Results.



-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").


%% function for eunit tests
faketimer() -> ok.


create_representation_test_() ->
    %% anon fun for profilers
    F = fun() -> ok end,
    G = fun(_) -> ok end,
    [
        {"anon test", ?_assertEqual(
            create_tests(F, []),
            [#test{function=F}]
        )},
        {"anon test with args", ?_assertEqual(
            create_tests({G, [foo]}, []),
            [#test{function={G, [foo]}}]
        )},
        {"mf test", ?_assertEqual(
            create_tests({?MODULE, faketimer}, []),
            [#test{function={?MODULE, faketimer}}]
        )},
        {"mf test with args", ?_assertEqual(
            create_tests({?MODULE, faketimer, [foo]}, []),
            [#test{function={?MODULE, faketimer, [foo]}}]
        )},
        {"line annotated test", ?_assertEqual(
            create_tests({1, F}, []),
            [#test{function=F,line=1}]
        )},
        {"named tests", ?_assertEqual(
            create_tests({"name", F}, []),
            [#test{name="name", function=F}]
        )},
        {"nested named tests", ?_assertEqual(
            create_tests({"oldname", {"newname", F}}, []),
            [#test{name="newname", function=F}]
        )},
        {"two anon tests", ?_assertEqual(
            create_tests([F, F], []),
            [#test{function=F}, #test{function=F}]
        )},
        {"repeated test", ?_assertEqual(
            create_tests({repeat, 3, {"repeated", F}}, []),
            [
                #test{name="repeated", function=F},
                #test{name="repeated", function=F},
                #test{name="repeated", function=F}
            ]
        )},
        {"averaged test", ?_assertEqual(
            create_tests({average, [{"one", F}, {"two", F}]}, []),
            [{average, [#test{name="one", function=F}, #test{name="two", function=F}]}]
        )},
        {"averaged averaged test", ?_assertEqual(
            create_tests({average, [
                {average, [{"one", F}, {"two", F}]},
                {average, [{"one", F}, {"two", F}]}
            ]}, []),
            [{average, [
                {average, [#test{name="one", function=F}, #test{name="two", function=F}]},
                {average, [#test{name="one", function=F}, #test{name="two", function=F}]}
            ]}]
        )},
        {"sum test", ?_assertEqual(
            create_tests({sum, [{"one", F}, {"two", F}]}, []),
            [{sum, [#test{name="one", function=F}, #test{name="two", function=F}]}]
        )},
        {"concurrent test", ?_assertEqual(
            create_tests({concurrent, [{"one", F}, {"two", F}]}, []),
            [{concurrent, [#test{name="one", function=F}, #test{name="two", function=F}]}]
        )},
        {"sequential test", ?_assertEqual(
            create_tests({sequential, [{"one", F}, {"two", F}]}, []),
            [{sequential, [#test{name="one", function=F}, #test{name="two", function=F}]}]
        )},
        {"averaged repeat test", ?_assertEqual(
            create_tests({average, {repeat, 3, {"repeated", F}}}, []),
            [{average, [
                #test{name="repeated", function=F},
                #test{name="repeated", function=F},
                #test{name="repeated", function=F}
            ]}]
        )},
        {"concurrent averaged test", ?_assertEqual(
            create_tests({concurrent, [
                {average, [{"one", F}, {"two", F}]},
                {average, [{"three", F}, {"four", F}]}
            ]}, []),
            [{concurrent, [
                {average, [#test{name="one", function=F}, #test{name="two", function=F}]},
                {average, [#test{name="three", function=F}, #test{name="four", function=F}]}
            ]}]
        )},
        {"averaged concurrent test", ?_assertEqual(
            create_tests({average, [
                {concurrent, [{"one", F}, {"two", F}]},
                {concurrent, [{"three", F}, {"four", F}]}
            ]}, []),
            [{average, [
                {concurrent, [#test{name="one", function=F}, #test{name="two", function=F}]},
                {concurrent, [#test{name="three", function=F}, #test{name="four", function=F}]}
            ]}]
        )}        
    ].


-endif.