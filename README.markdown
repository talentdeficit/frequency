# <a name="introduction">frequency (vrillyrillyalpha)</a> #

an erlang benchmarking library, inspired by eunit

copyright 2012 alisdair sullivan

frequency is released under the terms of the [MIT][MIT] license

[![Build Status](https://secure.travis-ci.org/talentdeficit/frequency.png?branch=master)](http://travis-ci.org/talentdeficit/jsx)




### index ###

* [introduction](#intro)
* [quickstart](#quickstart)
* [timers](#timers)
  - [simple timer representations](#simpletimers)
  - [control](#control)
* [configuration](#config)
* [acknowledgments](#thanks)




### <a name="quickstart">quickstart</a> ###

to build the library: `rebar compile`

to run a timer set: `frequency:time(TimerSet)`




### <a name="timers">timers</a> ###


<a name="simpletimers">simple timer representations</a>

the following are all simple timer objects:

* a function with no arguments: `fun() -> ok end`, `fun function/0` or `fun module:function/0`
* a function and a list of args that can be passed to `apply/2`: `{fun(_) -> ok end, [foo]}`, `{fun function/1, [foo]}`, `{fun function/2, [foo, bar]}`
* a tuple representing a module, a function, and (optionally) a list of arguments that can be passed to `apply/3`: `{module, function}` (equivalent to `{module, function, []}`) or `{module, function, [foo, bar]}`




<a name="control">control representations</a>

any timer can be wrapped in a control tuple to control how timers are run

the following control representations are available:

* `{repeat, N, TimerSet}` will run the timer set N times
* `{average, TimerSet}` will return the average of the runtimes of the timer set
* `{sum, TimerSet}` will return the sum of the runtimes of the timer set. this is the default
* `{concurrent, TimerSet}` will run the timer set concurrently in a number of processes at once (see [configuration](#config))
* `{sequential, TimerSet}` will run the timer set sequentially. this is the default




### <a name="config">configuration</a> ###

the following options may be passed to the `frequency:time/2` function to control how timers may be run and reported

* `verbose` will report results for every timer in a timer set, not just the overall results
* `{max_concurrent, N}` will allow `N` timers to be run concurrently




### <a name="thanks">acknowledgments</a> ###

richard carlsson's [eunit][eunit] provided substantial inspiration and guidance





[MIT]: http://www.opensource.org/licenses/mit-license.html
[eunit]: https://github.com/richcarl/eunit