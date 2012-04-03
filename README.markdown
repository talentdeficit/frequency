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



### <a name="thanks">acknowledgments</a> ###

richard carlsson's [eunit][eunit] provided substantial inspiration and guidance





[MIT]: http://www.opensource.org/licenses/mit-license.html
[eunit]: https://github.com/richcarl/eunit