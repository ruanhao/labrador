Labrador
========
Labrador is a web based Erlang process monitor.
Labrador takes advantage of [Bigwig], but also offers surprises: 

  - Simplify the framework and web interface
  - Support [Cowboy] 0.8.x
  - Support Erlang cluster monitoring
  - Supervise memory and CPU utilization for EVM


Get Started
-----------
Before run Labrador, you need to establish Erlang clustr.
Setup several Erlang nodes using the same cookie:  

Connect nodes, for example: `net_kernel:connect_node('central@hao').`  
```sh
$ git clone git@github.com:ruanhao/labrador.git
$ cd labrador
$ rebar get-deps
$ rebar compile
```
Configure labrador.config. The most important entry is *central_node*, and you can have others untouched. Here, we should set: `{central_node, 'hello@Hao-Ruans-Mac'}.`  
Start Labrador: 


Platform
--------
Chrome, Firefox, Safari

Acknowledgement
--------------
[SMELLS LIKE BEAM SPIRIT]


  [Bigwig]:  https://github.com/beamspirit/bigwig.git
  [Cowboy]:  https://github.com/extend/cowboy.git
  [SMELLS LIKE BEAM SPIRIT]:  http://www.metabrew.com/article/bigwig-erlang-webtool-spawnfest
