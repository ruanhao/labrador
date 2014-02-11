labrador
========

Overview
--------
Labrador is a web based Erlang process monitor.

  - Simplify the framework and web interface
  - Support [Cowboy] 0.8.x (i will update it to support latest version soon)
  - Support Erlang cluster monitoring
  - Supervise memory and CPU utilization for EVM

Get Started
-----------
###Download Labrador
[rebar] is required to build the project:
>     ~% git clone git@github.com:ruanhao/labrador.git
>     ~% cd labrador
>     ~/labrador% rebar get-deps
Notice: if you see something like:  
ERROR: git clone -n git://github.com/extend/ranch.git ranch failed with error: 128  
please manually update the entry in deps/cowboy/rebar.config as:  
{ranch, ".*", {git, "git@github.com:extend/ranch.git", {tag, "0.6.1"}}}.  

###Configure labrador.config
you have to specify the `central_node`. Here, we set:
>     {central_node, 'hello@hao'}.

###Start Labrador
Notice: you have to confirm that the **cookie** you set in *bootstrap* is **the same as** Erlang cluster.  
run `./bootstrap` under git root and wait to see message like:  
labrador is rocking on: hao.shanghai, please visit http://hao.shanghai:40829/.

###Enjoy Labrador in navigator
![homepage][1]
![process_info][2]

Note
----
You'd better use navigator that supports Websocket.
[Chrome] and [FireFox] are fine.

Acknowledgement
---------------
[SMELLS LIKE BEAM SPIRIT]

Do you like Labrador
--------------------
If you appreciate Labrador, how about a cup of coffee?
[![Donate]](http://goo.gl/6zcOL)


  [Bigwig]:  https://github.com/beamspirit/bigwig.git
  [Cowboy]:  https://github.com/extend/cowboy.git
  [SMELLS LIKE BEAM SPIRIT]:  http://www.metabrew.com/article/bigwig-erlang-webtool-spawnfest
  [Chrome]:  http://www.google.com/chrome/
  [FireFox]:  http://www.mozilla.org/en-US/firefox/
  [rebar]:  https://github.com/basho/rebar.git
  [Donate]:  https://www.paypal.com/en_US/i/btn/btn_donate_SM.gif
  [1]:  https://raw.github.com/ruanhao/labrador/master/priv/img/labrador_overview.png
  [2]:  https://raw.github.com/ruanhao/labrador/master/priv/img/process_info.png
