Labrador
========

Overview
--------
Labrador is a web based Erlang process monitor.
Labrador takes advantage of [Bigwig], but also offers surprises: 

  - Simplify the framework and web interface
  - Support [Cowboy] 0.8.x
  - Support Erlang cluster monitoring
  - Supervise memory and CPU utilization for EVM

Get Started
-----------
###Establish Erlang cluster
You can setup several Erlang nodes on different hosts **using the same cookie**. For example (note: malamute is the *central_node*):
>     ## On host Hao-Ruans-Mac
>     haoruan@Hao-Ruans-Mac:~% erl -sname malamute -setcookie labrador
>     haoruan@Hao-Ruans-Mac:~% erl -sname collie   -setcookie labrador
>     haoruan@Hao-Ruans-Mac:~% erl -sname bernard  -setcookie labrador
>     ## On host hao
>     root@hao:~# erl -sname husky -setcookie labrador
>     root@hao:~# erl -sname akita -setcookie labrador

###Connect nodes
For each node other than the central node, do:
>     net_kernel:connect_node('malamute@Hao-Ruans-Mac').

###Download Labrador
[rebar] is required to build the project:
>     haoruan@Hao-Ruans-Mac:~% git clone git@github.com:ruanhao/labrador.git
>     haoruan@Hao-Ruans-Mac:~% cd labrador
>     haoruan@Hao-Ruans-Mac:labrador% rebar get-deps
>     haoruan@Hao-Ruans-Mac:labrador% rebar compile

###Configure labrador.config
The most important entry is **central_node**, and you can have others untouched. Here, we set:
>     {central_node, 'malamute@Hao-Ruans-Mac'}.

###Start Labrador
>     haoruan@Hao-Ruans-Mac:labrador% ./bootstrap
>     Erlang R15B01 (erts-5.9.1) [source] [async-threads:0] [hipe] [kernel-poll:false]
>     
>     Eshell V5.9.1  (abort with ^G)
>     (labrador@Hao-Ruans-Mac)1> 
>     =INFO REPORT==== 16-May-2013::12:39:36 ===
>     starting labrador ...  
>     Connecting to central node  malamute@Hao-Ruans-Mac   ==========> ok
>     Connecting to node          collie@Hao-Ruans-Mac     ==========> ok
>     Connecting to node          bernard@Hao-Ruans-Mac    ==========> ok
>     Connecting to node          husky@hao                ==========> ok
>     Connecting to node          akita@hao                ==========> ok
>     All nodes connected  
>     Seting net tick time to     60                       ==========> ok
>     =INFO REPORT==== 16-May-2013::12:39:36 ===
>     labrador is rocking on: Hao-Ruans-Mac, please visit http://150.236.222.113:40829/

###Enjoy Labrador in navigator
####Index
![homepage][1]
####Process_info
![process_info][2]

Note
----
You'd better use navigator that is Websocket supported.  
It is ok to use [Chrome] and [FireFox].

Coming Soon
-----------
  * Interface optimization
  * Support logging file
  * Support tracing function
  * And so on

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
