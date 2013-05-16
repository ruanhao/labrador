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
###Establish Erlang cluster
You can setup several Erlang nodes on different hosts **using the same cookie**. For example (note: malamute is the *central_node*):
<code>haoruan@Hao-Ruans-Mac:~% erl -sname malamute -setcookie labrador</code>  
<code>haoruan@Hao-Ruans-Mac:~% erl -sname collie &nbsp;&nbsp;-setcookie labrador</code>  
<code>haoruan@Hao-Ruans-Mac:~% erl -sname bernard &nbsp;-setcookie labrador</code>  
<code>root@hao:~# erl -sname husky -setcookie labrador</code>  
<code>root@hao:~# erl -sname akita -setcookie labrador</code>
###Connect nodes
For each node other than the central node, do: 
<code>net_kernel:connect_node('malamute@Hao-Ruans-Mac').</code>
###Download Labrador
```sh
$ git clone git@github.com:ruanhao/labrador.git
$ cd labrador
$ rebar get-deps
$ rebar compile
```
###Configure labrador.config
The most important entry is **central_node**, and you can have others untouched. Here, we set: `{central_node, 'malamute@Hao-Ruans-Mac'}.`  
###Start Labrador
<code>./bootstrap</code>  
Now, you can see:  
<code>=INFO REPORT==== 16-May-2013::12:39:36 ===
<code>starting labrador ...  
<code>Connecting to central node     malamute@Hao-Ruans-Mac &nbsp;==========> ok</code>  
<code>Connecting to node &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;collie@Hao-Ruans-Mac &nbsp;&nbsp;&nbsp;==========> ok</code>  
<code>Connecting to node &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;bernard@Hao-Ruans-Mac &nbsp;&nbsp;==========> ok</code>  
<code>Connecting to node &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;husky@hao                      &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;==========> ok</code>  
<code>Connecting to node &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;akita@hao                      &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;==========> ok</code>  
<code>All nodes connected  
<code>Seting net tick time to&nbsp;&nbsp;&nbsp;&nbsp;60                             &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;==========> ok</code>  
<code>=INFO REPORT==== 16-May-2013::12:39:36 ===</code>  
<code>labrador is rocking on: Hao-Ruans-Mac.local, please visit http://150.236.222.113:40829/</code>  

###Enjoy Labrador in navigator
**Preview:**  
![preview](https://raw.github.com/ruanhao/labrador/master/priv/img/labrador_overview.png)  
**Process_info:**  
![process](https://raw.github.com/ruanhao/labrador/master/priv/img/process_info.png)

Note
----
You'd better use navigator that is Websocket supported.
[Chrome] and [FireFox] is ok.

Acknowledgement
--------------
[SMELLS LIKE BEAM SPIRIT]


  [Bigwig]:  https://github.com/beamspirit/bigwig.git
  [Cowboy]:  https://github.com/extend/cowboy.git
  [SMELLS LIKE BEAM SPIRIT]:  http://www.metabrew.com/article/bigwig-erlang-webtool-spawnfest
  [Chrome]:  http://www.google.com/chrome/
  [FireFox]:  http://www.mozilla.org/en-US/firefox/
