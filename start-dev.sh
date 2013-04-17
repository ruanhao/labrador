#!/bin/sh
service iptables stop
rebar compile
cd `dirname $0`
mkdir -p log/sasl
exec erl -sname assist -pa $PWD/ebin $PWD/deps/*/ebin -hidden -setcookie DALLAS -boot start_sasl -config sys.config -s dallas_assist
# Ericsson Environment Below
#toerl=`which erl | xargs cat | tail -n 1 | awk -F ' ' '{print $1}'`
#erl="${toerl%to_erl}erl -sname assist -pa $PWD/ebin $PWD/deps/*/ebin -hidden -setcookie DALLAS -boot start_sasl -config sys.config -s dallas_assist"
#exec $erl
