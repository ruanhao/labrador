#!/bin/sh
#service iptables stop
rebar compile
cd `dirname $0`
for file in $( ls ./priv/scripts/* ); do
    chmod a+x $file
done
mkdir -p log/sasl
exec erl -sname labrador -pa $PWD/ebin $PWD/deps/*/ebin -hidden -setcookie labrador -boot start_sasl -config sys.config -s labrador
