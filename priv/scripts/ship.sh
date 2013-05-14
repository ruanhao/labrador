#! /bin/bash

result=$( ping -c 1 $( hostname ) | \
          egrep -o '[[:digit:]]* bytes from .*[[:digit:]]{1,3}\.[[:digit:]]{1,3}\.[[:digit:]]{1,3}\.[[:digit:]]{1,3}' | \
          egrep -o '[[:digit:]]{1,3}\.[[:digit:]]{1,3}\.[[:digit:]]{1,3}\.[[:digit:]]{1,3}' )
echo -n $result
