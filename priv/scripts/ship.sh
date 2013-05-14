#! /bin/bash

##==========================================
## File    : ship.sh
## Author  : ryan.ruan@ericsson.com
## Created : 2013.05.14
## Purpose : snippet to show IP of the host
##==========================================

result=$( ping -c 1 $( hostname ) | \
          egrep -o '[[:digit:]]* bytes from .*[[:digit:]]{1,3}\.[[:digit:]]{1,3}\.[[:digit:]]{1,3}\.[[:digit:]]{1,3}' | \
          egrep -o '[[:digit:]]{1,3}\.[[:digit:]]{1,3}\.[[:digit:]]{1,3}\.[[:digit:]]{1,3}' )
echo -n $result
