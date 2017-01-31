#!/bin/sh
origin=$MAC_ADDR_ORIGIN
ios=$MAC_ADDR_IOS

now=`ifconfig | grep -A 1 "^en0" | tail -n1 | awk -F' ' '{print $2}'`

if [ $now = $origin ]
then
    echo 'now mac address is origin. change to ios? (y/n)'
    read input

    if [ $input = 'y' ]
    then
        sudo ifconfig en0 ether $ios
        sudo ifconfig en0 down
        sudo ifconfig en0 up
        echo 'changed origin to ios'
    fi
else
    echo 'now mac address is ios. change to origin? (y/n)'
    read input

    if [ $input = 'y' ]
    then
        sudo ifconfig en0 ether $origin
        sudo ifconfig en0 down
        sudo ifconfig en0 up
        echo 'changed ios to origin'
    fi
fi

