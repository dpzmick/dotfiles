#!/bin/sh
# https://github.com/ipmitool/ipmitool/issues/30

if [ $# != 2 ]
then
    echo "Usage: $0 [enable|disable] host-or-ip"
    exit 1
fi

if [ $1 == "enable" ]
then
    ipmitool -I lanplus -H $2 -U root -P $(read -s -p "root passwd: ") raw 0x30 0xce 0x00 0x16 0x05 0x00 0x00 0x00 0x05 0x00 0x00 0x00 0x00
elif [ $1 == "disable" ]
then
    ipmitool -I lanplus -H $2 -U root -P $(read -s -p "root passwd: ") raw 0x30 0xce 0x00 0x16 0x05 0x00 0x00 0x00 0x05 0x00 0x01 0x00 0x00
else
    echo "bad mode '$1', should be enable or disable"
    exit 1
fi
