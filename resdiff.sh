#!/usr/bin/bash -u

if [ $# -ne 2 ]; then echo -e "Usage is sh $0 <ARMfile> <X86file>"; exit 1; fi

sort "$1" > armSrtd
sort "$2" > x86Srtd

echo '| Language | % x86 |'
echo '| :------- | ----: |'
echo '{print "| "$1" | "$2" |"}' > ./printer.awk

join armSrtd x86Srtd | awk '{print $1" "$2/$3*100}' |  sort -k 2 -n -r | awk -E printer.awk 
