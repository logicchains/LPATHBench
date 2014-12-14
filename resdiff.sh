#!/usr/bin/bash -u

if [ $# -ne 2 ]; then echo -e "Usage is sh $0 <ARMfile> <X86file>"; exit 1; fi

cat "$1" > armSrtd
cat "$2" > x86Srtd

echo '<html><head></head><body><table style="width: 100%" border="1" cellspacing="1" cellpadding="1">'
echo '<th style="width: 50%">Language</th><th style="width: 50%"> % x86 speed</th>'
echo '{print "<tr><td>"$1"</td><td>"$2"</td></tr>"}' > ./printer.awk

join armSrtd x86Srtd | awk '{print $1" "$2/$3*100}' |  sort -k 2 -n -r | awk -E printer.awk 

echo '</table></body></html>'
