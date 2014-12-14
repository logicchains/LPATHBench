#!/usr/bin/bash -u

if [ $# -ne 1 ]; then echo -e "Usage is sh $0 <correctresult>"; exit 1; fi

armaddr=root@10.1.1.11

sshpass -p abc123 ssh $armaddr <<EOF
cd /storage/sdcard1/
sh startarch 
cd /home/jonathan/projects/LPATHBench/ 
sh runbench.sh $1 arm armhtml
EOF

sshpass -p abc123 scp root@10.1.1.11:/storage/sdcard1/lpart/arch/home/jonathan/projects/LPATHBench/arm ./arm
sshpass -p abc123 scp root@10.1.1.11:/storage/sdcard1/lpart/arch/home/jonathan/projects/LPATHBench/armhtml ./armhtml

sh resdiff.sh ./x86 ./arm > diffgraph.html
