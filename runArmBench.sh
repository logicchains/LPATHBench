#!/usr/bin/bash -u

armaddr=root@10.1.1.11

cmd='sshpass -p abc123 ssh root@10.1.1.11 && cd /storage/sdcard1/ && startarch && cd /home/jonathan/projects/LPATHBench/ && sh runbench.sh arm armhtml'

bash cmd
