make buildall

make runall

${start} > rawRes
for((i=0; i < ${#runners[@]}; i++));
do
    ${runners[i]} >> rawRes
done
