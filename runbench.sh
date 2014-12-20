
if [ $# -ne 3 ]; then echo -e "Usage is sh $0 <correctresult> <raw output file> <html output file>"; exit 1; fi

echo "starting build" > isbuilding

echo "Compiling..."
make buildall

runners=( "mono fs.exe"\
	"./cpp"\
	"./rkt"\
	"mono -O=all ./cs.exe"\
	"java jv"\
	"dart dart.dart"\
	"./hs"\
	"./ml"\
	"./lisp"\
	"./rs"\
	"./go"\
	"./gccgo"\
	"./d"\
	"./nim"\
	"./crystal"\
	"luajit lj.lua"\
	"/usr/bin/oraclejava ojv")

echo "Running..."

start=""
${start} > rawRes
${start} > $3
for((i=0; i < ${#runners[@]}; i++));
do
#    echo 'yolo'
    ${runners[i]} >> rawRes
done

filterStringPartOne='$2 == "LANGUAGE" && $1 == '
filterStringPartTwo=$1
filterStringPartThree=' { print $3 " " $4 }' 
awkCmd=$filterStringPartOne$filterStringPartTwo$filterStringPartThree 

echo $awkCmd > filterString.awk

awk -f filterString.awk ./rawRes > ./filteredRes

sort -k 2 -n ./filteredRes > ./sortedRes
cat sortedRes > $2

echo '| Language | Runtime (ms) |' >> $3
echo '| :------- | -----------: |' >> $3
echo '{print "| "$1" | "$2" |"}' > ./printtable.awk
awk -E printtable.awk ./sortedRes >> $3
