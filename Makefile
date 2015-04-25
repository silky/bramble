all:
	sh doc/drawdata.sh | dot -Tpng > doc/data.png

devel:
	commando -p cat -q -j | grep --line-buffered 'hs$$' | uniqhash | conscript make
