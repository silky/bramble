all:
	sh drawdata.sh | dot -Tpng > data.png

devel:
	commando -p cat -q -j | grep --line-buffered 'hs$$' | uniqhash | conscript make
