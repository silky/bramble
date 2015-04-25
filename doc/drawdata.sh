#!/bin/bash

echo 'digraph {'
data="$(grep '^data' src/Prototype.hs | sed 's/data //; s/ .*//')"
data="$data $(grep '^newtype' src/Prototype.hs | sed 's/newtype //; s/ .*//')"
for i in $data
do
	echo "$i;"
	targets="$(grep "data $i .*=" src/Prototype.hs -A 6 \
		| sed '/^$/,$d' \
		| grep '::' \
		| sed 's/.*:: //; s/Maybe //; s/J.Value//; s/String//; s/ .*//; s/\[//g; s/\]//g')"
	targets="$targets $(grep "newtype $i .*=" src/Prototype.hs -A 6 \
		| sed '/^$/,$d' \
		| grep '::' \
		| sed 's/.*:: //; s/Maybe //; s/J.Value//; s/String//; s/ .*//; s/\[//g; s/\]//g')"
	for j in $targets
	do
		echo "$i -> $j;"
	done
done

types="$(grep '^type' src/Prototype.hs | grep -v '^type Lookup' | sed 's/type //; s/ .*//')"
for i in $types
do
	echo "$i;"
	targets="$(grep "type $i .*=" src/Prototype.hs \
		| sed 's/^.*= //' \
		| sed 's/[()]//g' \
		| sed 's/Maybe//g' \
		| sed 's/[[:<:]]M.Map[[:>:]]/ /g' \
		| sed 's/[[:<:]]J.Value[[:>:]]/ /g' \
		| sed 's/[[:<:]]String[[:>:]]/ /g' \
		| sed 's/[[:<:]]Int[[:>:]]/ /g' \
		| sed 's/[[:<:]]Lookup[[:>:]]/ /g')"
	for j in $targets
	do
		echo "$i -> $j;"
	done
done

echo '}'
