#!/bin/bash

MODEL="src/RAML.hs"

echo 'digraph {'
data="$(grep '^data' $MODEL | sed 's/data //; s/ .*//')"
data="$data $(grep '^newtype' $MODEL | grep -v 'newtype Schema' | sed 's/newtype //; s/ .*//')"
for i in $data
do
	echo "$i;"
	targets="$(grep "data $i .*=" $MODEL -A 6 \
		| sed '/^$/,$d' \
		| grep '::' \
		| sed 's/.*:: //; s/Maybe //; s/J.Value//; s/String//; s/ .*//; s/\[//g; s/\]//g')"
	targets="$targets $(grep "newtype $i .*=" $MODEL -A 6 \
		| sed '/^$/,$d' \
		| grep '::' \
		| sed 's/.*:: //; s/Maybe //; s/J.Value//; s/String//; s/ .*//; s/\[//g; s/\]//g')"
	for j in $targets
	do
		echo "$i -> $j;"
	done
done

types="$(grep '^type' $MODEL | grep -v '^type Lookup' | sed 's/type //; s/ .*//')"
for i in $types
do
	echo "$i;"
	targets="$(grep "type $i .*=" $MODEL \
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
