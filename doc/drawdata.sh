#!/bin/bash

echo 'digraph {'
data="$(grep '^data' Prototype.hs | sed 's/data //; s/ .*//')"
for i in $data
do
	echo "$i;"
	targets="$(grep "data $i .*=" Prototype.hs -A 6 \
		| sed '/^$/,$d' \
		| grep '::' \
		| sed 's/.*:: //; s/Maybe //; s/J.Value//; s/String//; s/ .*//')"
	for j in $targets
	do
		echo "$i -> $j;"
	done
done

types="$(grep '^type' Prototype.hs | grep -v '^type Lookup' | sed 's/type //; s/ .*//')"
for i in $types
do
	echo "$i;"
	targets="$(grep "type $i .*=" Prototype.hs \
		| sed 's/^.*= //' \
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
