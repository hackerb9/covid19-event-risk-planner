#!/bin/bash
#set -x
fname=$(date +%Y%m%d_%H%M%S) 
wget https://covidtracking.com/api/v1/states/current.csv \
	-O "${fname}.csv" \
	-a "current.log"; 
[ ! -s "${fname}.csv" ] && rm -f "${fname}.csv"
