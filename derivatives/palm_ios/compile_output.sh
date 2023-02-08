#!/bin/bash

# compile PALM output into a single file

for c in 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20; do
	echo " "
	for s in tstat tstat_uncp tstat_fwep tstat_cfwep; do
	
		cat *_dat_${s}_c${c}.csv
		echo *_dat_${s}_c${c}.csv
	
	done
done
