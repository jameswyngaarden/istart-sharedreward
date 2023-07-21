#!/bin/bash

# compile PALM output into a single file
#run as: bash compile_output.sh > palm_compiled.csv

for c in 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20; do
	echo " "
	for s in tstat tstat_uncp tstat_fwep tstat_cfwep; do
	
		cat palm_ios_model-1_dat_${s}_c${c}.csv
		echo palm_ios_model-1_dat_${s}_c${c}.csv
	
	done

done
