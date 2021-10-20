# ISTART-sharedreward: Analysis Code

## Overview and disclaimers
- run_* scripts loop through a list of subjects for a given script; e.g., run_L1stats.sh loops all subjects through the L1stats.sh script.
- paths to input/output data should work without error, but check package/software installation

## scripts:
L1stats.sh
L2stats.sh
L3stats.sh
README.md
barweb_dvs2.m
compileZimages.sh
extractROI.sh
gen3colfiles.sh
plotROIdata.m
run_L1stats.sh
run_L2stats.sh
run_L3stats.sh
run_gen3colfiles.sh

## Imaging analyses  
1. Convert `*_events.tsv` files to 3-column files (compatible with FSL) using Tom Nichols' [BIDSto3col.sh](https://github.com/INCF/bidsutils) script. This script is wrapped into our pipeline using `bash gen_3col_files.sh $sub $nruns`
1. Run analyses in FSL. Analyses in FSL consist of three stages, which we call "Level 1" (L1) and "Level 2" (L2).
  - `L1stats.sh` -- initial time series analyses, relating brain responses to the task conditions in each run
  - `L2stats.sh` -- combines data across runs
  - `L3stats.sh` -- combines data across subjects



[fmriprep]: http://fmriprep.readthedocs.io/en/latest/index.html
