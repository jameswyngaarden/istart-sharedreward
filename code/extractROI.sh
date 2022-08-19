#!/usr/bin/env bash

# ensure paths are correct irrespective from where user runs the script
scriptdir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
maindir="$(dirname "$scriptdir")"

for sub in `cat ${maindir}/code/newsubs.txt`; do	
	# ROI name and other path information
	for ROI in ppi_C10_FS-C_z8_sub-neg_cluster1 ppi_C10_FS-C_z8_sub-neg_cluster2 ppi_C13_rew-pun_F-C_z12_su-rs2_cluster1 ppi_C13_rew-pun_F-C_z1_main-effect ppi_C16_rew_F-C_z1_main-effect_cluster1 ppi_C16_rew_F-C_z1_main-effect_cluster2 ppi_C21_rew_F-SC_z1_main-effect ppi_C23_rew-pun_F-SC_z12_sub-neg_cluster1 ppi_c9_F-C_sub-neg; do
		for TASK in sharedreward; do
			for run in 1 2; do
				MASK=${maindir}/masks/${ROI}.nii.gz
				#MASK=${maindir}/masks/seed-${ROI}.nii.gz
				fslmeants -i /data/projects/istart-data/derivatives/fmriprep/sub-${sub}/func/sub-${sub}_task-${TASK}_run-${run}_space-MNI152NLin2009cAsym_desc-preproc_bold.nii.gz -m ${MASK} -o sub-${sub}_task-${TASK}_run-${run}_${ROI}.txt
				mv sub-${sub}_task-${TASK}_run-${run}_${ROI}.txt ${maindir}/derivatives/fsl/sub-${sub}/
			done		
		done
	done
done
