#!/usr/bin/env bash

# ensure paths are correct irrespective from where user runs the script
scriptdir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
maindir="$(dirname "$scriptdir")"

# ROI name and other path information
for ROI in MPFC; do
	MASK=${maindir}/masks/seed-${ROI}.nii.gz
	TASK=sharedreward
	TYPE=act
	outputdir=${maindir}/derivatives/imaging_plots
	mkdir -p $outputdir

	for r in 1 2; do
		for COPENUM in 1 2 3 4 5 6; do
			DATA=${maindir}/derivatives/fsl/zimages/compiled_task-sharedreward_model-02_type-act_run-0${r}_sm-6_cope-${COPENUM}_L1.nii.gz
			fslmeants -i $DATA -o ${outputdir}/${ROI}_type-${TYPE}_run-${r}_cope-${COPENUM}_zimg.txt -m ${MASK}
		done
	done
	for COPENUM in 1 2 3 4 5 6; do
		DATA=${maindir}/derivatives/fsl/zimages/compiled_task-sharedreward_model-02_type-act_sm-6_cope-${COPENUM}_L2.nii.gz
		fslmeants -i $DATA -o ${outputdir}/${ROI}_type-${TYPE}_cope-${COPENUM}_zimg.txt -m ${MASK}
	done
done
