#!/bin/bash
#module load FreeSurfer/6.0.0
#module load FSL
export SUBJECTS_DIR=/home/smadarlab/freesurfer/subjects
declare -a subject=('0025427' '0025429' '0025428' '0025430' '0025431' '0025432' '0025433' '0025434' '0025435' '0025436' '0025437' '0025438' '0025439' '0025440' '0025441' '0025442' '0025443' '0025444' '0025445' '0025446' '0025447' '0025448' '0025449' '0025450' '0025451' '0025452' '0025453' '0025454' '0025455' '0025456')
dir=/home/smadarlab/DataspellProjects/Yvonne/
for sub in "${subject[@]}"
do for ses in ses-01 ses-02 ses-10
do for hemi in lh rh
do mri_vol2surf --mov ${dir}/HNG_After_load_files/afterLoad${sub}/sub-${sub}_${ses}_MNI152NLin2009cAsym_desc-preproc_bold_afterLoadConf.nii.gz
  --mni152reg \
  --projfrac-avg 0.2 0.8 0.1 \
  --trgsubject fsaverage5 \
  --interp nearest \
  --hemi ${hemi} \
  --surf-fwhm --cortex --noreshape \
  --o ${dir}/Brainspace/ts_HNG_data/sub-${sub}-${ses}-preproc_smoth6.fsa5.${hemi}.mgz 
done
done
done
