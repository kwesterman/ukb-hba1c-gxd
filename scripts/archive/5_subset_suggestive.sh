#!/bin/sh


#$ -l h_vmem=10G
#$ -l h_rt=6:00:00
#$ -j y

source /broad/software/scripts/useuse
use GCC-5.2

cd kw/ukbb-gene-diet/scripts
sugg_dir=../data/processed/sensitivity_models
mkdir -p ${sugg_dir}

# Gather all suggestive variants from summary statistics
gwis_res_dir=../data/processed/main_ffq_PC_gwis_res
for exp in {1..10} _ERS10 _multiExp10; do
	tail -n +2 ${gwis_res_dir}/ffq_PC${exp}_merged_sugg \
		| awk '{print $2}' > ${sugg_dir}/all_sugg_rsIDs.txt
done
cat ${sugg_dir}/ffq_PC*_sugg_rsIDs.txt > ${sugg_dir}/all_sugg_rsIDs.txt

# Use qctool v2 to subset 
qctool="../../opt/qctool_v2.0.6-CentOS\ Linux7.3.1611-x86_64/qctool"
for chr in {1..22}; do
	eval "${qctool}" \
		-g /broad/ukbb/imputed_v3/ukb_imp_chr${chr}_v3.bgen \
		-incl-rsids ${sugg_dir}/all_sugg_rsIDs.txt \
		-og ${sugg_dir}/chr${chr}_subset.bgen \
		-os ${sugg_dir}/chr${chr}_subset.sample
done

## Merge subsets
cp ${sugg_dir}/chr1_subset.bgen ${sugg_dir}/base_subset.bgen
cp ${sugg_dir}/chr1_subset.sample ${sugg_dir}/base_subset.sample
for chr in {2..22}; do
	eval "${qctool}" \
		-g ${sugg_dir}/base_subset.bgen \
		-s ${sugg_dir}/base_subset.sample \
		-merge-in ${sugg_dir}/chr${chr}_subset.bgen ${sugg_dir}/chr${chr}_subset.sample \
		-og ${sugg_dir}/full_subset.bgen \
		-os ${sugg_dir}/full_subset.sample
	cp ${sugg_dir}/full_subset.bgen ${sugg_dir}/base_subset.bgen
	cp ${sugg_dir}/full_subset.sample ${sugg_dir}/base_subset.sample
done
rm ${sugg_dir}/base_subset.bgen ${sugg_dir}/base_subset.sample
