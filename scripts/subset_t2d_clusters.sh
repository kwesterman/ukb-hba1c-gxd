#!/bin/sh


#$ -l h_vmem=10G
#$ -l h_rt=12:00:00
#$ -j y

source /broad/software/scripts/useuse
use GCC-5.2

cd kw/ukbb-gene-diet/scripts
t2d_cluster_dir=../data/processed/t2d_cluster_subsets
mkdir -p ${t2d_cluster_dir}

# Use qctool v2 to subset 
qctool="../../opt/qctool_v2.0.6-CentOS\ Linux7.3.1611-x86_64/qctool"
for chr in {1..22}; do
	echo "Subsetting chr${chr}..."
	eval "${qctool}" \
		-g /broad/ukbb/imputed_v3/ukb_imp_chr${chr}_v3.bgen \
		-s /humgen/diabetes/UKBB_app27892/ukb27892_imp_chrAUT_v3_s487395.sample \
		-incl-rsids ${t2d_cluster_dir}/t2d_cluster_rsIDs.txt \
		-og ${t2d_cluster_dir}/chr${chr}_subset.bgen \
		-os ${t2d_cluster_dir}/chr${chr}_subset.sample
done

# Merge subsets
cp ${t2d_cluster_dir}/chr1_subset.bgen ${t2d_cluster_dir}/base_subset.bgen
cp ${t2d_cluster_dir}/chr1_subset.sample ${t2d_cluster_dir}/base_subset.sample
for chr in {2..22}; do
	eval "${qctool}" \
		-g ${t2d_cluster_dir}/base_subset.bgen \
		-s ${t2d_cluster_dir}/base_subset.sample \
		-merge-in ${t2d_cluster_dir}/chr${chr}_subset.bgen ${t2d_cluster_dir}/chr${chr}_subset.sample \
		-og ${t2d_cluster_dir}/full_subset.bgen \
		-os ${t2d_cluster_dir}/full_subset.sample
	cp ${t2d_cluster_dir}/full_subset.bgen ${t2d_cluster_dir}/base_subset.bgen
	cp ${t2d_cluster_dir}/full_subset.sample ${t2d_cluster_dir}/base_subset.sample
done
rm ${t2d_cluster_dir}/base_subset.bgen ${t2d_cluster_dir}/base_subset.sample

# Calculate scores for T2D clusters
plink2="../../opt/plink2"
for cluster in Beta-Cell Proinsulin Obesity Lipodystrophy Liver; do
	eval "${plink2}" \
		--bgen ${t2d_cluster_dir}/full_subset.bgen ref-first \
		--sample ${t2d_cluster_dir}/full_subset.sample \
		--score ${t2d_cluster_dir}/${cluster}_weights.txt \
		--out ${t2d_cluster_dir}/${cluster}
done
