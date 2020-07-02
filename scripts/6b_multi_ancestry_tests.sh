#!/bin/sh


#$ -l os=RedHat7
#$ -l h_vmem=10G
#$ -l h_rt=8:00:00
#$ -j y

trait=$1

cd ~/kw/ukbb-gene-diet/scripts

# Run primary model for each diet trait, for all suggestive variants,
# in each ancestry group

# AFR
singularity exec \
	-B ../data/processed:/data \
	-B /humgen/diabetes/UKBB_app27892:/sampledir \
	../../singularity/gem-v1.1-workflow.simg \
	/bin/bash <<EOF
	/GEM/GEM \
		--bgen /data/sensitivity_models/all_sugg_full.bgen \
		--sample /sampledir/ukb27892_imp_chrAUT_v3_s487395.sample \
		--pheno-file /data/ukbb_diet_gwis_phenos_AFR.csv \
		--sampleid-name id \
		--pheno-name hba1c \
		--pheno-type 0 \
		--exposure-names ${trait} \
		--covar-names sex age age_squared PC1 PC2 PC3 PC4 PC5 PC6 PC7 PC8 PC9 PC10 \
		--delim , \
		--missing-value NA \
		--robust 1 \
		--threads 1 \
		--out /data/sensitivity_models/${trait}_AFR
EOF


## AFR
#for chr in {1..22}; do
#singularity exec \
#	-B ../data/processed:/data \
#	-B /broad/ukbb/imputed_v3:/bgendir \
#	-B /humgen/diabetes/UKBB_app27892:/sampledir \
#	../../singularity/gem-v1.1-workflow.simg \
#	/bin/bash <<EOF
#	/GEM/GEM \
#		--bgen /bgendir/ukb_imp_chr${chr}_v3.bgen \
#		--include-snp-file /data/all_sugg_variants.txt \
#		--sample /sampledir/ukb27892_imp_chrAUT_v3_s487395.sample \
#		--pheno-file /data/ukbb_diet_gwis_phenos_AFR.csv \
#		--sampleid-name id \
#		--pheno-name hba1c \
#		--pheno-type 0 \
#		--exposure-names ${trait} \
#		--covar-names sex age age_squared PC1 PC2 PC3 PC4 PC5 PC6 PC7 PC8 PC9 PC10 \
#		--delim , \
#		--missing-value NA \
#		--robust 1 \
#		--threads 1 \
#		--out /data/sensitivity_models/${trait}_AFR_chr${chr}
#EOF
#done
#
## EAS
#cat ../data/processed/diet_trait_names.txt | while read trait; do
#singularity exec \
#	-B ../data/processed:/data \
#	../../singularity/gem-workflow.simg \
#	/bin/bash <<EOF
#	/GEM/GEM \
#		--bgen /data/sensitivity_models/full_subset.bgen \
#		--maf 0.005 \
#		--sample /data/sensitivity_models/full_subset.sample \
#		--pheno-file /data/ukbb_diet_gwis_phenos_EAS.csv \
#		--sampleid-name id \
#		--pheno-name hba1c \
#		--pheno-type 0 \
#		--exposure-names ${trait} \
#		--covar-names sex age age_squared PC1 PC2 PC3 PC4 PC5 PC6 PC7 PC8 PC9 PC10 \
#		--delim , \
#		--missing-value NA \
#		--robust 1 \
#		--threads 4 \
#		--out /data/sensitivity_models/${trait}_EAS
#EOF
#done
#
## SAS
#cat ../data/processed/diet_trait_names.txt | while read trait; do
#singularity exec \
#	-B ../data/processed:/data \
#	../../singularity/gem-workflow.simg \
#	/bin/bash <<EOF
#	/GEM/GEM \
#		--bgen /data/sensitivity_models/full_subset.bgen \
#		--maf 0.005 \
#		--sample /data/sensitivity_models/full_subset.sample \
#		--pheno-file /data/ukbb_diet_gwis_phenos_SAS.csv \
#		--sampleid-name id \
#		--pheno-name hba1c \
#		--pheno-type 0 \
#		--exposure-names ${trait} \
#		--covar-names sex age age_squared PC1 PC2 PC3 PC4 PC5 PC6 PC7 PC8 PC9 PC10 \
#		--delim , \
#		--missing-value NA \
#		--robust 1 \
#		--threads 4 \
#		--out /data/sensitivity_models/${trait}_SAS
#EOF
#done
