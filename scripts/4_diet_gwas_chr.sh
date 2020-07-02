#!/bin/sh

EXP=$1
chr=$2
n_threads=$3

#$ -l os=RedHat7
#$ -l h_vmem=10G
#$ -l h_rt=28:00:00
#$ -j y

#$ -pe smp 4
#$ -R y
#$ -binding linear:4


cd kw/ukbb-gene-diet/scripts

singularity exec \
	-B ../data/processed:/data \
	-B /broad/ukbb/imputed_v3:/bgendir \
	-B /humgen/diabetes/UKBB_app27892:/sampledir \
	../../singularity/gem-workflow.simg \
	/bin/bash <<EOF
/GEM/GEM \
	--bgen /bgendir/ukb_imp_chr${chr}_v3.bgen \
	--maf 0.005 \
	--sample /sampledir/ukb27892_imp_chrAUT_v3_s487395.sample \
	--pheno-file /data/ukbb_diet_gwis_phenos.csv \
	--sampleid-name id \
	--pheno-name hba1c \
	--pheno-type 0 \
	--exposure-names ${EXP} \
	--covar-names sex age age_squared cov_GENO_ARRAYUKBL PC1 PC2 PC3 PC4 PC5 PC6 PC7 PC8 PC9 PC10 \
	--delim , \
	--missing-value NA \
	--robust 1 \
	--threads ${n_threads} \
	--out /data/main_ffq_PC_gwis_res/${EXP}_chr${chr}
EOF
