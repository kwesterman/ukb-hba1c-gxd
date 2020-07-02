#!/bin/sh

EXP=$1

#$ -l os=RedHat7
#$ -l h_vmem=10G
#$ -l h_rt=12:00:00
#$ -j y

#$ -pe smp 4
#$ -binding linear:4


cd kw/ukbb-gene-diet/scripts

# Sensitivity model 1: sex + age + age^2 + genetic PCs as interaction covariates
singularity exec \
	-B ../data/processed:/data \
	-B /broad/ukbb/imputed_v3:/bgendir \
	-B /humgen/diabetes/UKBB_app27892:/sampledir \
	../../singularity/gem-workflow.simg \
	/bin/bash <<EOF
/GEM/GEM \
	--bgen /data/sensitivity_models/full_subset.bgen \
	--maf 0.005 \
	--sample /data/sensitivity_models/full_subset.sample \
	--pheno-file /data/ukbb_diet_gwis_phenos.csv \
	--sampleid-name id \
	--pheno-name hba1c \
	--pheno-type 0 \
	--exposure-names ${EXP} \
	--covar-names cov_GENO_ARRAYUKBL \
	--int-covar-names sex age age_squared PC1 PC2 PC3 PC4 PC5 PC6 PC7 PC8 PC9 PC10 \
	--delim , \
	--missing-value NA \
	--robust 1 \
	--threads 4 \
	--out /data/sensitivity_models/${EXP}_SM1
EOF

# Sensitivity model 2: additionally adjust for birthplace and study center
singularity exec \
	-B ../data/processed:/data \
	-B /broad/ukbb/imputed_v3:/bgendir \
	-B /humgen/diabetes/UKBB_app27892:/sampledir \
	../../singularity/gem-workflow.simg \
	/bin/bash <<EOF
/GEM/GEM \
	--bgen /data/sensitivity_models/full_subset.bgen \
	--maf 0.005 \
	--sample /data/sensitivity_models/full_subset.sample \
	--pheno-file /data/ukbb_diet_gwis_phenos.csv \
	--sampleid-name id \
	--pheno-name hba1c \
	--pheno-type 0 \
	--exposure-names ${EXP} \
	--covar-names cov_GENO_ARRAYUKBL \
	--int-covar-names sex age age_squared PC1 PC2 PC3 PC4 PC5 PC6 PC7 PC8 PC9 PC10 bpElsewhere bpEngland bpNorthern.Ireland bpRepublic.of.Ireland bpScotland bpWales acBirmingham acBristol acBury acCardiff acCroydon acEdinburgh acGlasgow acHounslow acLeeds acLiverpool acManchester acMiddlesborough acNewcastle acNottingham acOxford acReading acSheffield acStoke acSwansea acWrexham \
	--delim , \
	--missing-value NA \
	--robust 1 \
	--threads 4 \
	--out /data/sensitivity_models/${EXP}_SM2
EOF

# Sensitivity model 3: additionally adjust for physical activity, smoking, BMI, and educational attainment
singularity exec \
	-B ../data/processed:/data \
	-B /broad/ukbb/imputed_v3:/bgendir \
	-B /humgen/diabetes/UKBB_app27892:/sampledir \
	../../singularity/gem-workflow.simg \
	/bin/bash <<EOF
/GEM/GEM \
	--bgen /data/sensitivity_models/full_subset.bgen \
	--maf 0.005 \
	--sample /data/sensitivity_models/full_subset.sample \
	--pheno-file /data/ukbb_diet_gwis_phenos.csv \
	--sampleid-name id \
	--pheno-name hba1c \
	--pheno-type 0 \
	--exposure-names ${EXP} \
	--covar-names cov_GENO_ARRAYUKBL \
	--int-covar-names sex age age_squared PC1 PC2 PC3 PC4 PC5 PC6 PC7 PC8 PC9 PC10 bpElsewhere bpEngland bpNorthern.Ireland bpRepublic.of.Ireland bpScotland bpWales acBirmingham acBristol acBury acCardiff acCroydon acEdinburgh acGlasgow acHounslow acLeeds acLiverpool acManchester acMiddlesborough acNewcastle acNottingham acOxford acReading acSheffield acStoke acSwansea acWrexham bmi smk_012 \
	--delim , \
	--missing-value NA \
	--robust 1 \
	--threads 2 \
	--out /data/sensitivity_models/${EXP}_SM3
EOF

# Sensitivity model 4: as SM3 but including individuals with diabetes
singularity exec \
	-B ../data/processed:/data \
	-B /broad/ukbb/imputed_v3:/bgendir \
	-B /humgen/diabetes/UKBB_app27892:/sampledir \
	../../singularity/gem-workflow.simg \
	/bin/bash <<EOF
/GEM/GEM \
	--bgen /data/sensitivity_models/full_subset.bgen \
	--maf 0.005 \
	--sample /data/sensitivity_models/full_subset.sample \
	--pheno-file /data/ukbb_diet_gwis_phenos.csv \
	--sampleid-name id \
	--pheno-name hba1c_withDM \
	--pheno-type 0 \
	--exposure-names ${EXP} \
	--covar-names cov_GENO_ARRAYUKBL \
	--int-covar-names sex age age_squared PC1 PC2 PC3 PC4 PC5 PC6 PC7 PC8 PC9 PC10 bpElsewhere bpEngland bpNorthern.Ireland bpRepublic.of.Ireland bpScotland bpWales acBirmingham acBristol acBury acCardiff acCroydon acEdinburgh acGlasgow acHounslow acLeeds acLiverpool acManchester acMiddlesborough acNewcastle acNottingham acOxford acReading acSheffield acStoke acSwansea acWrexham bmi smk_012 \
	--delim , \
	--missing-value NA \
	--robust 1 \
	--threads 4 \
	--out /data/sensitivity_models/${EXP}_SM4
EOF
