#!/bin/sh


#$ -l os=RedHat7
#$ -l h_vmem=10G
#$ -l h_rt=12:00:00
#$ -j y

#$ -pe smp 4
#$ -binding linear:4


cd kw/ukbb-gene-diet/scripts

# Write files of quantitative and binary traits underlying dPCs
head -1 ../data/processed/ukbb_diet_gwis_phenos.csv \
	| tr ',' '\n' \
	| grep -s -E '_QT|_bin' > ../data/processed/diet_trait_names.txt

# Run primary model for each diet trait, for all suggestive variants
cat ../data/processed/diet_trait_names.txt | while read trait; do
singularity exec \
	-B ../data/processed:/data \
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
		--exposure-names ${trait} \
		--covar-names sex age age_squared PC1 PC2 PC3 PC4 PC5 PC6 PC7 PC8 PC9 PC10 cov_GENO_ARRAYUKBL \
		--delim , \
		--missing-value NA \
		--robust 1 \
		--threads 4 \
		--out /data/sensitivity_models/${trait}
EOF
done
