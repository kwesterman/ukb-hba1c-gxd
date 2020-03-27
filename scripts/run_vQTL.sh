#!/bin/sh


chr=$1
dir=/broad/hptmp/kwesterm/ukb_bfiles
osca=~/kw/opt/osca_Linux

source /broad/software/scripts/useuse

cd ~/kw/ukbb-gene-diet/scripts

## Create phenotype file
#use R-3.5
#R --slave <<EOF
#library(tidyverse)
#phenos <- read_csv("../data/processed/ukbb_diet_gwis_phenos.csv")
#cov_lm <- lm(hba1c ~ sex + age + age_squared + PC1 + PC2 + PC3 + PC4 + PC5 + PC6 + PC7 + PC8 + PC9 + PC10, data=phenos, na.action=na.exclude)
#phenos$hba1c_resid <- resid(cov_lm)
#phenos %>% mutate(FID=id, IID=id) %>% select(FID, IID, hba1c_resid) %>% write_tsv("${dir}/ukbb_hba1c_resid.pheno")
#EOF

## Generate list of unique SNPs from .bim files
#awk '{print $2}' ${dir}/chr${chr}.bim | sort | uniq > ${dir}/chr${chr}_unique_snps.txt

## Run vQTL
# ${osca} \
#	--vqtl \
#	--bfile ${dir}/chr${chr} \
#	--pheno ${dir}/ukbb_hba1c_resid.pheno \
#	--vqtl-mtd 2 \
#	--out ../data/processed/hba1c_vqtl/chr${chr}
#	#--extract-snp ${dir}/chr${chr}_unique_snps.txt \

# Run associated GWAS
plink2=../../opt/plink2
${plink2} \
	--bfile ${dir}/chr${chr} \
	--pheno ${dir}/ukbb_hba1c_resid.pheno \
	--pheno-name hba1c_resid \
	--glm \
	--out ../data/processed/hba1c_ME/chr${chr}
