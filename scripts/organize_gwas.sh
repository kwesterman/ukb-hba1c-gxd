#!/bin/bash


wdir=~/kw/ukbb-gene-diet/data/processed/hba1c_vqtl
cd $wdir
head -1 chr1.vqtl > hba1c_all_chr.vqtl
for chr in {1..22} X; do 
	tail -n +2 chr${chr}.vqtl >> hba1c_all_chr.vqtl; 
done

wdir=~/kw/ukbb-gene-diet/data/processed/hba1c_ME
cd $wdir
head -1 chr1.hba1c_resid.glm.linear > hba1c_all_chr.gwas
for chr in {1..22} X; do 
	tail -n +2 chr${chr}.hba1c_resid.glm.linear >> hba1c_all_chr.gwas; 
done
