#!/bin/bash


res_dir=../data/processed/main_ffq_PC_gwis_res
sugg_thresh=1e-5
sugg_dir=../data/processed/sensitivity_models

# Gather suggestive variants based on traits
(cd $res_dir && ls *_bin*merged *_QT*merged) | sed 's/_merged//g' > ../data/processed/30traits.txt
echo "" > ../data/processed/trait_sugg_variants.txt
cat ../data/processed/30traits.txt | while read trait; do
	echo $trait "..."
	awk -v thresh=$sugg_thresh '$13 < thresh {print $2}' $res_dir/${trait}_merged \
		>> ${sugg_dir}/trait_sugg_variants.txt
done

# Gather suggestive variants based on dPCs
echo "ffq_PC1 ffq_PC2 ffq_PC3 ffq_PC4 ffq_PC5 ffq_PC6 ffq_PC7 ffq_PC8 ffq_PC9 ffq_PC10 ffq_PC_ERS10 ffq_PC_ERS10adj" | tr ' ' '\n' > ../data/processed/ffq_PCs.txt
echo "" > ../data/processed/ffq_PC_sugg_variants.txt
cat ../data/processed/ffq_PCs.txt | while read PC; do
	echo $PC "..."
	awk -v thresh=$sugg_thresh '$13 < thresh {print $2}' $res_dir/${PC}_merged \
		>> ${sugg_dir}/ffq_PC_sugg_variants.txt
done

# Concatenate suggestive trait and dPC variants
cat <(echo "rsid") ${sugg_dir}/trait_sugg_variants.txt ${sugg_dir}/ffq_PC_sugg_variants.txt \
	> ${sugg_dir}/all_sugg_variants.txt

# Use qctool v2 to subset 
source /broad/software/scripts/useuse
use GCC-5.2
qctool="../../opt/qctool_v2.0.6-CentOS\ Linux7.3.1611-x86_64/qctool"

for chr in {1..22}; do
	eval "${qctool}" \
		-g /broad/ukbb/imputed_v3/ukb_imp_chr${chr}_v3.bgen \
		-incl-rsids ${sugg_dir}/all_sugg_variants.txt \
		-og ${sugg_dir}/all_sugg_chr${chr}.bgen \
		-os ${sugg_dir}/all_sugg_chr${chr}.sample
done

## Merge subsets
cp ${sugg_dir}/all_sugg_chr1.bgen ${sugg_dir}/all_sugg_base.bgen
cp ${sugg_dir}/all_sugg_chr1.sample ${sugg_dir}/all_sugg_base.sample
for chr in {2..22}; do
	echo "Merging chromosome ${chr}..."
	eval "${qctool}" \
		-g ${sugg_dir}/all_sugg_base.bgen \
		-s ${sugg_dir}/all_sugg_base.sample \
		-merge-in ${sugg_dir}/all_sugg_chr${chr}.bgen ${sugg_dir}/all_sugg_chr${chr}.sample \
		-og ${sugg_dir}/all_sugg_full.bgen \
		-os ${sugg_dir}/all_sugg_full.sample
	cp ${sugg_dir}/all_sugg_full.bgen ${sugg_dir}/all_sugg_base.bgen
	cp ${sugg_dir}/all_sugg_full.sample ${sugg_dir}/all_sugg_base.sample
done
rm ${sugg_dir}/all_sugg_base.bgen ${sugg_dir}/all_sugg_base.sample
