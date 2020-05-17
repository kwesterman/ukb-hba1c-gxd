#!/bin/bash


exp=$1
if [ $exp = "ffq_PC_multiExp10" ]; then p_col=121; else p_col=13; fi

source /broad/software/scripts/useuse
use R-3.5

echo "Creating summary files for ${exp}..."

sumstats_file=../data/processed/main_ffq_PC_gwis_res/${exp}

head -1 ../data/processed/main_ffq_PC_gwis_res/${exp}_chr1 > ${sumstats_file}_merged
for chr in {1..22} X; do
	echo "Chromosome " ${chr}
	tail -n +2 ../data/processed/main_ffq_PC_gwis_res/${exp}_chr${chr} >> ${sumstats_file}_merged
done

echo "Zipping..."
gzip < ${sumstats_file}_merged > ${sumstats_file}_merged.gz

echo "Creating suggestive subset..."
head -1 ${sumstats_file}_merged > ${sumstats_file}_merged_sugg
awk -v col=$p_col '$col < 1e-5' ${sumstats_file}_merged >> ${sumstats_file}_merged_sugg

echo "Generating summary report..."
sumstats_full_path=$(readlink -f ${sumstats_file}_merged)
Rscript -e 'rmarkdown::render("../../gxe_utils/summarize_GWIS.Rmd", output_file="gwis_summary.pdf", params=list(p_marginal_col="P_Value_Marginal"))' $sumstats_full_path \
	&& mv ../../gxe_utils/gwis_summary.pdf ../output/${exp}_summary.pdf

#echo "Writing p-value file..."
#tail -n +2 ../data/processed/main_ffq_PC_gwis_res/ffq_PC${pc}_merged | awk '{print $13}' > ../data/processed/main_ffq_PC_gwis_res/ffq_PC${pc}_merged_pvals.txt

#awk '{print $2,$3,$4,$6,$10,$13}' ../data/processed/main_ffq_PC_gwis_res/ffq_PC${pc}_merged > ../data/processed/main_ffq_PC_gwis_res/ffq_PC${pc}_merged_minimal
#gzip < ../data/processed/main_ffq_PC_gwis_res/ffq_PC${pc}_merged_minimal > ../data/processed/main_ffq_PC_gwis_res/ffq_PC${pc}_merged_minimal.gz
