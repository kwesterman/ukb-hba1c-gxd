#!/bin/sh


chr=$1
dir=/broad/hptmp/kwesterm/ukb_bfiles
plink2=../../opt/plink2

source /broad/software/scripts/useuse

cd kw/ukbb-gene-diet/scripts

${plink2} \
	--bgen /broad/ukbb/imputed_v3/ukb_imp_chr${chr}_v3.bgen \
	--sample /humgen/diabetes/UKBB_app27892/ukb27892_imp_chrX_v3_s486743.sample \
	--maf 0.005 \
	--make-bed \
	--out ${dir}/chr${chr}
	#--out ../data/processed/ukb_plinksets/chr${chr}
	#--sample /humgen/diabetes/UKBB_app27892/ukb27892_imp_chrAUT_v3_s487395.sample \

cp ${dir}/chr${chr}.bim ${dir}/chr${chr}.bim_raw

${plink2} \
	--bfile ${dir}/chr${chr} \
	--set-all-var-ids @:#_\$r_\$a \
	--new-id-max-allele-len 650 \
	--make-just-bim \
	--out ${dir}/chr${chr}

if [ chr = "X" ]
then
	cp ${dir}/chr${chr}.bim ${dir}/chr${chr}.bim_tmp
	sed 's/X/23/g' ${dir}/chr${chr}.bim_tmp > ${dir}/chr${chr}.bim
fi
