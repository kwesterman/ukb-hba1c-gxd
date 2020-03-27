#!/bin/bash


datadir=../data/raw

../../opt/plink2 --pgen $datadir/all_phase3.pgen \
	--pvar $datadir/all_phase3_noannot.pvar \
	--psam $datadir/phase3_corrected.psam \
	--keep-if SuperPop = EUR \
	--maf 0.005 \
	--make-pgen \
	--out ../data/processed/1kg/1kg_ld_ref
