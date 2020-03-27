#!/bin/sh

source /broad/software/scripts/useuse
use GCC-5.2

cd kw/ukbb-gene-diet/data/processed

../../../opt/qctool_v2.0.6-CentOS\ Linux7.3.1611-x86_64/qctool -g /broad/ukbb/imputed_v3/ukb_imp_chr22_v3.bgen -og chr22_random_10k.bgen -os chr22_random_10k.sample -incl-rsids chr22_random_10k_variants.txt
