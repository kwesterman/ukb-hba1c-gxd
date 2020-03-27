#!/bin/sh

source /broad/software/scripts/useuse
use R-3.5

export PATH=/home/unix/kwesterm/kw/opt/pandoc-2.9.2/bin:$PATH

cd kw/ukbb-gene-diet/scripts

Rscript -e 'rmarkdown::render("2b_diet_qc.Rmd")'
