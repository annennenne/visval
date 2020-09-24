#!/bin/bash
#$ -S /bin/bash
#$ -N makeplotspar
#$ -cwd
#$ -pe smp 60
echo "start time"
date
R CMD BATCH makeplots_parallel.R
echo "end time"
date
