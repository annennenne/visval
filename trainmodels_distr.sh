#!/bin/bash
#$ -N train_qq
#$ -t 1:36     # Number of jobs
#$ -q all.q    # Queue. Use long.q for run time >8h and all.q otherwise
#$ -l h_vmem=12G # Memory limit, e.g. reserve 1 GB memory 
#$ -tc 128      # Max concurrent jobs
#$ -cwd         # Run in current directory
#$ -o output/qq  # Direct output to subdirectory
#$ -e output/qq   # Direct output to subdirectory

echo "start time"
date
R CMD BATCH trainmodels_qq.R output/qq/$JOB_NAME-I-$SGE_TASK_ID.Rout --no-restore --no-save
echo "end time"
date
