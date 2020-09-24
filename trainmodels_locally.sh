#!/bin/bash
echo "start time"
date
for i in {1..36}
do 
    echo $i 
    export type="scatter"
    export reverse="FALSE"
    export j=$i 
    R CMD BATCH --no-save --no-restore trainmodels_locally.R output/$type/$type-rev-$reverse-$i.Rout &
    #if (($i % 2 == 0))
    #then wait
    #fi
done
echo "end time"
date

#./trainmodels_locally.sh 1>/dev/null 2>&1 &
#./trainmodels_locally.sh > output/scatter/r0_bash_big3.out 2>&1 &
