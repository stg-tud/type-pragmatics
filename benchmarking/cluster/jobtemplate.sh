#!/bin/sh
# Job name
#BSUB -J <job name>
#
#BSUB -u <mailaddress>
#BSUB -u <another mailaddress>
#BSUB -N
#
# File / path where STDOUT will be written, the %J is the job id
#BSUB -o Standardoutput.out%J
#
# File / path where STDERR will be written, the %J is the job id
#BSUB -e Erroroutput.err%J
#
# Request the time you need for execution in minutes
# The format for the parameter is: [hour:]minute,
# that means for 80 minutes you could also use this: 1:20
#BSUB -W 0:10
#
# Request virtual memory you need for your job in MB
#BSUB -M 256000
#
# Request the number of compute slots you want to use
#BSUB -n 65
#

timeout=10
date=2016-07-03
path=~/output/${date}/
fullpath=~/output/${date}/${timeout}s/

mkdir $path
mkdir $fullpath

./runthis /home/groups/projects/proj_184/provers/<prover> /home/groups/projects/proj_184/AllEncodings ${fullpath} -t ${timeout} --mode casc --proof tptp --output_axiom_names on