#!/bin/bash

while getopts "l:c:p:t:s:" opt; do
    case $opt in
        l)
            pathToLogs=$OPTARG
            ;;
        c)
            provers+=("$OPTARG")
            ;;
        t)
            timeouts+=("$OPTARG")
            ;;
        p)
            pathToCompiledSQLFiles=$OPTARG
            ;;
        s)
            summariesDir=$OPTARG
            ;;
    esac
done
shift $((OPTIND -1))

# check if a needed flag is not set
exitLater=false
if ! test "$pathToLogs" ; then
    echo "A path to the directory where the logs are stored is obligatory (-l)"
    exitLater=true
fi

if [ ${#timeouts[@]} -eq 0 ]; then
    echo "At least one Timeout needs to be provided (-t)"
    exitLater=true
fi

if ! test "$pathToCompiledSQLFiles" ; then
    echo "Path to directory of encodings is obligatory (-p)"
    exitLater=true
fi

if ! test "$summariesDir" ; then
    echo "Path to directory where the summaries should be stored is obligatory (-s)"
    exitLater=true
fi
if [ ${#provers[@]} -eq 0 ]; then
    echo "At least one prover has to be selected (-c)"
    exitLater=true
fi

if [ "$exitLater" = true ] ; then
    exit 1
fi

# sort logs
for prover in "${provers[@]}"
do
    for timeout in "${timeouts[@]}"
    do
        sbt "run --sortHHLRoutput ${pathToLogs}/${timeout}s/${prover} ${pathToCompiledSQLFiles}"
    done
done

mkdir -p ${summariesDir}
# summarize
sbt -mem 2048 "run --logxls ${summariesDir}/summary-raw.xls --logoverviewxls ${summariesDir}/summary-overview.xls --summarizelogs ${pathToLogs}"

# layout
sbt "run --layoutData ${summariesDir}"

# execute GraphScript.r
cd datasets/layout
# need to install pryr and ggplot2 r packages
Rscript GraphScript.r

