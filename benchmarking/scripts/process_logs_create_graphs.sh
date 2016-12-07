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
            casestudy=$OPTARG
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

if [ ${#provers[@]} -eq 0 ]; then
    echo "At least one prover has to be selected (-c)"
    exitLater=true
fi

if [ "$casestudy" != "QL" ] && [ "$casestudy" != "SQL" ]; then
    echo "The choosen casestudy should be QL or SQL (-s)"
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

# summarize
summariesDir="datasets/Extended${casestudy}StudyResults"
sbt -mem 4096 "run --logxls ${summariesDir}/summary-raw.xls --logoverviewxls ${summariesDir}/summary-overview.xls --summarizelogs ${pathToLogs}"
./scripts/layout_create_graphs.sh -s ${casestudy}
