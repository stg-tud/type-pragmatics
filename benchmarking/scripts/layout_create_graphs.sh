#!/bin/bash
while getopts "s:" opt; do
    case $opt in
        s)
            casestudy=$OPTARG
            ;;
    esac
done
shift $((OPTIND -1))

if [ "$casestudy" != "QL" ] && [ "$casestudy" != "SQL" ]; then
    echo "The choosen casestudy should be QL or SQL (-s)"
    exitLater=true
fi

if [ "$exitLater" = true ] ; then
    exit 1
fi

summariesDir="datasets/Extended${casestudy}StudyResults"
layoutDir="datasets/layout/${casestudy}"
sbt "run --layoutData ${layoutDir} ${summariesDir}"

# execute GraphScript.r
cd datasets/layout
# need to install pryr and ggplot2 r packages
Rscript GraphScript.r ${casestudy}
