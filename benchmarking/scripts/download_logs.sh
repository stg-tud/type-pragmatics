#!/bin/bash
while getopts "d:u:p:" opt; do
    case $opt in
        u)
            username=$OPTARG
            ;;
        d)
            date=$OPTARG
            ;;
        p)
            pathToDownloadTo=$OPTARG
            ;;
    esac
done
shift $((OPTIND -1))

# check if a needed flag is not set
exitLater=false
if ! test "$username" ; then
    echo "Username is obligatory (-u)"
    exitLater=true
fi

if ! test "$date" ; then
    echo "Date is obligatory (-d format yyyy-mm-dd)"
    exitLater=true
fi
if ! test "$pathToDownloadTo" ; then
    echo "Download path is obligatory (-p)"
    exitLater=true
fi

if [ "$exitLater" = true ] ; then
    exit 1
fi
timeout=120s
date=2016-10-24
username=ka64uwek
cluster=lcluster9
projectID=proj_184
sshPath=${username}@${cluster}.hrz.tu-darmstadt.de

ssh $sshPath << SSH
cd /work/scratch/groups/projects/${projectID}
pwd
ls
rm -f ${date}.7z
7z a ${date}.7z ${date}
exit
SSH

sftp $sshPath << SFTP
cd /work/scratch/groups/projects/proj_184
get ${date}.7z ${pathToDownloadTo}
exit
SFTP
