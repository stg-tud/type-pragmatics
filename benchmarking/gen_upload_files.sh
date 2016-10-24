#!/bin/bash
# need to install p7zip via your preferred package manager
# need to create a rsa-key pair with ssh-keygen and copy it with ssh-copy-id to the cluster

# -u <username> -t <timeout> -c <name> -c <name> -p <path to encodings>

# collect values after flags
while getopts "u:c:p:t:" opt; do
    case $opt in
        u)
            username=$OPTARG
            ;;
        c)
            provers+=("$OPTARG")
            ;;
        t)
            timeout=$OPTARG
            ;;
        p)
            pathToCompiledSQLFiles=$OPTARG
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

if ! test "$timeout" ; then
    echo "Timeout is obligatory (-t)"
    exitLater=true
fi

if ! test "$pathToCompiledSQLFiles" ; then
    echo "Path to directory of encodings is obligatory (-d)"
    exitLater=true
fi

if [ ${#provers[@]} -eq 0 ]; then
    echo "At least one prover has to be selected (-c)"
    exitLater=true
fi

if [ "$exitLater" = true ] ; then
    exit 1
fi

proverArg=""
for val in "${provers[@]}"; do
    proverArg="${proverArg} -c $val"
done

# first run slurmscriptmaker /main
sbt "run -t ${timeout} ${proverArg} --generateSLURM $pathToCompiledSQLFiles"

# zip jobscripts
pathToJobScripts=datasets/HHLRJobScripts
jobscripts=jobscripts.7z
7z a $jobscripts $pathToJobScripts

# zip proverinput (7z)
pathToProverInputFiles=datasets/HHLRInputFiles
proverinput=proverinput.7z
now=`date +"%Y-%m-%d"`
7z a $proverinput ${pathToProverInputFiles}/${now}

# upload via sftp
projectID=proj_184
cluster=lcluster9
sshPath=${username}@${cluster}.hrz.tu-darmstadt.de

sftp $sshPath << SFTP
cd /home/groups/projects/${projectID}/runprovers/SlurmScripts
put $jobscripts
cd /home/groups/projects/${projectID}/Encodings
put $proverinput
exit
SFTP

rm $jobscripts $proverinput

ssh $sshPath << SSH
cd /home/groups/projects/${projectID}/runprovers/SlurmScripts
7z e ${jobscripts} -aoa
cd /home/groups/projects/${projectID}/Encodings
mkdir $now
7z e ${proverinput} -o${now} -aoa
exit
SSH
