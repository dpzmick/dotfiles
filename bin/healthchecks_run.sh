#!/bin/bash

set -o pipefail
# don't set -e because we want failure to make it all the way to the end, not
# sure that's actually happening here

if [ "$#" -lt 2 ]; then
    echo "Usage: URL cmd..."
    exit 1
fi

url=$1

echo "Sending start message"
curl --silent -fsS --retry 3 -X GET ${url}/start
echo

# run the rest of the arguments as a command
${@:2}

rc=$?
if [ ${rc} -eq 0 ]; then
    echo "Success"
    curl --silent -fsS --retry 3 -X GET ${url} # done!
    echo
else
    echo "Command ${@:2} failed"
    echo "Sending fail message"
    curl --silent -fsS --retry 3 -X GET ${url}/fail
    echo
fi
exit ${rc}
