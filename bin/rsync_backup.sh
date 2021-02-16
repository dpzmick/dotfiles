#!/bin/sh

if ! ping -c4 worf.chi.dpzmick.com >/dev/null;
then
    echo "Could not reach remote host"
    exit 2
fi

who=$(hostname)
healthchecks_url=https://hc-ping.com/397996f9-18b9-4e68-b9cf-9ef6b7c0fa33
src=/home/dpzmick/
dst=worf.chi.dpzmick.com:/nas/backups/${who}/
tmpfile=$(mktemp /tmp/rsync_backup.XXXXXX)

echo "Backing up ${src} to ${dst}"

# write the excludes file to a temp file
cat << EOF > ${tmpfile}
/.AMDuProf/
/.BitwigStudio/
/.audacity-data/
/.cache/
/.cargo/
/.config/
/.debug/
/.duplicacy/
/.emacs.d/
/.fzf/
/.gradle/
/.julia/
/.jupyter/
/.lein/
/.local/
/.mozilla/
/.npm/
/.opam/
/.renderdoc/
/.rustup/
/.steam/
/.winbox/
/.wine/
/.zoom/
/builds/
/go/
EOF

echo "Sending start message"
curl --silent -fsS --retry 3 -X GET ${healthchecks_url}/start >/dev/null

cd ~/
rsync -avx --delete --delete-excluded --exclude-from=${tmpfile} ${src} ${dst}

# capture return code
rc=$?

if [ ${rc} -eq 0 ]; then
    echo "Success!"
    curl --silent -fsS --retry 3 -X GET ${healthchecks_url} >/dev/null # done!
else
    echo "Backup failed... Sending fail message"
    curl --silent -fsS --retry 3 -X GET ${healthchecks_url}/fail >/dev/null
fi
exit ${rc}

# cleanup temp file
rm ${tmpfile}

# exit
exit ${rc}
