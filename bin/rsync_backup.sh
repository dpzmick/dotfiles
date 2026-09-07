#!/bin/zsh
#set -xe
set -e

export PATH=/sbin/:$PATH

echo "Running as $USER"

host=worf.chi.dpzmick.com
#host=100.85.121.19

# Bail out and let healthchecks staleness warnings detect that we've stopped
# backing up
if ! ping -c4 ${host} >/dev/null;
then
    echo "Could not reach remote host"
    exit 2
fi

# FIXME maybe don't put this on the internet?

who=$(hostname -s)
src=/home/dpzmick/
dstdir=/nas/backups/${who}
dst=dpzmick@${host}:${dstdir}/
tmpfile=$(mktemp /tmp/rsync_backup.XXXXXX)
trap 'rm -f ${tmpfile}' EXIT

if [ ${who} = "picard" ]; then
    healthchecks_url="https://hc-ping.com/397996f9-18b9-4e68-b9cf-9ef6b7c0fa33"
elif [ ${who} = "spock" ]; then
    healthchecks_url="https://hc-ping.com/f5e2ff62-eb66-42ec-8679-93c2311aeeda"
elif [ ${who} = "worf" ]; then
    healthchecks_url="https://hc-ping.com/f5ffa77c-30b9-439b-b24c-63b6bc7a64b3"
elif [ ${who} = "tpring" ]; then
    # tpring is osx
    src=/Users/dpzmick/
    healthchecks_url="https://hc-ping.com/693dc4b0-d29d-4bfb-be20-35556f8ce550"
else
    echo "Don't know what healthchecks URL to use for ${who}"
    exit 1
fi

echo "Backing up ${src} to ${dst} (healthchecks url ${healthchecks_url})"

# write the excludes file to a temp file
cat << EOF > ${tmpfile}
/.AMDuProf/
/.BitwigStudio/
/.Rack/
/.Trash/
/.audacity-data/
/.cache/
/.cargo/
/.config/
/.config/google-chrome/
/.debug/
/.duplicacy/
/.elan/
/.emacs.d/
/.fzf/
/.gradle/
/.julia/
/.jupyter/
/.lein/
/.local/
/.mozilla/
/.nix-defexpr/
/.nix-profile/
/.npm/
/.opam/
/.renderdoc/
/.rustup/
/.spack/
/.steam/
/.winbox/
/.wine/
/.zoom/
/Library/
/builds/
/dotfiles/config.symlink/google-chrome/
/dropbox
/go/
/icloud
/qemu/
/spack/
EOF

# don't let a healthchecks outage block the backup itself
echo "Sending start message"
curl --silent -fsS --retry 3 -X GET ${healthchecks_url}/start >/dev/null || true

cd ~/
rc=0

# icloud/dropbox are symlinks into cloud storage; back their contents up to
# sibling dest dirs (e.g. ${who}-icloud) so the main backup's --delete-excluded
# never touches them (macOS openrsync ignores 'P' protect filters)
for cloud in icloud dropbox; do
    [ -d ~/${cloud} ] || continue
    rsync -avx --delete ~/${cloud}/ dpzmick@${host}:${dstdir}-${cloud}/ || rc=$?
done

rsync -avx --delete --delete-excluded \
    --exclude-from=${tmpfile} ${src} ${dst} || rc=$?

if [ ${rc} -eq 0 ]; then
    echo "Success!"
    curl --silent -fsS --retry 3 -X GET ${healthchecks_url} >/dev/null || true # done!
else
    echo "Backup failed (rc=${rc})... Sending fail message"
    curl --silent -fsS --retry 3 -X GET ${healthchecks_url}/fail >/dev/null || true
fi

exit ${rc}
