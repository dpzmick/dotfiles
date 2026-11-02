#!/bin/bash
# usage: omapi.sh METHOD PATH [json-body]   (PATH relative to /openapi/v1/$OMADA_ID)
set -euo pipefail
. ~/.config/omada/openapi.env
tokfile=~/.config/omada/.token
if [ ! -s "$tokfile" ] || [ $(( $(date +%s) - $(stat -c %Y "$tokfile") )) -gt 3000 ]; then
  curl -sk -X POST "$OMADA_URL/openapi/authorize/token?grant_type=client_credentials" \
    -H "Content-Type: application/json" \
    -d "{\"omadacId\":\"$OMADA_ID\",\"client_id\":\"$OMADA_CLIENT_ID\",\"client_secret\":\"$OMADA_CLIENT_SECRET\"}" \
    | jq -er ".result.accessToken" > "$tokfile"
fi
curl -sk -X "$1" "$OMADA_URL/openapi/v1/$OMADA_ID$2" -H "Authorization: AccessToken=$(cat "$tokfile")" \
  -H "Content-Type: application/json" ${3:+-d "$3"}
