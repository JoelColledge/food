#!/bin/bash

set -x

FILE="$1"

AUTH_KEY="$(cat "auth_key")"

curl -X POST https://content.dropboxapi.com/2/files/upload \
  --header "Authorization: Bearer $AUTH_KEY" \
  --header "Dropbox-API-Arg: {\"path\": \"/$FILE\",\"mode\": \"add\",\"autorename\": true,\"mute\": false}" \
  --header "Content-Type: application/octet-stream" \
  --data-binary @"$FILE"

echo
