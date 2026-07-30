#!/usr/bin/env bash
set -euo pipefail

# The cached Gmail OAuth token in .secrets/ is too large for a single
# GitHub Actions secret (~48 KB limit), so this splits its base64 form
# into chunks and sets/clears the GMAIL_OAUTH_TOKEN_1..MAX_PARTS repo
# secrets accordingly. Re-run this any time the token cache is
# regenerated (e.g. after re-running gm_auth()).
#
# Usage: scripts/set_gmail_oauth_secret.sh [repo]
# Defaults to pteridogroup/ppg-voting if not given.

REPO="${1:-pteridogroup/ppg-voting}"
MAX_PARTS=10
CHUNK_BYTES=40000

TOKEN_FILE=$(find .secrets -maxdepth 1 -type f \
  -name '*pteridogroup.no.reply@gmail.com*')
if [ -z "$TOKEN_FILE" ]; then
  echo "No cached token file found under .secrets/" >&2
  exit 1
fi

TMP_DIR=$(mktemp -d)
trap 'rm -rf "$TMP_DIR"' EXIT

base64 -w0 "$TOKEN_FILE" > "$TMP_DIR/token.b64"
split -b "$CHUNK_BYTES" -d -a 2 "$TMP_DIR/token.b64" "$TMP_DIR/part."

NUM_PARTS=$(ls "$TMP_DIR"/part.* | wc -l)
if [ "$NUM_PARTS" -gt "$MAX_PARTS" ]; then
  echo "Token needs $NUM_PARTS parts, but only $MAX_PARTS slots are" \
       "wired into the workflow. Increase MAX_PARTS here and add more" \
       "OAUTH_TOKEN_B64_N entries in .github/workflows/digest.yml." >&2
  exit 1
fi

for i in $(seq -w 00 $((MAX_PARTS - 1))); do
  n=$((10#$i + 1))
  part="$TMP_DIR/part.$i"
  if [ -f "$part" ]; then
    gh secret set "GMAIL_OAUTH_TOKEN_${n}" --repo "$REPO" < "$part"
  else
    gh secret remove "GMAIL_OAUTH_TOKEN_${n}" --repo "$REPO" 2>/dev/null || true
  fi
done

echo "Set $NUM_PARTS chunk secret(s); cleared any unused slots up to" \
     "$MAX_PARTS."
