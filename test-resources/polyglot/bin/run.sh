#!/bin/bash
# Records a session, trains on it, and reports.
set -euo pipefail

DIR="$(cd "$(dirname "$0")" && pwd)"
source "$DIR/env.sh"

record() {
  local minutes="$1"
  if [[ -z "$minutes" ]]; then
    echo "usage: run.sh minutes" >&2
    return 1
  fi
  python scripts/train.py "$PORT" &
  wait
}

record "${1:-5}"
bb tasks/report.clj
./bin/deploy
cat <<EOF
Finished. Don't forget to "upload" the results.
EOF
echo "$#"
