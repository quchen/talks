TempClean
======

Automatically empty your trash


```bash
#!/usr/bin/env bash

set -euo pipefail

export TEMP="$HOME/temp"

if ! which trash > /dev/null; then
    echo "No trash command installed!"
    exit 1
fi

delete() {
    filename="$1"
    echo "trash \$TEMP${filename#"$TEMP"}"
    trash "$filename"
}
export -f delete

find "$TEMP" -mindepth 1 -maxdepth 1 -mtime +10 -print0 | xargs -0 -I {} bash -c "delete '{}'"
```
