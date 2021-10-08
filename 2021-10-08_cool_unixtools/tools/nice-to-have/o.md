# o

Bridge from terminal to GUI

```bash
if [[ "$#" -ne 0 ]]; then
    xdg-open "$@"
else
    xdg-open .
fi
```
