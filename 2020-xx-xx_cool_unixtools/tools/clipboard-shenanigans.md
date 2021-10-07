Clipboard shenanigans
=========

Type clipboard’s contents
-----

```bash
sleep 1
xdotool type --delay 50 "$(xsel --clipboard -o)"
```


Copy text from picture
--------

```bash
xfce4-screenshooter --region --open cat \
    | tesseract stdin stdout -l eng+deu \
    | tr -dc '[:print:]'
```
