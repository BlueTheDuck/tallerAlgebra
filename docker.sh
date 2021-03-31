#!/bin/sh
echo "Mounted $(pwd) in /mnt, use :l /mnt/file.hs to load $(pwd)/file.hs"
docker run -it --rm -e "TERM=xterm-256color" --mount type=bind,src=$(pwd),dst=/mnt --name "haskell" haskell:8
