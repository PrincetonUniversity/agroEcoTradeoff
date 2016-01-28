#!/bin/bash

echo "Installing agroEcoTradeoffs"
echo "Fetching demo datasets"

a=`pwd`

mkdir -p $a/agroEcoTradeoffs/external/data/ZA
mkdir $a/agroEcoTradeoffs/external/output


cd $a/external/data/ZA
wget https://www.dropbox.com/s/ka91x59ujuqprjn/demodat.zip

echo "Decompressing demo data"
unzip demodat.zip
rm demodat.zip

cd $a/external/data/
wget https://www.dropbox.com/s/k2kys4b7odvyjd2/parks-roads-mask.zip
rm parks-roads-mask.zip

cd $a
wget https://www.dropbox.com/s/6cnaz0by3umru6x/installer.R
R CMD BATCH installer.R

echo "Success! (We hope)"