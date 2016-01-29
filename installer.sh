#!/bin/bash

echo "Installing agroEcoTradeoff"

a=`pwd`'/agroEcoTradeoff'
echo "Setting up file structure"
mkdir -p $a/external/data/ZA
mkdir $a/external/output
mkdir $a/Rmd

cd $a/Rmd
wget https://www.dropbox.com/s/dyeenvfam2oo8cu/tradeoff-simulator.html
wget https://www.dropbox.com/s/97910nhavhbjg6x/tradeoff-simulator.Rmd

echo "Fetching demo datasets"
cd $a/external/data/ZA
wget https://www.dropbox.com/s/ka91x59ujuqprjn/demodat.zip
wget https://www.dropbox.com/s/k2kys4b7odvyjd2/parks-roads-mask.zip

echo "Decompressing demo data"
unzip demodat.zip
unzip parks-roads-mask.zip
rm demodat.zip
rm parks-roads-mask.zip

echo "Installing R libraries"
cd $a
wget https://www.dropbox.com/s/6cnaz0by3umru6x/installer.R
R CMD BATCH installer.R

echo "Success! (We hope)"