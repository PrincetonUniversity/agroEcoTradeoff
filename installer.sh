#!/bin/bash

echo "Installing agroEcoTradeoffs"
echo "Fetching demo datasets"
cd external/data/dt/latest/
wget https://www.dropbox.com/s/9tmsbxtdfo0gzxi/demodat.zip

echo "Decompressing demo data"
unzip demodat.zip
rm demodat.zip

cd ../../..
R CMD BATCH installer.R
