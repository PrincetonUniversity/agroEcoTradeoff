# agroEcoTradeoff

___Note___: ___This is the development branch. Instructions below are likely to be broken.___ 

A spatial tradeoff analysis model, focusing on minimizing the ecological costs of agricultural expansion.  Based on Koh and Ghazoul (2010). 

## Installation

Installation is in several steps. First, you should have installed already the following: 

  + R
  + Rstudio
  + gdal (mac users should get the frameworks from [kyngchaos](http://www.kyngchaos.com/software/frameworks), get the gdal complete framework)

Once you have those installed, download and the installer script to a folder of your choosing on your computer, run the following lines within a terminal: 

```
wget https://github.com/PrincetonUniversity/agroEcoTradeoff/raw/master/installer.sh
chmod +x installer.sh
./installer.sh
```

## Running the model

If all went well, the model should work. See [tradeoff-simulator](Rmd/tradeoff-simulator.Rmd) for an example of how to run the model. 

