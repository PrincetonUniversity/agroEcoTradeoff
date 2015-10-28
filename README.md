# agroEcoTradeoff

A spatial tradeoff analysis model, focusing on minimizing the ecological costs of agricultural expansion.  Based on Koh and Ghazoul (2010). 

## Installation

Installation is in several steps. First, you should have installed already the following: 

  + R
  + Rstudio
  + gdal (mac users should get the frameworks from [kyngchaos](http://www.kyngchaos.com/software/frameworks), get the gdal complete framework)

Once you have those installed, download the full repository to your computer. The unzipped directory will give you the full directory you need to run the model.  

Navigate into the directory you unzipped on your computer.  Find the `agroEcoTradeoff.Rproj` file, and double-click it. This should open up an RStudio project for you.  

<<<<<<< HEAD
Use the files navigation pane to find the `installer.R` script, which will execute the installation of the model packageand its dependencies.  
=======
Use the files navigation pane to find the `installer'.R` script, which will execute the installation of the model packageand its dependencies.  
>>>>>>> fa1682b50897f98deb197f0c1ba6ec7743380c12

Once you have done that, download the following files: 

[input files](https://www.dropbox.com/s/wt0ygdl4rejiz6s/dt.zip?dl=0)
<<<<<<< HEAD

=======
>>>>>>> fa1682b50897f98deb197f0c1ba6ec7743380c12
[mask file](https://www.dropbox.com/s/ncy76swgnhqx98h/ZA-mask.tif?dl=0)

The first should be added into your model directory under `external/data/`, and unzipped there. The second can be downloaded straight to the same location. You should end up with a file `ZA-mask.tif` under `external/data/`, and a folder 
`external/data/dt` with 9 csv files in it.

If all went well, the model should work.  See `optimization-runner.Rmd` and `tradeoff-simulator.Rmd` for examples of how to run the model. 

