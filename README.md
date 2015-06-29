# agroEcoTradeoff

A spatial tradeoff analysis model, focusing on minimizing the ecological costs of agricultural expansion.  Based on Koh and Ghazoul (2010). 

## Installation

The model is written in R, and can be installed as an R library as follows: 

```
require(devtools)
install_github("PrincetonUniversity/agroEcoTradeoff")
require(agroEcoTradeoff)
```

This should install the core components of the model and allow you to follow the model overviews and tutorials in the vignettes folder. 

Notes: 

1. model-demo.Rmd is based on old code, so won't work as written.  A more complex demonstration of using the model in batch mode, which should work, is shown in tradeoff-mod-demonstration.Rmd/html. 
2. There are a number of gridded datasets that are not part of the github repo that will likely have to be imported so that something doesn't break. These are input datasets kept in the external/ext_data folder. The exception to this are the .csv files kept in this directory under the dt/ folder. These are included because they support the use of the model for Zambia in the data.table (faster) variant of the model. To run the raster-based version, the grids prefixed with ZA will have to be imported into this folder.  
3. The best way to install is probably to clone the github repo, then add in the missing files from your dropbox access to this directory (assuming you have it), then install the R package.  Hopefully that works!  



