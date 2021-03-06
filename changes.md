---
title: "Changes"
author: "Lyndon Estes"
output: 
  html_document:
    highlight: tango
    theme: spacelab
    toc: yes 
---

# v2.0.1

## Improvements
+ Added (back) ability to select a subset of crops, based on target values passed into `targets`, via `tradeoff_mod`, `tradeoff_batch`, and `pareto`. Switch is made in `input_handler`. 
+ Reordered arguments in `tradeoff_mod`.
+ Added save for batch parameters to `tradeoff_batch`. 
+ Added more plot flexibility (N crops plotted, colors) to `imp_plot`. 
+ Added `heat` to create raster showing frequency of conversions.
+ `tradeoff_batch` passes out the bcode now
+ `pareto` uses bcode passed from `tradeoff_batch`, and saves optitab to batch directory. 
+ `tradeoff_batch` impact tables now come out as data.frames from both todisk TRUE and FALSE cases.  Changes to `batch_param` made to accomodate (table rather than list). From the todisk=FALSE case, the output conversion tables are served as a list that is the second element of the output list. 
+ Added `imp_sorter` function for selecting subsets of conversion maps and ordering them to correctly match names of selected impact tables. 
+ Added new measures to `impact` for calculating total travel hours (`impact_mod2`), and total biodiversity cost (`impact_mod3`). 

## Bugs
+ Removed hard-coded `input_key` ("ZA") in `tradeoff_batch` call to `tradeoff_mod`. 
+ Fixed .tif append to `impacts_tab.rda` output from `tradeoff_batch`.  
+ Fixed `tradeoff_batch` so that `impacts_tab.rda` is a data.frame not a list. 
+ Conversion tables from `tradeoff_batch` were being re-ordered because of random numbers/time-stamp structure in `run_code`. The run/batch/name is simplified and fixed, with corresponding changes to `tradeoff_batch`, `tradeoff_mod`, `pareto`. 

# v2.0

## Summary of improvements
1. Current production becomes non-spatial, adjusted with coefficient

     + Notes: 
         + currprod.rda is main file to read

     + Status: __Done__
     

2. Fraction available cropland needs to be incorporated

     + Status: 
         + __Done__ in `convert`
         + __Done__ in `impacts`
     
3. New biodiversity impact metric incorporated

     + Status: Mostly - still need to change the biodiversity metric so that impact can be weighted evenly on protected weighted rarity and intactness

## Changes

+ `fetch_inputs`

    + including altering carbon-names.rda, dumping cropnames.rda entirely
    + Updated fetch_inputs to read in convertible fraction data.table, and to find spatial metadata. 
    + Put in logic to check for equality of row number in input data.tables, and to call a halt if there are NAs in any of the inputs. 
    + removed datasets in R data folder, because they should be installed as part of library, and therefore not called with a specific path by this function.  To allow users to generate their own data. 
        + All inputs now live under `external/data/[ID]`, and are read in directly from there. These changes are designed to separate model from R package structure.  

+ `input_handler`

    + `yield_mod`
        + Name changed from `yield_mod_dt`
        + fixed to remove pp_curr from list of modified rasters
        + noticed potential standardization issue
        + renamed to just `yield_mod`
        + moved yield standardization to input_handler
    + added yield standardization here
    + switched off one potential yield modification option
    + all data.tables, including mask, reduced down to location with >0 farmable areas. 
    + key set on index of reduced mask
    + cost/yield is performed on those areas only
    + rearranged order of inputs to properly deal with recyled input list
    + disabled yield modification function for now, on the assumption that this becomes an offline process for the time being. If ybeta_update is set to 1, model will halt.  

+ `targets`

    + name changed from `targets_dt`
    + replaced scheme for modifying current production with simple modification vector, which draws on vector of current production, with argument named `currprodmod`. 
    + yield_mod argument removed. 
    
+ `constraints`

    + name changed from `contraints_dt`
    + allocation logic checked
    + j loop in inner allocation loop replaced with data.table-centric code

+ `convert`

    + Name changed from `convert_dt`
    + Replaced j for loop with data.table syntax

+ `impact`

    + Name changed from `impact_dt`
    + Modularized - impact metrics now calculated in sub-modules, except for total area converted. 
        + Revised metrics for cost - mean cost and cost/yield calculated also - these will be more appropriate than total cost
        + New metrics for biodiversity, based on rarity. Several flavors to choose from - mean vegetation type rarity of converted pixels (0-1); rarity + protectedness (priority, 0 - 1); average intactness (0 - 1); mean of intactness and priority (the measure currently used by `constraints`); ha of forest reserves lost
        + Added additional biodiversityChanged biodiversity impact to be based on both intactness and rarity - i.e. run it on the cons prioritization layer. 
    + Reduced number of input arguments to 2 (il and conv).

+ `tradeoff_mod`

    + Updated to incorporate changes to 4 primary sub-modules.
    
+ `tradeoff_batch`

    + Simplified (yield_mod disabled) and parallelized with foreach and doMC. 
    + Output saving options altered. Conversion tables written out without xy coordinates, to save disk space and write times. 
    + Outputs now write into `external/output`
    

+ `pareto`

    + Modularized 
       + `pareto_step` function created, removing this portion of code from inside main function
       + `batch_params` combines parameter permutations into matrix used by `pareto` and `tradeoff_batch`
       + `batch_stat` calculates summarizes impact metrics of interest from a batch run, which are fed to `non-dominator`. 
       + `non-dominator` Separate function created for code that removes non-dominated solutions from output table of impacts
       + Function engine is `tradeoff_batch`. 
    + parameter returned with optimization table in output list 

