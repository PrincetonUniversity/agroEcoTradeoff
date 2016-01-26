---
title: "Changes"
author: "Lyndon Estes"
date: "January 21, 2016"
output: 
  html_document:
    highlight: tango
    theme: spacelab
    toc: yes 
---

# Upgrades for version 2

1. Current production becomes non-spatial, adjusted with coefficient

     + Notes: 
         + currprod.rda is main file to read

     + Status: __Done__
     

2. Fraction available cropland needs to be incorporated

     + Status: 
         + __Done__ in `convert`
         + __Done__ in `impacts`
     
3. New biodiversity impact metric incorporated

     + Status: Done

# Fixes

1. Yield standardization procedure

    + Notes: 
        + Might need to move this back to standardizing against each crop
        + Current version is correct, checked by Marcus

# Sequence

1. `fetch_inputs`

    + including altering carbon-names.rda, dumping cropnames.rda entirely
    + Updated fetch_inputs to read in convertible fraction data.table, and to find spatial metadata. 
    + Put in logic to check for equality of row number in input data.tables, and to call a halt if there are NAs in any of the inputs. 

2. `input_handler`

    + fixes to `yield_mod_dt`
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

3. `targets_dt`

    + renamed to targets
    + replaced scheme for modifying current production with simple modification vector, which draws on vector of current production, with argument named `currprodmod`. 
    + yield_mod argument removed. 
    
4. `constraints_dt`

    + renamed to `targets`
    + allocation logic checked
    + j loop in inner allocation loop replaced with data.table-centric code

5. `convert_dt`

    + Now `convert`
    + Replaced j for loop with data.table syntax

6. `impact_dt`

    + Now `impact`
    + Modularized - impact metrics now calculated in sub-modules, except for total area converted. 
        + Revised metrics for cost - mean cost and cost/yield calculated also - these will be more appropriate than total cost
        + New metrics for biodiversity, based on rarity. Several flavors to choose from - mean vegetation type rarity of converted pixels (0-1); rarity + protectedness (0 - 1); average intactness (0 - 1); ha of forest reserves lost
    + Reduced number of input arguments to 2 (il and conv).

7. `tradeoff_mod`

    + Updated to incorporate changes to 4 primary sub-modules. 

8. `tradeoff_batch`

    + Simplified (yield_mod disabled) and parallelized with foreach and doMC. 
    + Output saving options altered. Conversion tables written out without xy coordinates, to save disk space and write times. 
    

9. `pareto`

    + Modularized 
       + `pareto_step` function created, removing this portion of code from inside main function
       + `batch_params` combines parameter permutations into matrix used by `pareto` and `tradeoff_batch`
       + `batch_stat` calculates summarizes impact metrics of interest from a batch run, which are fed to `non-dominator`. 
       + `non-dominator` Separate function created for code that removes non-dominated solutions from output table of impacts
       + Function engine is `tradeoff_batch`. 


