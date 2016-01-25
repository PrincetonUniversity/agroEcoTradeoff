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
         + __Pending__ in `impacts`
     
3. New biodiversity impact metric incorporated

     + Status: Not yet done

# Fixes

1. Yield standardization procedure

    + Notes: 
        + Might need to move this back to standardizing against each crop
        + Current version is correct, checked by Marcus

# Sequence

1. `fetch_inputs`

    + including altering carbon-names.rda, dumping cropnames.rda entirely
    + Updated fetch_inputs to read in convertible fraction data.table, and to find spatial metadata. 

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

3. `targets_dt`

    + renamed to targets
    + replaced scheme for modifying current production with simple modification vector, which draws on vector of current production, with argument named `currprodmod`. 
    + yield_mod argument removed. 
    
4. `constraints_dt`

    + renamed to `targets`
    + allocation logic checked
    + j loop in inner allocation loop replaced with data.table-centric code



    

Updated fetch_inputs to read in convertible fraction data.table, and to find spatial metadata.

