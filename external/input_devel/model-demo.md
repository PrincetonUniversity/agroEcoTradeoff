---
title: "Model demo"
author: "Lyndon Estes"
date: "13 January 2015"
output: 
  html_document:
    highlight: tango
    theme: spacelab
    toc: yes 
  pdf:
bibliography: trade.bib
---

## Spatial Tradeoff Model (___trademod___) 
+ Evaluates tradeoffs between:
    + Meeting production targets for key crops
    + Satisfying other land use constraints, including biodiversity and carbon protection
+ Four primary components:

    1. `targets` - determines amount of production increases needed for a given scenario
    
    2. `constraints` - determines probability of pixels being converted to meet `targets`
    
        + Can be altered by change in yield potential, or application of 3 constraints
            1. Carbon density of landscape
            2. Biodiversity value (existing PAs, degree of fragmentation, mammal richness)
            3. Transport cost (in development)
            
    3. `convert` - determines which pixels are converted to meet the crop production target
    
    4. `impacts` - assesses impacts in terms of land converted, carbon lost, protected areas transformed, etc. (outstanding)
    
## Some examples

### A few of the input datasets


![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1.png) 

### And a few analyses
#### Let's establish production targets

A doubling in the need for 9 crops (over current production), except for maize (4X) and soybean (8X): 

```r
setwd(path)
currprod <- brick(full_path(path, "external/ext_data/ZA-current-production.tif"))
prod_targ <- c("maize" = 4, "cassava" = 2, "ground" = 2, "cotton" = 2, "soy" = 8, "pulse" = 2, 
               "sunflower" = 2, "sugarcane" = 2, "wheat" = 2)
potprod <- brick(full_path(path, "external/ext_data/ZA-potential-production-current-area.tif"))
target <- targets(prod_targ = prod_targ, currprod = currprod, potprod = potprod, ybeta = NULL)
target
```

```
##           current potential  target target_newland t_pct_currland
## maize      754962    662523 3019848        2357325           21.9
## cassava    660066    931814 1320132         388318           70.6
## ground      44322     93433   88644          -4789          105.4
## cotton      63798     88894  127596          38702           69.7
## soy         15332     11334  122656         111322            9.2
## pulse       14613      5991   29226          23235           20.5
## sunflower    8231     11484   16462           4978           69.8
## sugarcane 1717108      4619 3434216        3429597            0.1
## wheat       67354        38  134708         134670            0.0
##           t_pct_newland
## maize              78.1
## cassava            29.4
## ground             -5.4
## cotton             30.3
## soy                90.8
## pulse              79.5
## sunflower          30.2
## sugarcane          99.9
## wheat             100.0
```

#### And then run `constraints` to see which areas are most likely to be converted
We'll first assume no constraint other than crop yield potential is applied

```r
setwd(path)
con1 <- constraints(y = y, ybeta = NULL, cbetas = c(0, 0, 0), clist = clist)
```

```
## [1] "C : no constraint applied"    "bd : no constraint applied"  
## [3] "cost : no constraint applied"
## [1] "B"
```

```r
names(con1) <- cropnames
plot(con1,axes = FALSE, box = FALSE) 
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

Next we'll assume that we care about protecting carbon and biodiversity also

```r
setwd(path)
con2 <- constraints(y = y, ybeta = NULL, cbetas = c(1, 1, 0), clist = clist)
```

```
## [1] "cost : no constraint applied"
## [1] "C : full constraint applied"
## [1] "bd : full constraint applied"
## [1] "B" "C" "D"
```

```r
names(con2) <- cropnames
plot(con2,axes = FALSE, box = FALSE) 
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

### Lastly, let's see what get's converted given the crop production targets we need to meet

```r
setwd(path)
converted1 <- convert2(con1, target, crop_frac = crop_frac, pot_yield = pot_yield, valind = valind, 
                       cropnames = cropnames)
converted2 <- convert2(con2, target, crop_frac = crop_frac, pot_yield = pot_yield, valind = valind,
                       cropnames = cropnames)
```


If crop yield is the only constraint
![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 

If we also care about carbon and biodiversity
![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7.png) 













