---
title: "Model overview"
author: "Lyndon Estes"
date: "06 January 2015"
output: 
  html_document:
    highlight: tango
    theme: spacelab
    toc: yes 
bibliography: trade.bib
---

The model (`trademod`) has 5 major modules: 

1. __Master__ module, where flexible parameters are set and runs controlled. The master module calls the following modules/functions in order: 
2. __Targets__, which calculates the amount of each crop (in tonnes) that needs to be produced. 
+ This may also readjust the cropland fractional share as a function of the relative demand increase for each crop. 
3. __Constraints__, which applies the various land use constraints on top of the main constraint, which is the land's productivity for a given crop. The formulation of the land use constraints follows Koh and Ghazoul [-@koh_spatially_2010]:

    $$
    \begin{aligned}
     C_{ij} = \beta_0Y_{ij} \beta_1c1_{ij} \beta_2c2_{ij} \beta_3c3_{ij}
    \end{aligned}
    $$

    Where $C_{ij}$ are the total constraints on production for crop i in pixel j,  $Y$ is crop i's yield potential, $c1-3$ are constraints for carbon, biodiversity, and transport costs, respectively, and $\beta_0$ is a positive coefficent (from 0 to some arbitary multiple, but the maximum should be line with realistic potential yield gains) that modifies the yield potential of crop i in some way. Here we will use grids representing climate change impacts and irrigation-based increases, respectively, to provide these values, so $\beta_0$ will be the product of two coefficients. $\beta_{1-3}$ range between 0-1, where 0 means the constraint is not applied, and 1 that it is fully applied, with values in between representing partial weights. Theoretically, this value could be greater than 1, which would simply place a greater weight on that constraint. This means that: 
    
    $$
    \begin{aligned}
     P_{ij} = C_{ij} 
    \end{aligned}
    $$

    That is, the probability $P$ that pixel j is converted to crop i is equivalent to the product of the constraints $C$. Let's demonstrate whether this is the correct way of going about things or not, that is, multiplicatively as opposed to additively. First, we'll set up a dummy matrix representing three constraints (c1-3) on three pixels (p1-3).  


    
    ```r
    options(width = 90)
    set.seed(3)
    pmat <- sapply(1:3, function(x) sample(1:100, 3) / 100)
    pmat
    ```
    
    ```
    ##      [,1] [,2] [,3]
    ## [1,] 0.17 0.33 0.13
    ## [2,] 0.80 0.60 0.30
    ## [3,] 0.38 0.99 0.57
    ```
Here's are the rankings when we assume that all pixels have a $\beta$ of 1 and we multiply, and also when we add constraints and divide by 3. Here we'll just see whether each pixel's probability rank is the same under the different schemes.
   
    
    ```r
    unname(rank(apply(pmat, 1, prod)) == rank(rowSums(pmat) / 3))
    ```
    
    ```
    ## [1] TRUE TRUE TRUE
    ```
    So they produce the same ranks.  But let's try this a bit more robustly, with more pixels and more random permutations of probabilities. 
    
    
    ```r
    set.seed(3)
    rank_diffs <- sapply(1:1000, function(x) {
      pmat <- t(sapply(1:10, function(x) sample(1:100, 10) / 100))
      d <- rank(apply(pmat, 1, prod), ties.method = "first") - rank(rowSums(pmat) / 10, ties.method = "first")
      d[d != 0] <- 1
      sum(d)
    })
    hist(rank_diffs)
    ```
    
    ![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 
    
    So these are not at all the same. In any case, let's go with multiplying, since we are creating a probability, and probabilities should be multiplied.  

4. __Convert__, which converts the area of cropland necessary to hit $T_i$, which is the target production in tonnes for crop i: 

    $$
    \begin{aligned}
      A_i =  \sum_{j=max(P_i)}^{t} fa_i
    \end{aligned}
    $$
    
    Where $A_i$ is the total area converted, as determined by summing the area of each pixel $a$ converted for crop i, multiplied by the fraction $f$ available in that pixel for converting to that crop (predetermined and possibly modified by the __Targets__ module). Conversion begins in the pixel where $P_i$ is greatest, and ends in pixel $t$, which is the last pixel where: 
    
    $$
    \begin{aligned}
      T_i \leq \sum_{j=max(P_i)}^{t} fa_iY_i
    \end{aligned}
    $$
    
5. __Impacts__, which, in addition to the total area converted will assess some impacts for each scenario, which must still be developed, such as the total $CO_2$ emissions, some sort of biodiversity impact metric, some water use metric, etc. Ideas for biodiversity

+ Species loss metric (e.g. matrix weighted species area curve; [@koh_spatially_2010]$)
+ Fragmentation metric
+ Mean richness metric per converted pixel
    

#### References
