#' Impacts module 1 (carbon)
#' @param il List of inputs created by input_handler
#' @param conv_mat binary matrix of cropland conversions
#' @param conv_area matrix of converted area
#' @return data.frame of total C loss and mean carbon loss to yield ratio
#' @keywords internal
#' @export
impact_mod1 <- function(il, conv, conv_area) {
 
  # potential C loss
  pot_closs <- il$carbon[, (veg + soil * 0.25)]  # hard-coded loss assumption

  # total C loss
  tot_closs <- conv_area[, lapply(.SD, function(x) sum(x * pot_closs))]
  
  # mean C loss / yield
  cn <- il$cropnames
  mymean <- function(x) mean(x[x > 0], na.rm = TRUE)
  closs <- data.table((as.matrix(conv[, cn, with = FALSE]) * pot_closs)  / 
    as.matrix(il$p_yield))
  closs_yld <- closs[, lapply(.SD, mymean)]
  
  out_stat <- data.frame(round(rbind(tot_closs, closs_yld), 2))
  rownames(out_stat) <- c("tot_C", "C_yld")
  # out_stat <- cbind("var" = c("tot_C", "C_yld"), 
                    # round(rbind(tot_closs, closs_yld), 2))
  return(out_stat)
}

#' Impacts module 2 (cose)
#' @param il List of inputs created by input_handler
#' @param conv_mat binary matrix of cropland conversions
#' @param conv_area matrix of converted area
#' @return data.table of costs
#' @keywords internal
#' @export
impact_mod2 <- function(il, conv, conv_area) {
  
  # Total cost
  cn <- il$cropnames
  
  # total cost
  tot_cost <- conv[, lapply(.SD, function(x) sum(x * il$cost)), .SDcols = cn]
  
  mymean <- function(x) mean(x[x > 0], na.rm = TRUE)
  # mean cost
  cost_mu <- conv[, lapply(.SD, function(x) mymean(x * il$cost[[1]])), 
                  .SDcols = cn]
  
  # mean cost / yield
  cost <- data.table((as.matrix(conv[, cn, with = FALSE]) * il$cost[[1]])  / 
                       as.matrix(il$p_yield))
  cost_yld <- cost[, lapply(.SD, mymean)]
  
  out_stat <- data.frame(cbind(round(rbind(tot_cost, cost_mu, cost_yld), 2)))
  rownames(out_stat) <- c("tot_cost", "mu_cost", "cost_yld")
  # out_stat <- cbind("var" = c("tot_cost", "mu_cost", "cost_yld"), 
                    # round(rbind(tot_cost, cost_mu, cost_yld), 2))
  return(out_stat)
}

#' Impacts module 3 (biodiversity)
#' @param il List of inputs created by input_handler
#' @param conv_area matrix of converted area
#' @return data.frame of total biodiversity loss, measured as rarity weighted ha
#' @keywords internal
#' @export
impact_mod3 <- function(il, conv_area) {
  
  cn <- il$cropnames
  wv <- copy(il$bdprops)[2:3, ]
  
  # impact in terms of mean BD weight of converted hectares (two flavors)
  bdimp <- do.call(cbind, lapply(cn, function(x) { # x <- "maize"
   
    # select converted areas
    conv_mat <- conv_area[[x]]  
    cid <- which(conv_mat > 0)  # converted areas
    
    # adjust total converted areas to account for additional unfarmable areas
    bd_conv_area <- as.matrix(il$bd[cid, ]) * il$sp$ha
    bd_conv_areaf <- bd_conv_area * (conv_mat[cid] / rowSums(bd_conv_area))
    
    # total converted area
    tot_conv_area <- round(sum(bd_conv_areaf))
    
    gt0 <- function(x) length(which(x > 0))
   
    # calculate mean BD weight, across both types
    metric <- unique(wv$var)
    imps <- do.call(rbind, lapply(1:nrow(wv), function(y) {
      wvdt <- wv[var == metric[y]]  
      wvnms <- names(wvdt)[!names(wvdt) %in% "var"]
      wvec <- as.matrix(wvdt[, wvnms, with = FALSE])[1, ]
      ha_wgt <- rowSums(sweep(bd_conv_areaf, MARGIN = 2, wvec, `*`))
      imp <- sum(ha_wgt) / tot_conv_area  
      out_stat <- data.frame(imp)
      dimnames(out_stat) <- list(metric[y], x)
      out_stat
    }))
    
    # other measures
    intpa_mat <- as.matrix(il$intpa[cid, ])
    int_mean <- mean(intpa_mat[, 1])  # mean intactness of converted areas
    int_prior <- mean(c(int_mean, imps["priority", 1]))  # mean of int + prior 
    fpa_conv <- sum(conv_mat[which(intpa_mat[, 2] == 3)])  # converted for res
    
    # assemble
    fragmet <- data.frame(rbind(int_prior, int_mean, fpa_conv))
    colnames(fragmet) <- x
    
    # output
    out_stat <- round(rbind(imps, fragmet), 3)
    return(out_stat)
  }))  
  # out_stat <- cbind("var" = rownames(bdimp), data.table(bdimp))
  out_stat <- bdimp
}
