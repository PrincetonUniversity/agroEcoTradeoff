#' Calculates impacts of cropland conversions
#' @param conv data.table of cropland conversions
#' @param carbon data.table of veg and soil carbon stocks
#' @param pot_yield data.table of potential crop yields (modified as needed)
#' @param div_list list of diversity data.tables
#' @param crop_frac data.table of cropland fractions
#' @param cropnames Vector of crop names in analysis 
#' @param ha Pixel area
#' @details Calculates summary statistics of the impacts of cropland 
#' conversions, including total tons carbon lost, carbon loss/ton crop yield,
#' total area converted per crop, total area loss of PAs, mean diversity of
#' converted pixels, mean diversity/ton crop yield
#' @note Some estimate of fragmentation might be useful here. I have to fix
#' impacts to not double-count where winter-crops are grown (wheat here)
#' @examples
#' rc <- run_code(input_key = "ZA")  
#' il <- input_handler(input_key = "ZA", ybetas = c(1, 1), code = rc, 
#'                     ybeta_update = 1)
#' CRSobj <- projection(raster("external/ext_data/ZA-carbon.tif"))
#' ha <- res(raster("external/ext_data/ZA-carbon.tif"))[2]^2 / 10000
#' prod_targ <- c("maize" = 4, "cassava" = 2, "ground" = 2, "cotton" = 2, 
#'                "soy" = 2, "pulse" = 2,"sunflower" = 2, "sugarcane" = 2, 
#'                "wheat" = 2)
#' ybetas <- list(1, 1)
#' cbetas <- c(1, 1, 0, 0)
#' names(cbetas) <- c("Ag", "C", "bd", "cost")
#' impnames <- c("carbon", "richness", "pas")
#' 
#' # target
#' target <- targets_dt(prod_targ = prod_targ, currprod = il$currprod, 
#'                      potprod = il$pp_curr)
#' # constraints module 
#' c_prob <- constraints_dt(inlist = il[c("y_std", "carbon_p", "cons_p", 
#'                          "cost")], 
#'                          cbetas = cbetas, code = rc, 
#'                          cropnames = il$cropnames)
#' 
#' # convert module
#' converted <- convert_dt(conv_prob = c_prob, target = target, 
#'                         crop_frac = il$cropfrac, 
#'                         pot_yield = il$p_yield, cropnames = il$cropnames, 
#'                         base = il$mask, ha = ha, keep_index = FALSE)
#' # impacts
#' impacts <- impact_dt(conv = converted, 
#'                      carbon = il$carbon[, c("veg", "soil"), with = FALSE], 
#'                      pot_yield = il$p_yield, div_list = il[impnames[2:3]], 
#'                      crop_frac = il$cropfrac, cropnames = il$cropnames, 
#'                      ha = ha)

#' @export
impact_dt <- function(conv, carbon, pot_yield, div_list, cost, crop_frac, cropnames,
                      ha) {
   cn <- cropnames
   mymean <- function(x) mean(x[x > 0], na.rm = TRUE)
   mysum <- function(x) sum(x, na.rm = TRUE)
   # areas of conv crops (No longer multiplied by the crop fraction.)
   conv_area <- conv[, cn, with = FALSE] * ha  
   conv_area[, tot := Reduce(`+`, .SD)] # total area of conversion
   # all veg & 25% soil OC, per ha
   closs <- data.table(closs = carbon[, ((veg + soil * 0.25))])
   # C loss/C yield
   cyld <- unlist(lapply(cn, function(i) {
     ((conv[, i, with = FALSE] * closs) / 
      pot_yield[, i, with = FALSE])[, lapply(.SD, mymean)]
   }))
   # Total C loss
   closs_sum <- unlist(lapply(cn, function(i) {
    (conv_area[, i, with = FALSE] * closs)[, lapply(.SD, sum, na.rm = TRUE)]
   }))
   
   # Total cost
   cost_sum <- unlist(lapply(cn, function(i) {
     (conv_area[, i, with = FALSE] * cost)[, lapply(.SD, sum, na.rm = TRUE)]
   }))

   # Biodiversity loss - species richness
   bdloss <- do.call(rbind.data.frame, lapply(cn, function(x) {
     bdmu <- unlist(unname((conv[, c(x), with = FALSE] * 
                             div_list[["richness"]])[, lapply(.SD, mymean)]))
     bdyld <- unname(((conv[, c(x), with = FALSE] * div_list[["richness"]]) / 
                       (pot_yield[, c(x), with = FALSE]))[,lapply(.SD, mymean)])                        
     bdyld <- unlist(bdyld)
     paloss <- unlist(unname((ifelse(!is.na(div_list[["pas"]][[1]]), 1, 0) * 
                               conv_area[, c(x), 
                                         with = FALSE])[, lapply(.SD, mysum)]))
     #natmu <- unlist(unname((div_list[, c("wheat"), with = FALSE] * 
     #                         div_list[[3]][[1]])[, lapply(.SD, mymean)]))
     #round(cbind.data.frame(bdmu, bdyld, paloss, natmu), 1)
     cbind(round(bdmu, 1), round(bdyld, 1), round(paloss))
   }))
   conv_areat <- round(conv_area[, lapply(.SD, mysum), .SDcol = cn])
   crop_impacts <- cbind.data.frame(t(conv_areat), bdloss, round(cyld, 1),
                                    closs_sum, cost_sum)
   colnames(crop_impacts) <- c("conv_area", "rich_mu", "rich/t_yld", "pa_loss",
                               "tC/t_yld", "C_tot", "cost_tot")
   return(crop_impacts)
}

