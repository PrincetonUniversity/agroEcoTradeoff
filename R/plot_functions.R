#' Legend function for model output plots 
#' @param ldim Long dimensions of legend
#' @param sdim Short dimensions of legend
#' @param tdim Text x and y
#' @param legdim x and y coordinate for farmblock area legend
#' @return Three stacked legends using lmisc::flex_legend, one conventional legend
#' @keywords internal
#' @export
leg_fun <- function(ldim, sdim, tdim, legdim, brks, brks2, cx, yor, gr, bp,
                    ladj, textcol = "white", bordercol = "white", lp = NULL) {
  sd2 <- 0.02
  lp <- ifelse(is.null(lp), c(3, 2), lp)
  flex_legend(ncuts = length(brks) - 1, legend.text = "% Existing Cropland",
              legend.vals = brks * 100, horiz = TRUE, legend.pos = lp, 
              leg.adj = ladj, cex.val = cx, longdims = ldim, 
              shortdims = c(sdim[3], sd2), colvec = bp, textcol = textcol,
              bordercol = bordercol)
  flex_legend(ncuts = length(brks2) - 2, 
              legend.text = "In Untransformed Areas", cex.val = cx, 
              legend.vals = round(brks2[-1] * 100), horiz = TRUE, 
              legend.pos = lp, leg.adj = ladj, 
              longdims = ldim, shortdims = c(sdim[2], sd2), colvec = yor[-1], 
              textcol = textcol, bordercol = bordercol)
  flex_legend(ncuts = length(brks2) - 2, 
              legend.text = "In Farm Blocks/Existing Cropland", 
              legend.vals = round(brks2[-1] * 100), cex.val = cx, horiz = TRUE,
              legend.pos = lp, leg.adj = ladj, longdims = ldim, 
              shortdims = c(sdim[1], sd2), colvec = gr[-1], 
              textcol = textcol, bordercol = bordercol)
  par(xpd = NA)
  text(grconvertX(tdim[1], from = "ndc", to = "user"), col = textcol, 
       grconvertY(tdim[2], from = "ndc", to = "user"), "% New Cropland", 
       cex = cx)
  legend(grconvertX(legdim[1], from = "ndc", to = "user"), text.col = textcol,
         border = bordercol, grconvertY(legdim[2], from = "ndc", to = "user"),
         pt.cex = cx * 1.5, legend = "Farm blocks",  
         fill = "grey50", bty = "n", cex = cx) 
}

#' Plots multiple maps showing cropland conversions
#' @param scen List index specification which of several results to read in
#' @return map
#' @keywords internal
#' @export
conv_plot <- function(scen, ldim = c(0.4, 0.75), sdim = c(0.03, 0.12, 0.25),
                      tdim = c(0.58, 0.18), legdim = c(0.8, 0.15), 
                      brks = c(0, 0.01, 0.05, 0.1, 0.15, 0.2), 
                      brks2 = c(0, seq(0.001, 0.601, 0.15)),  
                      yor = c("transparent", 
                              brewer.pal(9, name = "YlOrRd")[5:8]), 
                      gr = c("transparent", 
                             brewer.pal(9, name = "Greens")[5:8]), 
                      bp = c("grey90", 
                             brewer.pal(9, name = "Blues")[c(4, 6, 7, 9)]), 
                      cx = 1.2, ladj=1) {
  par(mar = c(0, 0, 2, 0), mfrow = c(3, 3))
  for(i in 1:length(il$cropnames[!il$cropnames %in% 
                                  c("sugarcane", "wheat")])) {
    r1 <- ((dtrs[[scen]][[i]] > 0) & (cropland > 0.01)) * dtrs[[scen]][[i]]
    r2 <- ((dtrs[[scen]][[i]] > 0) & (fblock > 0)) * dtrs[[scen]][[i]]
    leg <- ifelse(i == 2, TRUE, FALSE)
    plot(cropland, breaks = brks, col = bp, axes = FALSE, box = FALSE, 
         legend = FALSE, main = il$cropnames[i], cex.main = 1.2)
    image(fblock, add = TRUE,  legend = FALSE, col = "grey50")
    image(dtrs[[scen]][[i]], breaks = brks2, axes = FALSE, cex.main = 2, 
          add = TRUE, col = yor, xlab = "", ylab = "")
    image(r1, breaks = brks2, col = gr, legend = FALSE, add = TRUE)
    image(r2, breaks = brks2, col = gr, legend = FALSE, add = TRUE)
  }
 leg_fun(ldim, sdim, tdim, legdim, brks, brks2, cx, yor, gr, bp, ladj)
} 

#' Plots single map of cropland conversions
#' @param r Input raster showing cropland conversions
#' @param ldim Long dimensions of legend
#' @param sdim Short dimensions of legend
#' @param tdim Text x and y
#' @param legdim x and y coordinate for farmblock area legend
#' @return map
#' @keywords internal
#' @export
conv_plot_1 <- function(r, ldim, sdim, tdim, legdim, 
                        brks = c(0, 0.01, 0.05, 0.1, 0.15, 0.2), 
                        brks2 = c(0, seq(0.001, 0.601, 0.15)),  
                        yor = c("transparent", 
                                brewer.pal(9, name = "YlOrRd")[5:8]), 
                        gr = c("transparent", 
                               brewer.pal(9, name = "Greens")[5:8]), 
                        bp = c("grey90", 
                               brewer.pal(9, name = "Blues")[c(4, 6, 7, 9)]), 
                        cx = 1.2, oma = c(8, 0, 0, 0), ladj=1) {
  par(mar = c(0, 0, 0, 0), oma = oma, bg = "transparent")
  r1 <- ((r > 0) & (cropland > 0.01)) * r
  r2 <- ((r > 0) & (fblock > 0)) * r
  #plot(zam, border = "grey90", col = "grey90")
  plot(cropland, breaks = brks, col = bp, axes = FALSE, box = FALSE, 
       legend = FALSE, cex.main = 1.2)#, add = TRUE)
  image(fblock, add = TRUE,  legend = FALSE, col = "grey50")
  image(r, breaks = brks2, axes = FALSE, cex.main = 2, add = TRUE, 
        col = yor, xlab = "", ylab = "")
  image(r1, breaks = brks2, col = gr, legend = FALSE, add = TRUE)
  image(r2, breaks = brks2, col = gr, legend = FALSE, add = TRUE)
  leg_fun(ldim, sdim, tdim, legdim, brks, brks2, cx, yor, gr, bp, ladj)
} 
