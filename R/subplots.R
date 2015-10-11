plot.new()
par(mfcol = c(3,3))

ot <- data.frame(ot)

ot$colors <- rainbow(length(ot$ind))
ot$ind <- seq(1, length(ot$ind))

inds <- ot$ind
cols <- ot$colors
ot2 <- ot[order(ot$carbon),]
inds2 <- ot2$ind
cols2 <- ot2$colors
ot3 <- ot[order(ot$biodiversity),]
inds3 <- ot3$ind
cols3 <- ot3$colors


barplot(ot$land, names.arg = inds, ylab = "Area Converted (ha)", col = cols, main = "by Area")
barplot(ot$carbon, names.arg = inds, ylab = "Carbon Loss (t)", col = cols)
barplot(ot$biodiversity, names.arg = inds, ylab = "PA Loss (ha)", col = cols, xlab = "Scenario Number")
 
barplot(ot2$land, names.arg = inds2, col = cols2, main = "by Carbon")
barplot(ot2$carbon, names.arg = inds2, col = cols2)
barplot(ot2$biodiversity, names.arg = inds2, col = cols2, xlab = "Scenario Number")

barplot(ot3$land, names.arg = inds3, col = cols3, , main = "by PA")
barplot(ot3$carbon, names.arg = inds3, col = cols3)
barplot(ot3$biodiversity, names.arg = inds3, col = cols3, xlab = "Scenario Number")