#Define Margins. 
par(mar=c(3, 8, 3, 2) + 0.1)

#Plot the first set of bars
barplot(ot$carbon, width = 1, space = 3, axes=F, xlab="", ylab="",col="blue", main="", xlim = c(0,150))
axis(2, at = seq(0, signif(max(ot$carbon), digits = 1), 
                 signif(max(ot$carbon), digits = 1)/4), col="black",lwd=2, line = -1)
mtext(2,text="Total Carbon Lost (t)",line=0.75)



#Plot the second set of bars
par(new=T)
barplot(ot$land, width = 1, space = 3, axes=F, xlab="", ylab="",col="black", main="", xlim = c(1,151))
axis(2, at = seq(0, signif(max(ot$land), digits = 1), 
                signif(max(ot$land), digits = 1)/4),lwd=2,line=2)
mtext(2,text="Total Area Converted (ha)",line=3.75)


#Plot the third set of bars
par(new=T)
barplot(ot$biodiversity, width = 1, space = 3, axes=F, xlab="", ylab="",col="green", main="", xlim = c(2,152))
axis(2, at = seq(0, signif(max(ot$biodiversity), digits = 1), 
                 signif(max(ot$biodiversity), digits = 1)/4), lwd=2,line=5)
mtext(2,text="Total PA Loss (ha)",line=6.75)


#Dumy x-axis
axis(1, labels = FALSE, at = c(2, 150))
mtext("",side=1,col="black",line=2)


#Plot the legend
legend(x=80,y= max(ot$biodiversity + 1000),
       legend=c("biodiversity","land","carbon"), fill = c("green", "black", "blue"), bty = "n")