# For making pretty pairs plot in basic regression plot
# when read lecture note, it appeared unknow function usage as par("usr")

# https://chitchatr.wordpress.com/2012/09/18/parusr-is-my-new-friend-for-inserting-legends-in-plots/

# For this example I will show my solution to always having the legend 
# in the lower right corner (since I knew ahead of time the data would not fall in that region).

# Create fake data
x1 <- seq(0.1, 10, 0.5) + rnorm (length(seq(0.1, 10, 0.5)), 0.25)
y1 <- seq(0.1, 10, 0.5) + rnorm (length(seq(0.1, 10, 0.5)), 0.25)

# Plot
plot(x1, y1, pch = 20, cex = 1.5,col = c("red"))

# Determine plot boundaries, in units of the data
# par("usr"):A vector of the form c(x1, x2, y1, y2) giving 
# the extremes of the user coordinates of the plotting region.

# > par("usr")
# [1]  0.3129914 12.2706279 -0.5050859 11.0621410

xmin <- par("usr")[1]
xmax <- par("usr")[2]
ymin <- par("usr")[3]
ymax <- par("usr")[4]

#Now determine the size of the legend you would like to plot.  Right now the exact
#location is not important, we just want to know the dimension!  Note that we are
# treating the lengend as a variable and we are NOT plotting the legend on the figure!

# plot=F: logical. If FALSE, nothing is plotted but the sizes are returned

lgd <- legend(x = mean(c(xmin,xmax)), y =  mean(c(ymin,ymax)),
              plot = F)
#  pch = c(20),col = c("red"),
# c("Your data name"),
# > lgd
# $rect
# $rect$w
# [1] 3.689372
# 
# $rect$h
# [1] 1.970986
# 
# $rect$left
# [1] 6.29181
# 
# $rect$top
# [1] 5.278528
# 
# 
# $text
# $text$x
# [1] 7.024595
# 
# $text$y
# [1] 4.293034

# Add legend in the lower right corner:
legend(x = xmax - lgd$rect$w, y =  ymin + lgd$rect$h,
       c("Your data name"), pch = c(20), col = c("red"), plot = T)

#####################################################################################################################

# also for making pairs plot looks nicer

# will give out correlation matrix in the bottom left triangle
# have the correlation number in the upper right triangle
# also define to show statistic significant level

attach(datStata14)

dat <- data.frame(avg_A2A3,Score,A1PercentCorrect,
                  A2PercentCorrect,A3PercentCorrect,A4PercentCorrect)
panel.cor <- function(x, y, digits=2, prefix="", cex.cor)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex <- 0.8/strwidth(txt)
  test <- cor.test(x,y)
  # borrowed from printCoefmat
  Signif <- symnum(test$p.value, corr = FALSE, na = FALSE,
                   cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                   symbols = c("***", "**", "*", ".", " "))
  text(0.5, 0.5, txt, cex = 1.5 )
  text(.7, .8, Signif, cex=cex, col=2)
}
pairs( dat, lower.panel=panel.smooth, upper.panel=panel.cor)

detach()
