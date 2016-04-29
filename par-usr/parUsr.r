# https://chitchatr.wordpress.com/2012/09/18/parusr-is-my-new-friend-for-inserting-legends-in-plots/
# Create fake data
x1 <- seq(0.1, 10, 0.5) + rnorm (length(seq(0.1, 10, 0.5)), 0.25)
y1 <- seq(0.1, 10, 0.5) + rnorm (length(seq(0.1, 10, 0.5)), 0.25)

# Plot
plot(x1, y1, pch = 20, cex = 1.5,col = c("red"))

# Determine plot boundaries, in units of the data
# par("usr"):A vector of the form c(x1, x2, y1, y2) giving 
# the extremes of the user coordinates of the plotting region.
xmin <- par("usr")[1]
xmax <- par("usr")[2]
ymin <- par("usr")[3]
ymax <- par("usr")[4]

#Now determine the size of the legend you would like to plot.  Right now the exact
#location is not important, we just want to know the dimension!  Note that we are
# treating the lengend as a variable and we are NOT plotting the legend on the figure!
# plot=F: logical. If FALSE, nothing is plotted but the sizes are returned

lgd <- legend(x = mean(c(xmin,xmax)), y =  mean(c(ymin,ymax)),
              c("Your data name"), pch = c(20), col = c("red"), plot = F)

# Add legend in the lower right corner:
legend(x = xmax - lgd$rect$w, y =  ymin + lgd$rect$h,
       c("Your data name"), pch = c(20), col = c("red"), plot = T)




