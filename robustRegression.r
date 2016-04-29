# www.ats.ucla.edu/stat/r/dae/rreg.htm

require(foreign)
require(MASS)

cdata <- read.dta("http://www.ats.ucla.edu/stat/data/crime.dta")
summary(cdata)


# begin by running an OLS regression and looking at diagnostic plots examining residuals, 
# fitted values, Cook's distance, and leverage.
summary(ols <- lm(crime ~ poverty + single, data = cdata))

opar <- par(mfrow = c(2,2), oma = c(0, 0, 1.1, 0))
plot(ols, las = 1)

par(opar)


cdata[c(9, 25, 51), 1:2]

# A conventional cut-off point is 4n, where n is the number of observations in the data set. 
# We will use this criterion to select the values to display. 

d1 <- cooks.distance(ols)
r <- stdres(ols)# stdres: The standardized residuals. These are normalized to unit variance, 
                # fitted including the current data point.
a <- cbind(cdata, d1, r)
a[d1 > 4/51, ]

# absolute residual values

rabs <- abs(r)
a <- cbind(cdata, d1, r, rabs)
asorted <- a[order(-rabs), ]
asorted[1:10, ]

# iterated re-weighted least squares (IRLS)

summary(rr.huber <- rlm(crime ~ poverty + single, data = cdata))

# look at the final weights created by the IRLS process
hweights <- data.frame(state = cdata$state, resid = rr.huber$resid, weight = rr.huber$w)
hweights2 <- hweights[order(rr.huber$w), ]
hweights2[1:15, ]

# bisquare weighting function
rr.bisquare <- rlm(crime ~ poverty + single, data=cdata, psi = psi.bisquare)
summary(rr.bisquare)

biweights <- data.frame(state = cdata$state, resid = rr.bisquare$resid, weight = rr.bisquare$w)
biweights2 <- biweights[order(rr.bisquare$w), ]
biweights2[1:15, ]

