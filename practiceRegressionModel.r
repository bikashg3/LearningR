# load data using stata fomart dta data file
library(readstata13)
datStata14 <- read.dta13("Z:/directoryName/A-Net/ANetData/ELAMath2yrMerge_A2A3_4Analysis.dta",generate.factors=T)


# descriptive statistics
str(datStata14)
summary(datStata14)
colnames(datStata14)
dim(datStata14)
nrow(datStata14)
ncol(datStata14)
head(datStata14)
tail(datStata14)
class(datStata14)


# pretty pairs plot
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

# basic regression syntax
attach(datStata14)

reg1 <- lm(Score~avg_A2A3)

summary(reg1)
# nice visual diagnostics
par(mfrow=c(2,2))
plot(reg1)

# useful model fit functions
# model coefficient
coefficients(reg1)

# CIs for model parameters
confint(reg1,level=0.95)

# predicted values
fitted(reg1)

# residuals
residuals(reg1)

# anova table
anova(reg1)

# regerssion diagnostics
influence(reg1)

# pretty ouput
# package memisc

# One of the aims of this package is to make life easier for
# useRs who deal with survey data sets. It provides an
# infrastructure for the management of survey data including
# value labels, definable missing values, recoding of variables,
# production of code books, and import of (subsets of) SPSS and
# Stata files. Further, it provides functionality to produce
# tables and data frames of arbitrary descriptive statistics and
# (almost) publication-ready tables of regression model
# estimates, which can be exported to LaTeX and HTML.

# quadratic term
reg2 <- lm(Score~avg_A2A3+I(avg_A2A3^2))

# no intercept term
reg3 <- lm(Score~avg_A2A3-1)

pretty.table <- mtable("Model 1"=reg1, "Model2"=reg2,"Model3"=reg3,
                       summary.stats=c("R-squared","F","p","N"))
pretty.table

# specification
avg_A2A3_100 <- avg_A2A3*100
summary(lm(Score~avg_A2A3_100+Grade+female+stEconomicallyDisadvantaged
           + IEP + stEthnicity ))

# model exploration 2
summary(lm(Score~I(avg_A2A3_100^2)+Grade+female+stEconomicallyDisadvantaged
           + IEP + stEthnicity ))


# model exploration 3
summary(lm(Score~log(avg_A2A3_100)+Grade+female+stEconomicallyDisadvantaged
           + IEP + stEthnicity ))


# model exploration 4
summary(lm(Score~log(avg_A2A3_100)+Grade+female+stEconomicallyDisadvantaged
           + IEP ))


# model exploration 5
summary(lm(Score~avg_A2A3_100+Grade+female+stEconomicallyDisadvantaged
           + IEP+I(avg_A2A3_100*Grade*female*stEconomicallyDisadvantaged
                   * IEP) ))
# non-numeric argument to binary operator

# keep all, drop intercept
summary(lm(Score~avg_A2A3_100+Grade+female+stEconomicallyDisadvantaged
           + IEP-1))


# drop one level in dummy variable, for example if we have dum as dummy variable
summary(lm(Score~avg_A2A3_100+Grade+female+stEconomicallyDisadvantaged
           + IEP+dum))


# standarized regression coefficients(beta)

install.packages("lm.beta")

library(lm.beta) 

reg1.beta<- lm.beta(reg1)
reg1.beta
summary(reg1.beta)

# robust standard errors
# Robust regression is an alternative to least squares regression when data are contaminated with 
# outliers or influential observations, and it can also be used for the purpose of 
# detecting influential observations.
install.packages( "sandwhich" ) 
install.packages( "lmtest" )
library(sandwich)
library(lmtest)

# reproduce the Stata default
# robust; HC1(Stata default)
coeftest(reg1,vcov=vcovHC(reg1,"HC1"))

# check that "sandwich" returns HC0(number zero)
coeftest(reg1,vcov=sandwich) #robust:sandwich

coeftest(reg1,vcov=vcovHC(reg1,"HC0")) #robust: HCO


# check that the default robust var-cov matrix is HC3
coeftest(reg1, vcov=vcovHC(reg1))

coeftest(reg1,vcov=vcovHC(reg1,"HC3")) #robust:HC3(default)

detach()
