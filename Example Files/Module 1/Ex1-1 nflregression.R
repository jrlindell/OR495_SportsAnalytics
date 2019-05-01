# Read in Y and X from csv as a matrix
nfldata = read.csv("Ex1-1 nfldata.csv", sep=",")

# convert data to matrix so we can do linear algebra operations
M <- as.matrix(nfldata)
Y = M[,1]
X = M[,2:10]

# solve for coefficients: inv(X'X)X'Y 
coefs = solve(t(X) %*% X, t(X) %*% Y)

# do regression from the book
book.model = lm(Margin~.-Intercept, data=nfldata)
summary(book.model)

# compare optimization to regression
data.frame("OPT"=coefs, "REG.MODEL"=book.model$coefficients)


### Independent X's?
pairs(nfldata[,c(-1,-2)])
# zoom in...  X's do not appear to be correlated

library(car)
# VIF = 1/(1-R^2) for R^2 in a model for EACH X variable vs the others
# measures how well each X is predicted as a function of the other X's
sqrt(vif(book.model)) #>2 indicates that multicollinearity may be an issue
# all <2, so we'll assume the linear assumption holds


### Errors have ZERO mean
# this one is simple -> use residuals as an estimate for errors
mean(book.model$residuals)
# VERY close to zero!


### Errors are Normally distributed
# test Normality of Residuals
# qq plot for studentized resid
qqPlot(book.model, main="QQ Plot")
# distribution of studentized residuals

library(MASS)
sresid <- studres(book.model) 
hist(sresid, freq=FALSE, 
     main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit)
# QQ plot and histogram look pretty Normal


### Errors have constant variance
# plot residuals vs fitted values
# looking for a "shotgun" spead
# if variation (vertical) seems to change
# as you move horzontally, that indicates that
# ERROR is not constant (heteroskedastic!)
plot(book.model$fitted.value,book.model$residuals)
# looks like a pretty good spread

# or use ncvTest, where Ho: constant error variance and
# H1: error variance is not constant
# if pvalue is small, there is strong evidence to suggest
# that the error variance is not constant
ncvTest(book.model)
# Fail to reject Ho!


# See http://www.statmethods.net/stats/rdiagnostics.html for more info

### Use "Plot" function to see most of these all at once!
plot(book.model)

