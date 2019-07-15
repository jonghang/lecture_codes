## Simulation: Independent vs Uncorrelated TS

a0 = 0.2
a1 = 0.5
b1 = 0.3
w = rnorm(5000)
eps = rep(0, 5000)
sigsq = rep(0, 5000)
for (i in 2:5000) {
   sigsq[i] = a0 + a1 * (eps[i-1]^2) + b1 * sigsq[i-1]
   eps[i] = w[i]*sqrt(sigsq[i])
 }
## Plot the acf of the time series and squared time series
acf(eps)
acf(eps^2)

## Financial Data Analysis
library(quantmod)
## Get the daily traiding data for PDCE
getSymbols("PDCE",src="google")
## What does the matrix PDCE include?
PDCE[1,]
candleChart(PDCE,multi.col=TRUE,theme="white")
## Log returns of the close price
pdcert = diff(log(Cl(PDCE)))
plot(pdcert,main="")

###############################################################################
## Fit ARIMA
final.aic = Inf
final.order = c(0,0,0)
for (p in 1:6) for (d in 0:1) for (q in 1:6) {
   current.aic = AIC(arima(pdcert, order=c(p, d, q)))
   if (current.aic < final.aic) {
     final.aic = current.aic
     final.order = c(p, d, q)
     final.arima = arima(pdcert, order=final.order)
   }
 }
## What is the selected order?
#> final.order
#[1] 5 0 6

## Residual Analysis
resids = resid(final.arima)[-1]
acf(resids,main="Residuals of ARIMA Fit")
acf(resids^2,main="Squared Residuals of ARIMA Fit")

# for serial correlation
Box.test((resids)^2,lag=12,type='Ljung',fitdf=11)
# for arch effect
Box.test(resids,lag=12,type='Ljung',fitdf=11)


## Estimate the variance using nonparametric regression 
library(mgcv)
zt.sq.log = log(resids^2)
n = length(resids)
time.pts = c(1:n)
time.pts = (time.pts-min(time.pts))/(max(time.pts)-min(time.pts))
gam.var = gam(zt.sq.log~s(time.pts))
pdcert.obs = pdcert[-1]
pdcert$PDCE.Close[-1]=sqrt(exp(fitted(gam.var)))
pdcert.var.fit=pdcert
plot(pdcert.var.fit,main="")

######## Apply ARCH Model ###################################################
## garch from tseries library
library(tseries)
pacf(resids^2,main="Squared Residuals")
arch.fit = garch(resids, order = c(0, 7),trace=F)
summary(arch.fit)
 resids.fgarch = residuals(arch.fit)[-c(1:7)]
acf(resids.fgarch,main="ACF of ARCH Residuals")
acf(resids.fgarch^2,main="ACF of Squared ARCH Residuals")                    
Box.test(resids.fgarch,lag=10,type='Ljung')
Box.test(resids.fgarch^2,lag=10,type='Ljung')
hist(resids.fgarch,"Histrogram of the Residuals")
qqnorm(resids.fgarch)

## garchFit from the fGarch library
library(fGarch)
archFit.resid = garchFit(~ garch(7,0), data = resids, trace = FALSE)
archFit.ts = garchFit(~ arma(5,6)+ garch(7,0), data=pdcert[-1], trace = FALSE)

######## Apply GARCH Model #############################################
##divide into training and testing
## Predict July & August 
pdcert = pdcert[-1]
n=length(pdcert)
pdcert.test = pdcert[2643:n]
pdcert.train =  pdcert[-c(2643:n)]

## garchFit from the fGarch library
library(fGarch)
archFit.ts = garchFit(~ arma(5,6)+ garch(1,1), data=pdcert[-1], trace = FALSE)

##### Order Selection ################################################
## Find GARCH Order given ARMA order identified before
## ugrach from rugarch libary
library(rugarch)
final.bic = Inf
final.order = c(0,0)
for (p in 0:3) for (q in 0:3){
    spec = ugarchspec(variance.model=list(garchOrder=c(p,q)),
                 mean.model=list(armaOrder=c(5, 6), include.mean=T),
                 distribution.model="std")    
    fit = ugarchfit(spec, pdcert.train, solver = 'hybrid')
    current.bic = infocriteria(fit)[2] 
    if (current.bic < final.bic) {
     final.bic = current.bic
     final.order = c(p, q)
   }
 }
#> final.order
#[1] 1 2

## Refine the ARMA order
final.bic = Inf
final.order.arma = c(0,0)
for (p in 0:6) for (q in 0:6){
    spec = ugarchspec(variance.model=list(garchOrder=c(1,2)),
                 mean.model=list(armaOrder=c(p, q), include.mean=T),
                 distribution.model="std")    
    fit = ugarchfit(spec, pdcert.train, solver = 'hybrid')
    current.bic = infocriteria(fit)[2] 
    if (current.bic < final.bic) {
     final.bic = current.bic
     final.order.arma = c(p, q)
   }
 }
#> final.order.arma 
#[1] 0 0
 
## Refine the GARCH order
final.bic = Inf
final.order.garch = c(0,0)
for (p in 0:3) for (q in 0:3){
    spec = ugarchspec(variance.model=list(garchOrder=c(p,q)),
                 mean.model=list(armaOrder=c(final.order.arma[1], final.order.arma[2]), 
                 include.mean=T), distribution.model="std")    
    fit = ugarchfit(spec, pdcert.train, solver = 'hybrid')
    current.bic = infocriteria(fit)[2] 
    if (current.bic < final.bic) {
     final.bic = current.bic
     final.order.garch = c(p, q)
   }
 }
#> final.order.garch
#[1] 1 1

### Goodness of Fit ####################################################
spec.1 = ugarchspec(variance.model=list(garchOrder=c(1,2)),
                 mean.model=list(armaOrder=c(5, 6), 
                 include.mean=T), distribution.model="std")    
final.model.1 = ugarchfit(spec.1, pdcert.train, solver = 'hybrid')

spec.2 = ugarchspec(variance.model=list(garchOrder=c(1,2)),
                 mean.model=list(armaOrder=c(0, 0), 
                 include.mean=T), distribution.model="std")    
final.model.2 = ugarchfit(spec.2, pdcert.train, solver = 'hybrid')

spec.3 = ugarchspec(variance.model=list(garchOrder=c(1,1)),
                 mean.model=list(armaOrder=c(0, 0), 
                 include.mean=T), distribution.model="std")    
final.model.3 = ugarchfit(spec.3, pdcert.train, solver = 'hybrid')

## compare Information Criteria
infocriteria(final.model.1)
infocriteria(final.model.2)
infocriteria(final.model.3)

## Residual Analysis 
resids.final.model = residuals(final.model.3)
acf(resids.final.model,main="ACF of ARCH Residuals")
acf(resids.final.model^2,main="ACF of Squared ARCH Residuals")                    
Box.test(resids.final.model,lag=10,type='Ljung')
Box.test(resids.final.model^2,lag=10,type='Ljung')
qqnorm(resids.final.model)

### Prediction ##################################################

## 1. Prediction of the return time series
## 2. Prediction of the volatility 
nfore = length(pdcert.test)
fore.series.1 = NULL
fore.sigma.1 = NULL
fore.series.2 = NULL
fore.sigma.2 = NULL
fore.series.3 = NULL
fore.sigma.3 = NULL
for(f in 1: nfore){
    ## Fit models
    data = pdcert.train
    if(f>2)
       data = c(pdcert.train,pdcert.test[1:(f-1)])  
    final.model.1 = ugarchfit(spec.1, data, solver = 'hybrid')    
    final.model.2 = ugarchfit(spec.2, data, solver = 'hybrid')
    final.model.3 = ugarchfit(spec.3, data, solver = 'hybrid')
    ## Forecast
    fore = ugarchforecast(final.model.1, n.ahead=1)
    fore.series.1 = c(fore.series.1, fore@forecast$seriesFor)
    fore.sigma.1 = c(fore.sigma.1, fore@forecast$sigmaFor)
    fore = ugarchforecast(final.model.2, n.ahead=1)
    fore.series.2 = c(fore.series.2, fore@forecast$seriesFor)
    fore.sigma.2 = c(fore.sigma.2, fore@forecast$sigmaFor)
    fore = ugarchforecast(final.model.3, n.ahead=1)
    fore.series.3 = c(fore.series.3, fore@forecast$seriesFor)
    fore.sigma.3 = c(fore.sigma.3, fore@forecast$sigmaFor)
    }
 
 ## Compute Accuracy Measures 

### Mean Squared Prediction Error (MSPE)
mean((fore.series.1 - pdcert.test)^2)
mean((fore.series.2 - pdcert.test)^2)
mean((fore.series.3 - pdcert.test)^2)
### Mean Absolute Prediction Error (MAE)
mean(abs(fore.series.1 - pdcert.test))
mean(abs(fore.series.2 - pdcert.test))
mean(abs(fore.series.3 - pdcert.test))
### Mean Absolute Percentage Error (MAPE)
mean(abs(fore.series.1 - pdcert.test)/pdcert.test)
mean(abs(fore.series.2 - pdcert.test)/pdcert.test)
mean(abs(fore.series.3 - pdcert.test)/pdcert.test)
### Precision Measure (PM)
sum((fore.series.1 - pdcert.test)^2)/sum((pdcert.test-mean(pdcert.test))^2)
sum((fore.series.2 - pdcert.test)^2)/sum((pdcert.test-mean(pdcert.test))^2)
sum((fore.series.3 - pdcert.test)^2)/sum((pdcert.test-mean(pdcert.test))^2)   

ymin = min(c(as.vector(pdcert.test),fore.series.1,fore.series.2,fore.series.3))
ymax = max(c(as.vector(pdcert.test),fore.series.1,fore.series.2,fore.series.3))
data.plot = pdcert.test
names(data.plot)="Fore"
plot(pdcert[c(n-90):n],type="l", ylim=c(ymin,ymax), xlab="Time", ylab="Return Price")
data.plot$Fore=fore.series.1
points(data.plot,lwd= 2, col="blue")
data.plot$Fore=fore.series.2
points(data.plot,lwd= 2, col="brown")
data.plot$Fore=fore.series.3
points(data.plot,lwd= 2, col="purple")

ymin = min(c(as.vector(pdcert.test^2),fore.sigma.1^2,fore.sigma.2^2,fore.sigma.3^2))
ymax = max(c(as.vector(pdcert.test^2),fore.sigma.1^2,fore.sigma.2^2,fore.sigma.3^2))

plot(pdcert[c(n-90):n]^2,type="l", ylim=c(ymin,ymax), xlab="Time", ylab="Return Price")
data.plot$Fore=fore.sigma.1^2
points(data.plot,lwd= 2, col="blue")
data.plot$Fore=fore.sigma.2^2
points(data.plot,lwd= 2, col="brown")
data.plot$Fore=fore.sigma.3^2
points(data.plot,lwd= 2, col="purple")

