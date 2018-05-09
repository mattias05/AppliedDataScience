
library(ks)
library(rugarch)
library(rmgarch)
library(parallel)
library(quantmod)

#load data, time series closing prices, 10 year sample
#DAX 30
getSymbols('^GDAXI', src='yahoo', return.class='ts',from="2005-01-01",    to="2015-01-31")
GDAXI.DE=GDAXI[ , "GDAXI.Close"]
#S&P 500
getSymbols('^GSPC', src='yahoo', return.class='ts',from="2005-01-01", to="2015-01-31")
GSPC=GSPC[ , "GSPC.Close"]
#Credit Suisse Commodity Return Strat I
getSymbols('CRSOX', src='yahoo', return.class='ts',from="2005-01-01", to="2015-01-31")
CRSOX=CRSOX[ , "CRSOX.Close"]
#iShares MSCI Emerging Markets
getSymbols('EEM', src='yahoo', return.class='ts',from="2005-01-01", to="2015-01-31")
EEM=EEM[ , "EEM.Close"]

Dat<-data.frame(GSPC,CRSOX,EEM)
Dat<-apply(Dat, 2, function(x) Delt( x, k=1, type="log"))
Dat = Dat[-1,]

spec1 = ugarchspec()

xspec = ugarchspec(mean.model = list(armaOrder = c(1, 1)), 
                   variance.model = list(garchOrder = c(1,1), model = 'sGARCH'), 
                   distribution.model = 'norm')
uspec = multispec(replicate(3, xspec))
spec1 = dccspec(uspec = uspec, 
                dccOrder = c(1, 1), 
                distribution = 'mvnorm')
spec1a = dccspec(uspec = uspec, 
                 dccOrder = c(1, 1), 
                 model='aDCC', 
                 distribution = 'mvnorm')

#cl = makePSOCKcluster(4)
#multf = multifit(uspec, Dat, cluster = cl)

fit1 = dccfit(spec1, 
              data = Dat, 
              fit.control = list(eval.se = TRUE))
              #fit = multf, 
              #cluster = cl)

fit_adcc = dccfit(spec1a, 
                  data = Dat, 
                  fit.control = list(eval.se = TRUE))
                  #fit = multf, 
                  #cluster = cl)

print(fit1)           
print(fit_adcc)
