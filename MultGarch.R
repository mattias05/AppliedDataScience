library(rmgarch)
library(parallel)
library(quantmod)

Dat<-data.frame(GDAXI.DE[-c(1:22)], 
                GSPC,CRSOX,EEM)
Dat<-apply(Dat, 2, function(x) Delt( x, k=1, type="log"))

xspec = ugarchspec(mean.model = list(armaOrder = c(1, 1)), 
                   variance.model = list(garchOrder = c(1,1), model = 'sGARCH'), 
                   distribution.model = 'norm')
uspec = multispec(replicate(4, xspec))
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

fit_adcc = dccfit(spec1, 
                  data = Dat, 
                  fit.control = list(eval.se = TRUE))
                  #fit = multf, 
                  #cluster = cl)

print(fit1)           
print(fit_adcc)