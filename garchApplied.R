# load the libraries
library(ks)
library(rugarch)
library(rmgarch)
library(parallel)
library(quantmod)

# read data
df = read.csv("data_renamed")

# set index and the column X 
rownames(df) <- df$X
df$X <- NULL

# consider only a subset of the dataset
df <- df[,c("ENEL","Volkswagen","UniCredit")]

# show the first rows
head(df)

spec1 = ugarchspec()

xspec = ugarchspec(mean.model = list(armaOrder = c(1, 1)), 
                   variance.model = list(garchOrder = c(1,1), model = 'sGARCH'), 
                   distribution.model = 'std')
#uspec = multispec(replicate(48, xspec))
uspec = multispec(replicate(3, xspec))
spec1 = dccspec(uspec = uspec, 
                dccOrder = c(1, 1), 
                distribution = 'mvt')
spec1a = dccspec(uspec = uspec, 
                 dccOrder = c(1, 1), 
                 model='aDCC', 
                 distribution = 'mvt')

#cl = makePSOCKcluster(4)
#multf = multifit(uspec, Dat, cluster = cl)

fit1 = dccfit(spec1, 
              data = df, 
              fit.control = list(eval.se = TRUE))
#fit = multf, 
#cluster = cl)

fit_adcc = dccfit(spec1a, 
                  data = df, 
                  fit.control = list(eval.se = TRUE))
#fit = multf, 
#cluster = cl)

print(fit1)           
print(fit_adcc)

likelihood(fit_adcc)
residuals(fit1)
plot(seq(from = 1, to = 17193, by = 1), residuals(fit1)[,1], type="l", lty=1, lwd=2)
plot(seq(from = 1, to = 17193, by = 1), residuals(fit1)[,2], type="l", lty=1, lwd=2)
plot(seq(from = 1, to = 17193, by = 1), residuals(fit1)[,3], type="l", lty=1, lwd=2)

plot(seq(from = 1, to = 17193, by = 1), sigma(fit1)[,1], type="l", lty=1, lwd=2)
plot(seq(from = 1, to = 17193, by = 1), sigma(fit1)[,2], type="l", lty=1, lwd=2)
plot(seq(from = 1, to = 17193, by = 1), sigma(fit1)[,3], type="l", lty=1, lwd=2)


'''
https://www.rdocumentation.org/packages/rugarch/versions/1.4-0/topics/ugarchspec-methods

https://rdrr.io/rforge/rgarch/man/dccspec-methods.html
dccspec(uspec, VAR = FALSE, VAR.opt = list(robust = FALSE, lag = 1, lag.max = NULL, 
                                           lag.criterion = c("AIC", "HQ", "SC", "FPE"), external.regressors = NULL, 
                                           robust.control = list("gamma" = 0.25, "delta" = 0.01, "nc" = 10, "ns" = 500)), 
        dccOrder = c(1,1), distribution = c("mvnorm", "mvt", "mvlaplace"),
        start.pars = list(), fixed.pars = list())
'''


