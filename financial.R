#!/usr/bin/env Rscript

library("MTS")
library(gtools)
library("forecast")
VARMAmodelSelection <- function(x,diff){
	#ACF of data
	#tsx = ts(x)
	#acfx = acf(tsx)
	#acfx = acf(tsx,type="partial")
	xx = x
	times = 1
	while (times <= diff) {
		xx = xx[2:nrow(xx),] - xx[1:nrow(xx)-1,]
		times = times + 1
		print(times)
	}
	lambda1 = BoxCox.lambda(xx[,1])
	lambda3 = BoxCox.lambda(xx[,2])
	yy1 = BoxCox(xx[,1], lambda1)
	yy3 = BoxCox(xx[,2], lambda3)
	yy = cbind(yy1, yy3)
	amodel = VARMA(xx,0,0)
	aparam = c(0,0)
	bmodel = amodel
	bparam = aparam
	bic = bmodel$bic
	aic = amodel$aic
	#Determine the order
	params_set = 0:5
	params = permutations(n=6, r=2,v=params_set,repeats.allowed=T)
	for (idx in 2:nrow(params)) {
	  p = params[idx,1]
	  q = params[idx,2]
		result = tryCatch({
			temp = VARMA(yy,p,q)
			if(temp$bic < bic){
				print("update bic")
				bmodel = temp
				bic = bmodel$bic
				bparam = c(p,q)
			}
			if(temp$aic < aic){
				print("update aic")
				amodel = temp
				aic = amodel$aic
				aparam = c(p,q)
			}
		},
			error = function(e) {
			message(e) 
			return(NULL)
		})
		print(c(p,q,aic,bic))
	}
	ret = list()
	ret$aicModel = amodel
	ret$aicParam = aparam
	ret$bicModel = bmodel
	ret$bicParam = bparam
	ret$lambda = c(lambda1, lambda3)
	return(ret)
}
x = read.csv("data/4-2_financial.csv",header=F,sep=",")
x = sapply(x[2:nrow(x), c(1,2)],as.character)
class(x) <- "numeric"

ptm <- proc.time()
ret <- VARMAmodelSelection(x,0)
proc.time() - ptm

png(filename="financial_ACF_aic.png")
acf(ret$aicModel$residual,lag=30)
dev.off()
png(filename="financial_PACF_aic.png")
acf(ret$aicModel$residual,type="partial",lag=30)
dev.off()
png(filename="financial_ACF_bic.png")
acf(ret$bicModel$residual,lag=30)
dev.off()
png(filename="financial_PACF_bic.png")
acf(ret$bicModel$residual,type="partial",lag=30)
dev.off()

print(ret$aicModel)
print(BIC(ret$aicModel))
print(ret$aicParam) #(5,5), d1(4,4), d2(5,5)
print(ret$bicModel)
print(BIC(ret$bicModel))
print(ret$bicParam) #(2,0), d1(2,1), d2(0,4)