#!/usr/bin/env Rscript

library("MTS")
VARMAmodelSelection <- function(x,prd){
	#ACF of data
	#tsx = ts(x)
	#acfx = acf(tsx)
	#acfx = acf(tsx,type="partial")

	amodel = VARMA(x,0,0)
	aparam = c(0,0)
	bmodel = amodel
	bparam = aparam
	bic = bmodel$bic
	aic = amodel$aic
	#Determine the order
	for(p in 0:5){
		for(q in 0:5){
			result = tryCatch({
				temp = VARMA(x,p,q)
				if(temp$bic < bic){
					bmodel = temp
					bic = bmodel$bic
					bparam = c(p,q)
				}
				if(temp$aic < aic){
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
	}
	ret = list()
	ret$aicModel = amodel
	ret$aicParam = aparam
	ret$bicModel = bmodel
	ret$bicParam = bparam
	return(ret)
}
x = read.csv("data/4-2_financial.csv",header=F,sep=",")
x = sapply(x[2:nrow(x), c(1,2)],as.character)
class(x) <- "numeric"
ret <- VARMAmodelSelection(x,4)
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
print(ret$aicParam) #(5,5)
print(ret$bicModel)
print(BIC(ret$bicModel))
print(ret$bicParam) #(2,0)
