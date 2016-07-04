#!/usr/bin/env Rscript

library("MTS")
library("gtools")
library("forecast")
library("MASS")
VARMAmodelSelection <- function(x,diff){
	#ACF of data
	#tsx = ts(x)
	#acfx = acf(tsx)
	#acfx = acf(tsx,type="partial")
	xx = x
	for(times in 1:diff)
		xx = xx[2:nrow(xx),] - xx[1:nrow(xx)-1,]
	amodel = VARMA(xx,0,0)
	aparam = c(0,0)
	bmodel = amodel
	bparam = aparam
	bic = bmodel$bic
	aic = amodel$aic
	#Determine the order
	for(p in 0:5){
		for(q in 0:5){
			result = tryCatch({
				temp = VARMA(xx,p,q)
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
	}
	ret = list()
	ret$aicModel = amodel
	ret$aicParam = aparam
	ret$bicModel = bmodel
	ret$bicParam = bparam
	return(ret)
}
sVARMAmodelSelection <- function(x,s){
	amodel <- sVARMA(x,c(0,0,0),c(0,0,0),s)
	aparam = c(c(0,0,0),c(0,0,0))
	bmodel = amodel
	bparam = aparam
	bic = bmodel$bic
	aic = amodel$aic
	for(p in 0:5){
		for(d in 0:5){
			for(q in 0:5){
				for(P in 0:5){
					for(D in 0:5){
						for(Q in 0:5){
							result = tryCatch({
								temp = sVARMA(x,c(p,d,q),c(P,D,Q),s)
								if(temp$bic < bic){
									print("update bic")
									bmodel = temp
									bic = bmodel$bic
									bparam = c(c(p,d,q),c(P,D,Q))
								}
								if(temp$aic < aic){
									print("update aic")
									amodel = temp
									aic = amodel$aic
									aparam = c(c(p,d,q),c(P,D,Q))
								}
							},
								error = function(e) {
								message(e) 
								return(NULL)
							})
							print(c(p,d,q,P,D,Q,s,aic,bic))
						}
					}
				}
			}
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
lambda <- BoxCox.lambda(x)
# xx <- BoxCox(x,lambda)
# model <- sVARMA(xx,c(2,0,1),c(1,2,1),24)
# acf(model$residual,lag=72)
# acf(model$residual,type="partial",lag=72)

ret <- sVARMAmodelSelection(BoxCox(x,lambda),24)
png(filename="financial_ACF_aic.png")
acf(ret$aicModel$residual,lag=64)
dev.off()
png(filename="financial_PACF_aic.png")
acf(ret$aicModel$residual,type="partial",lag=64)
dev.off()
png(filename="financial_ACF_bic.png")
acf(ret$bicModel$residual,lag=64)
dev.off()
png(filename="financial_PACF_bic.png")
acf(ret$bicModel$residual,type="partial",lag=64)
dev.off()

print(ret$aicModel)
print(BIC(ret$aicModel))
print(ret$aicParam) #(5,5), d1(4,4), d2(5,5)
print(ret$bicModel)
print(BIC(ret$bicModel))
print(ret$bicParam) #(2,0), d1(2,1), d2(0,4)
