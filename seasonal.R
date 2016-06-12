#!/usr/bin/env Rscript

Problem2 <- function(x,prd){
	#ACF of data
	#tsx = ts(x)
	#acfx = acf(tsx)
	#acfx = acf(tsx,type="partial")

	#ACF after differenced
	#d = arima(x,order=c(0,1,0))
	#acf(d$residual,lag=30)
	#acf(d$residual,type="partial",lag=30)

	#ACF after seasonal differenced 1 at 4
	#d = arima(x,order=c(0,0,0),seasonal=list(order=c(0,1,0),period=4))
	#acf(d$residual,lag=30)

	#ACF of D1 and seasonal D1 at 4
	#d = arima(x,order=c(0,1,0),seasonal=list(order=c(0,1,0),period=12))
	#acf(d$residual,lag=30)

	#PACF of D1 and seasonal D1 at 4
	#acf(d$residual,type="partial",lag=30)

	amodel = arima(x,order=c(0,0,0),seasonal=list(order=c(0,0,0),period=prd))
	aparam = c(0,0,0,0,0,0,prd)
	bmodel = amodel
	bparam = aparam
	bic = BIC(bmodel)
	aic = amodel$aic
	#Determine the order
	for(p in 0:5){
		for(d in 0:3){
			for(q in 0:5){
				for(P in 0:5){
					for(D in 0:3){
						for(Q in 0:5){
							result = tryCatch({
								temp = arima(x,order=c(p,d,q),seasonal=list(order=c(P,D,Q),period=prd))
								if(BIC(temp) < bic){
									bmodel = temp
									bic = BIC(bmodel)
									bparam = c(p,d,q,P,D,Q,prd)
								}
								if(temp$aic < aic){
									amodel = temp
									aic = amodel$aic
									aparam = c(p,d,q,P,D,Q,prd)
								}
							},
								error = function(e) {
								message(e) 
								return(NULL)
							})
							print(c(p,d,q,P,D,Q,aic,bic))
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
x = read.csv("data/2-1_pe.csv",header=F,sep=",")
ret1 <- Problem2(x,4)
png(filename="pe_ACF_aic.png")
acf(ret1$aicModel$residual,lag=30)
dev.off()
png(filename="pe_PACF_aic.png")
acf(ret1$aicModel$residual,type="partial",lag=30)
dev.off()
png(filename="pe_ACF_bic.png")
acf(ret1$bicModel$residual,lag=30)
dev.off()
png(filename="pe_PACF_bic.png")
acf(ret1$bicModel$residual,type="partial",lag=30)
dev.off()

x = read.csv("data/2-2_rsp.csv",header=F,sep=",")
ret2 <- Problem2(x,12)
png(filename="rsp_ACF_aic.png")
acf(ret2$aicModel$residual,lag=30)
dev.off()
png(filename="rsp_PACF_aic.png")
acf(ret2$aicModel$residual,type="partial",lag=30)
dev.off()
png(filename="rsp_ACF_bic.png")
acf(ret2$bicModel$residual,lag=30)
dev.off()
png(filename="rsp_PACF_bic.png")
acf(ret2$bicModel$residual,type="partial",lag=30)
dev.off()
print("pe")
print(ret1$aicModel)
print(BIC(ret1$aicModel))
print(ret1$aicParam)
print(ret1$bicModel)
print(BIC(ret1$bicModel))
print(ret1$bicParam)
print("rsp")
print(ret2$aicModel)
print(BIC(ret2$aicModel))
print(ret2$aicParam)
print(ret2$bicModel)
print(BIC(ret2$bicModel))
print(ret2$bicParam)
