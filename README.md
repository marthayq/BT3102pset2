# BT3102pset2
N<-1000
location<-runif(N, min = 0, max = 1)
sf<-sample(500:3000, N, replace = T)
bedroom<-sample(0:4, N, replace = T)
hist(location)
hist(sf)
hist(bedroom)
###Q1
price<-1200 * sf + 10^6 * bedroom + rnorm(N)
house<-cbind.data.frame(price, sf, bedroom, location)
plot(location, price)
fit<-price~sf + bedroom
lm1<-lm(price~sf + bedroom)
summary(lm1)
###Q2
CBDsqd<-(location - 0.5) ^2
price2<-1200 * sf + 10^6 * bedroom - 8 * 10^7 * CBDsqd + rnorm(N)
price2<-ifelse(price2 < 1, 1, price2)
plot(location, price2)
OLSreg<-lm(price2~bedroom + sf + CBDsqd)
summary(OLSreg)

###Q3
library(spdep)
neighbor<-matrix(nrow = N, ncol = N)
for (i in 1:N){
  for(j in 1:N){
    x=abs(location[i] - location[j])
    neighbor[i,j] = ifelse(i!=j, x, 0)
  }
}
listw<-mat2listw(neighbor, style = "M")
summary(listw)
moran.test(price2, alternative = "two.sided", listw = listw)
moran.plot(price2, alternative = "two.sided", listw = listw)

#######################################
morani<-function(attr,w){
  N=length(w[1,])
  mean=mean(attr)
  Num = 0
  #calculate the numerator of I
  for (i in 1:N){
    for (j in 1:N){
      Num = Num + w[i,j]*(attr[i]-mean)*(attr[j]-mean)}}
  Num = N*Num
  #calculate the denominator of I
  aVar = 0
  for (i in 1:N){aVar = aVar+(attr[i]-mean)^2}
  Weight = sum(w)
  Den = Weight*aVar
  I = Num/Den
  #calculate the Expected I
  Exp = -1/(N-1)
  #calculate variance with S1, S2, S3, S4 ans S5
  S1=1/2*sum((2*w)^2)
  S2=0
  for (i in 1:N){
    sum1 = sum(w[i,])
    sum2 = sum(w[,i])
    S2 = S2 + (sum1+sum2)^2 }
  S3num = 0
  for (i in 1:N){S3num = S3num+((attr[i]-mean)^4)}
  S3den = 0
  for (i in 1:N){S3den = S3den+((attr[i]-mean)^2)}
  S3 = (N*S3num)/(S3den)^2
  S4 = (N^2-3*N+3)*S1-N*S2+3*(sum(w)^2)
  S5 = (N^2-N)*S1-2*N*S2+6*(sum(w)^2)
  Var = (N*S4-S3*S5)/((N-1)*(N-2)*(N-3)*(sum(w)^2))-(Exp)^2
  #calculate the confidence interval at 5% significance level
  Lower_I = Exp-1.96*sqrt(Var)
  Upper_I = Exp+1.96*sqrt(Var)
  print(cbind(I,Exp, Var, Lower_I, Upper_I))}
morani(price2, neighbor)
##########################################################
###Q4
for (i in 1:N){
  for(j in 1:N){
    x=abs(location[i] - location[j])
    neighbor[i,j] = ifelse(x<=0.1 && i!=j, 1, 0)
  }
}
mydata<-cbind.data.frame(price2, bedroom, sf, CBDsqd)
spatial.lag<-lagsarlm(price2~sf + bedroom + CBDsqd, data = mydata, listw)
