library(astsa)
library(FitAR)
library(stats)
powerAR1=function(n,phi,r,k){
z=matrix(0,n,r)
a=matrix(0,n,r)
est=numeric(r)
resid=matrix(0,n,r)
acf.res=matrix(0,n-1,r)
ljungbox=numeric(r)
p.value=numeric(r)
code=numeric(r)
code.p=numeric(r)
for (i in 1:r){
  a[,i]=rnorm(n,0,1)
  z[1,i]=a[1,i]
  for (t in 2:n) {
  z[t,i]=phi*z[t-1,i]+a[t,i]}
  }
for (i in 1:r){
  est[i]=list(arima(z[,i],order=c(1,0,0)))
  resid[,i]=est[[i]]$residuals
  acf.res[,i]=acf1(resid[,i],max.lag=n-1,plot=FALSE)
  for (j in 1:k){
  ljungbox[i]=n*(n+2)*sum(((acf.res[j,i])^2)/(n-j))}
chisqr=qchisq(0.05,k-2)
p.value[i]=1-pchisq(ljungbox[i],k-2)
if (ljungbox[i] <= chisqr) {
    code[i] <- 1}
  else {
    code[i] <- 0}
if (p.value[i]>= 0.05) {
  code.p[i]<-1}
  else{
  code.p[i]<-0}
percentage.st=(sum(code)/r)*100
percentage.p=(sum(code.p)/r)*100
}
list(data=z,acf.resid=acf.res, residual=resid, hasil=cbind(ljungbox,p.value,code,code.p),power=c(percent.lj=percentage.st,percent.p=percentage.p))
}
coba1=powerAR1(20,0.3,5,9)