library(astsa)
library(FitAR)
library(stats)

###Residual Normal Standar
powerMA1.Normal=function(n){
r=100
theta=0.3
z=matrix(0,n,r)
a=matrix(0,n,r)
est=numeric(r)
resid=matrix(0,n,r)
acf.res=matrix(0,n-1,r)
boxpirc.k1=numeric(r)
boxpirc.k2=numeric(r)
boxpirc.k3=numeric(r)
boxpirc.k4=numeric(r)
ljungbox.k1=numeric(r)
ljungbox.k2=numeric(r)
ljungbox.k3=numeric(r)
ljungbox.k4=numeric(r)
p.value.bpk1=numeric(r)
p.value.bpk2=numeric(r)
p.value.bpk3=numeric(r)
p.value.bpk4=numeric(r)
p.value.ljk1=numeric(r)
p.value.ljk2=numeric(r)
p.value.ljk3=numeric(r)
p.value.ljk4=numeric(r)
code.bp.50=numeric(r)
code.bp.10=numeric(r)
code.bp.5=numeric(r)
code.lj.50=numeric(r)
code.lj.10=numeric(r)
code.lj.5=numeric(r)

for (i in 1:r){
  a[,i]=rnorm(n,0,1)
  z[1,i]=a[1,i]
  for (t in 2:n) {
  z[t,i]=a[t,i]-theta*a[t-1,i]}
  }
for (i in 1:r){
  k1=12
  est[i]=list(arima(z[,i],order=c(0,0,1)))
  resid[,i]=est[[i]]$residuals
  acf.res[,i]=acf1(resid[,i],max.lag=n-1,plot=FALSE)
  for (j in 1:k1){
  boxpirc.k1[i]=n*sum(acf.res[j,i]^2)
  ljungbox.k1[i]=n*(n+2)*sum(((acf.res[j,i])^2)/(n-j))}
}
for (i in 1:r){
  k2=24
  est[i]=list(arima(z[,i],order=c(0,0,1)))
  resid[,i]=est[[i]]$residuals
  acf.res[,i]=acf1(resid[,i],max.lag=n-1,plot=FALSE)
  for (p in 1:k2){
  boxpirc.k2[i]=n*sum(acf.res[p,i]^2)
  ljungbox.k2[i]=n*(n+2)*sum(((acf.res[p,i])^2)/(n-p))}
}
for (i in 1:r){
  k3=36
  est[i]=list(arima(z[,i],order=c(0,0,1)))
  resid[,i]=est[[i]]$residuals
  acf.res[,i]=acf1(resid[,i],max.lag=n-1,plot=FALSE)
  for (v in 1:k3){
  boxpirc.k3[i]=n*sum(acf.res[v,i]^2)
  ljungbox.k3[i]=n*(n+2)*sum(((acf.res[v,i])^2)/(n-v))}
}
for (i in 1:r){
  k4=48
  est[i]=list(arima(z[,i],order=c(0,0,1)))
  resid[,i]=est[[i]]$residuals
  acf.res[,i]=acf1(resid[,i],max.lag=n-1,plot=FALSE)
  for (u in 1:k4){
  boxpirc.k4[i]=n*sum(acf.res[j,i]^2)
  ljungbox.k4[i]=n*(n+2)*sum(((acf.res[u,i])^2)/(n-u))}
}
for (i in 1:r){
p.value.bpk1[i]=1-pchisq(boxpirc.k1[i],k1-1)
p.value.bpk2[i]=1-pchisq(boxpirc.k2[i],k2-1)
p.value.bpk3[i]=1-pchisq(boxpirc.k3[i],k3-1)
p.value.bpk4[i]=1-pchisq(boxpirc.k4[i],k4-1)
p.value.ljk1[i]=1-pchisq(ljungbox.k1[i],k1-1)
p.value.ljk2[i]=1-pchisq(ljungbox.k2[i],k2-1)
p.value.ljk3[i]=1-pchisq(ljungbox.k3[i],k3-1)
p.value.ljk4[i]=1-pchisq(ljungbox.k4[i],k4-1)
if (p.value.bpk1[i]||p.value.bpk2[i]||p.value.bpk3[i]||p.value.bpk4[i]>=0.5) {
  code.bp.50[i]<-0}
  else{
  code.bp.50[i]<-1}
if (p.value.bpk1[i]||p.value.bpk2[i]||p.value.bpk3[i]||p.value.bpk4[i]>=0.1) {
  code.bp.10[i]<-0}
  else{
  code.bp.10[i]<-1}
if (p.value.bpk1[i]||p.value.bpk2[i]||p.value.bpk3[i]||p.value.bpk4[i]>=0.05) {
  code.bp.5[i]<-0}
  else{
  code.bp.5[i]<-1}
if (p.value.ljk1[i]||p.value.ljk2[i]||p.value.ljk3[i]||p.value.ljk4[i]>=0.5) {
  code.lj.50[i]<-0}
  else{
  code.lj.50[i]<-1}
if (p.value.ljk1[i]||p.value.ljk2[i]||p.value.ljk3[i]||p.value.ljk4[i]>=0.1) {
  code.lj.10[i]<-0}
  else{
  code.lj.10[i]<-1}
if (p.value.ljk1[i]||p.value.ljk2[i]||p.value.ljk3[i]||p.value.ljk4[i]>=0.05) {
  code.lj.5[i]<-0}
  else{
  code.lj.5[i]<-1}
frekcum.bp50=sum(code.bp.50)
frekcum.bp10=sum(code.bp.10)
frekcum.bp5=sum(code.bp.5)
frekcum.lj50=sum(code.lj.50)
frekcum.lj10=sum(code.lj.10)
frekcum.lj5=sum(code.lj.5)
BoxPierce=c(alpha50=frekcum.bp50,alpha10=frekcum.bp10,alpha5=frekcum.bp5)
LjungBox=c(alpha50=frekcum.lj50,alpha10=frekcum.lj10,alpha5=frekcum.lj5)
power=cbind(BoxPierce,LjungBox)
}
list(data=z,residual=resid,acf.resid=acf.res,power=power)
}
XX1=powerMA1.Normal(100)
XX2=powerMA1.Normal(500)

###Residual t(1)###
powerMA1.t1=function(n){
r=100
theta=0.3
z=matrix(0,n,r)
a=matrix(0,n,r)
est=numeric(r)
resid=matrix(0,n,r)
acf.res=matrix(0,n-1,r)
boxpirc.k1=numeric(r)
boxpirc.k2=numeric(r)
boxpirc.k3=numeric(r)
boxpirc.k4=numeric(r)
ljungbox.k1=numeric(r)
ljungbox.k2=numeric(r)
ljungbox.k3=numeric(r)
ljungbox.k4=numeric(r)
p.value.bpk1=numeric(r)
p.value.bpk2=numeric(r)
p.value.bpk3=numeric(r)
p.value.bpk4=numeric(r)
p.value.ljk1=numeric(r)
p.value.ljk2=numeric(r)
p.value.ljk3=numeric(r)
p.value.ljk4=numeric(r)
code.bp.50=numeric(r)
code.bp.10=numeric(r)
code.bp.5=numeric(r)
code.lj.50=numeric(r)
code.lj.10=numeric(r)
code.lj.5=numeric(r)
for (i in 1:r){
  a[,i]=rt(n,1)
  z[1,i]=a[1,i]
  for (t in 2:n) {
  z[t,i]=a[t,i]-theta*a[t-1,i]}
  }
for (i in 1:r){
  k1=12
  est[i]=list(arima(z[,i],order=c(0,0,1)))
  resid[,i]=est[[i]]$residuals
  acf.res[,i]=acf1(resid[,i],max.lag=n-1,plot=FALSE)
  for (j in 1:k1){
  boxpirc.k1[i]=n*sum(acf.res[j,i]^2)
  ljungbox.k1[i]=n*(n+2)*sum(((acf.res[j,i])^2)/(n-j))}
}
for (i in 1:r){
  k2=24
  est[i]=list(arima(z[,i],order=c(0,0,1)))
  resid[,i]=est[[i]]$residuals
  acf.res[,i]=acf1(resid[,i],max.lag=n-1,plot=FALSE)
  for (p in 1:k2){
  boxpirc.k2[i]=n*sum(acf.res[p,i]^2)
  ljungbox.k2[i]=n*(n+2)*sum(((acf.res[p,i])^2)/(n-p))}
}
for (i in 1:r){
  k3=36
  est[i]=list(arima(z[,i],order=c(0,0,1)))
  resid[,i]=est[[i]]$residuals
  acf.res[,i]=acf1(resid[,i],max.lag=n-1,plot=FALSE)
  for (v in 1:k3){
  boxpirc.k3[i]=n*sum(acf.res[v,i]^2)
  ljungbox.k3[i]=n*(n+2)*sum(((acf.res[v,i])^2)/(n-v))}
}
for (i in 1:r){
  k4=48
  est[i]=list(arima(z[,i],order=c(0,0,1)))
  resid[,i]=est[[i]]$residuals
  acf.res[,i]=acf1(resid[,i],max.lag=n-1,plot=FALSE)
  for (u in 1:k4){
  boxpirc.k4[i]=n*sum(acf.res[j,i]^2)
  ljungbox.k4[i]=n*(n+2)*sum(((acf.res[u,i])^2)/(n-u))}
}
for (i in 1:r){
p.value.bpk1[i]=1-pchisq(boxpirc.k1[i],k1-1)
p.value.bpk2[i]=1-pchisq(boxpirc.k2[i],k2-1)
p.value.bpk3[i]=1-pchisq(boxpirc.k3[i],k3-1)
p.value.bpk4[i]=1-pchisq(boxpirc.k4[i],k4-1)
p.value.ljk1[i]=1-pchisq(ljungbox.k1[i],k1-1)
p.value.ljk2[i]=1-pchisq(ljungbox.k2[i],k2-1)
p.value.ljk3[i]=1-pchisq(ljungbox.k3[i],k3-1)
p.value.ljk4[i]=1-pchisq(ljungbox.k4[i],k4-1)
if (p.value.bpk1[i]||p.value.bpk2[i]||p.value.bpk3[i]||p.value.bpk4[i]>=0.5) {
  code.bp.50[i]<-0}
  else{
  code.bp.50[i]<-1}
if (p.value.bpk1[i]||p.value.bpk2[i]||p.value.bpk3[i]||p.value.bpk4[i]>=0.1) {
  code.bp.10[i]<-0}
  else{
  code.bp.10[i]<-1}
if (p.value.bpk1[i]||p.value.bpk2[i]||p.value.bpk3[i]||p.value.bpk4[i]>=0.05) {
  code.bp.5[i]<-0}
  else{
  code.bp.5[i]<-1}
if (p.value.ljk1[i]||p.value.ljk2[i]||p.value.ljk3[i]||p.value.ljk4[i]>=0.5) {
  code.lj.50[i]<-0}
  else{
  code.lj.50[i]<-1}
if (p.value.ljk1[i]||p.value.ljk2[i]||p.value.ljk3[i]||p.value.ljk4[i]>=0.1) {
  code.lj.10[i]<-0}
  else{
  code.lj.10[i]<-1}
if (p.value.ljk1[i]||p.value.ljk2[i]||p.value.ljk3[i]||p.value.ljk4[i]>=0.05) {
  code.lj.5[i]<-0}
  else{
  code.lj.5[i]<-1}
frekcum.bp50=sum(code.bp.50)
frekcum.bp10=sum(code.bp.10)
frekcum.bp5=sum(code.bp.5)
frekcum.lj50=sum(code.lj.50)
frekcum.lj10=sum(code.lj.10)
frekcum.lj5=sum(code.lj.5)
BoxPierce=c(alpha50=frekcum.bp50,alpha10=frekcum.bp10,alpha5=frekcum.bp5)
LjungBox=c(alpha50=frekcum.lj50,alpha10=frekcum.lj10,alpha5=frekcum.lj5)
power=cbind(BoxPierce,LjungBox)
}
list(data=z,residual=resid,acf.resid=acf.res,power=power)
}
XT1=powerMA1.t1(100)
XT2=powerMA1.t1(500)
