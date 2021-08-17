library(nortest)

#power test of normality test by montecarlo simulation
#Normality test: Kolmogorov-Smirnov (KS), Shapiro-Wilk (SW), and Anderson-Darling (AD)

test_norm.u=function(n,r,bb,ba){
dat.u=matrix(0,n,r)
hx=0
hh=0
jx=0
kx=0
lx=0
ks=numeric(r)
sw=numeric(r)
ad=numeric(r)
koding.ks=numeric(r)
koding.sw=numeric(r)
koding.ad=numeric(r)
for (h in 1:r){
	dat.u[,h] = runif(n,bb,ba)
	}
dat.u[hx]=dat.u
for (i in 1:r){
	hasil1[i]=list(ks.test(dat.u[,i],”punif”,bb,ba))
    	ks[i]=hasil1[[i]]$p.value
	hasil2[i]=list(shapiro.test(dat.u[,i]))
	sw[i]=hasil2[[i]]$p.value
	hasil3[i]=list(ad.test(dat.u[,i]))
	ad[i]=hasil3[[i]]$p.value
	}
ks[hh]=ks
sw[hh]=sw
ad[hh]=ad
for (j in 1:r) {
  if (ks[j] <= 0.05) {
    koding.ks[j] <- 1}
  else {
    koding.ks[j] <- 0}
}
koding.ks[jx] <- koding.ks
percent.ks <- (sum(koding.ks)/r)*100
for (k in 1:r) {
  if (sw[k] <= 0.05) {
    koding.sw[k] <- 1}
  else {
    koding.sw[k] <- 0}
}
koding.sw[kx] <- koding.sw
percent.sw <- (sum(koding.sw)/r)*100
for (l in 1:r) {
  if (ad[l] <= 0.05) {
    koding.ad[l] <- 1}
  else {
    koding.ad[l] <- 0}
}
koding.ad[lx] <- koding.ad
percent.ad <- (sum(koding.ad)/r)*100
list(PercentageKS=percent.ks,PercentageSW=percent.sw,PercentageAD=percent.ad)
hasil=rbind(percent.ks,percent.sw,percent.ad)
print(hasil)
}
normalitypower1=test_norm.u(10,100,2,3)
