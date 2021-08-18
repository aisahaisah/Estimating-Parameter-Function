
#Function of simulation random samples and resampling by grouped bootstrap
#Function of simulation random samples
simulasi = function(psi,n) {
  N = n+100
  at = rnorm(N,0,1)
  yt = matrix(nrow=N,ncol=1)
  yt[1]=at[1]
  for (i in 2:N) {
    yt[i]=psi*yt[i-1]+at[i]
  }
  yt1=yt[101:N]
par(mfrow=c(3,1))
ts.plot(yt1)
acf(yt1)
pacf(yt1)
list(yt1=yt1)
}

#Function of grouped/divided sample to prepare for bootstrap/resampling
bagi = function(m,s){
  blok=matrix(nrow=m,ncol=s)
  data = simulasi(0.3,300)
  n =length(data)
  for (i in 1:n){
  for (j in 1:m){
    i=j
    a = s*i-(s-1)
    b = s*i
    blok[j,]=data$yt1[a:b]
  }
  }
  list(blok=blok)
}
w=bagi(60,5)
