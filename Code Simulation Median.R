#Q1
#simulate random sample with mean 13300 and st dev 3193.744 with 
n=5 #sample size
m=1000 #replication
x=matrix(0,n,m) #matrix of sample simulation with size 5 and replication 1000
med=rep(0,m) #matrix of median sample with 1000 replication
cmed=rep(0,m) 
for (i in 1:m){
  x[,i] <- rnorm(n,mean=13300,sd=3193.744) #simulate n=5 random sample
  med[i] <- median(x[,i])
  #calculate probability median between 10000 and 18000
  if (med[i]>=10000 & med[i]<=18000){
    cmed[i]=1
  }
  else {
    cmed[i]=0
  }
  prob=sum(cmed)/m #probability median between 10000 and 18000
}
print(prob) #show result of probability

#Q2
#input data
data = read.csv("E:\\data_Q2.csv")
x = sort(data$y)
xbar.x=mean(y)
sigma.x=sqrt(var(y))
#calculate probability distribution
hx = dnorm(x, mean=xbar.x, sd=sigma.x, log=FALSE)
#draw probability function plot
plot(x, hx, type="n", xlab="data", ylab="density",
     main="Probability Distribution")
lines(x, hx)
polygon(c(lb,x,ub), c(0,hx,0), col="snow2")

area <- pnorm(ub, xbar.x, sigma.x) - pnorm(lb, xbar.x, sigma.x)
result <- paste("P(",lb,"< IQ <",ub,") =",
                signif(area, digits=3))
mtext(result,3)
axis(1, at=seq(40, 160, 20), pos=0)


#Q5
#code R trailing zero in factorial
nzero = function(n){
  f1 = 1
  if (n<=1) {
    f1 = 1
  }
  else{
    for (i in 1:n){
      f1 =f1*i
    }
  }
  facto = f1
  count = 0
  j = 5
  while (facto%%j == 0) {
    count = count+1
    facto = facto/j
  }
  print(paste("the factorial of", n, "is", f1, "and the count end zeros is", count))
}
coba5=nzero(5)
coba6=nzero(6)
coba10=nzero(10)
coba100=nzero(100)
coba1000=nzero(1000)

#code count end zeros in a number
nn=function(n){
  count = 0
  j = 5
  while (n%%j == 0) {
    count = count+1
    n=n/5
  }
  print(count)
}

#code calculate factorial number
factt = function(n){
  f1 = 1
  if (n<=1) {
    f1 = 1
  }
  else{
    for (i in 1:n){
      f1 =f1*i
    }
  }
  factorial.is=f1
  print(factorial.is)
}
factt(1000)

#Based on Theorem if the number of factorial is very large
nzero.large=function(n){
  j=5
  endzero=rep(0,n)
  for (i in 1:n){
    if((j^i)<n){
      endzero[i]=floor(n/(5^i))
    }
    else{
      endzero[i]=0
    }
    nzze=sum(endzero)
  }
print(nzze)
}
nzero.large(100000)
