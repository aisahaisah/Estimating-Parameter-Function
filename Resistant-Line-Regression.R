data<-read.csv(file.choose(),header=T) #choose dataresistant.csv
data
dataurut<-data[order(data$x),]
dataurut
      a = 1
      b = 14
      c = 15
      d = 27
      e = 28
      f = 40
      databawah=dataurut[a:b,]
      xbawah=databawah$x
      ybawah=databawah$y
      datatengah=dataurut[c:d,]
      xtengah=datatengah$x
      ytengah=datatengah$y
      dataatas=dataurut[e:f,]
      xatas=dataatas$x
      yatas=dataatas$y
medx.bawah=median(databawah$x,na.rm=FALSE)
medx.tengah=median(datatengah$x,na.rm=FALSE)
medx.atas=median(dataatas$x,na.rm=FALSE)
medy.bawah=median(databawah$y,na.rm=FALSE)
medy.tengah=median(datatengah$y,na.rm=FALSE)
medy.atas=median(dataatas$y,na.rm=FALSE)
b1=(medy.atas-medy.bawah)/(medx.atas-medx.bawah)
a1=(1/3)*((medy.bawah+medx.tengah+medy.atas)-(b1*(medx.bawah+medx.tengah+medx.atas)))
estimasi=cbind(a1,b1)
rasio=1
while (rasio>0.01){
  yhat1bawah=a1+b1*medx.bawah
  yhat1tengah=a1+b1*medx.tengah
  yhat1atas=a1+b1*medx.atas
  e1bawah=yhat1bawah-ybawah
  e1tengah=yhat1tengah-ytengah
  e1atas=yhat1atas-yatas
  b1.aksen=(e1atas-e1bawah)/(medx.atas-medx.bawah)
  b2=b1+b1.aksen
  a2=(1/3)*((medy.bawah+medx.tengah+medy.atas)-(b2*(medx.bawah+medx.tengah+medx.atas)))
  yhat2bawah=a1+b2*medx.bawah
  yhat2tengah=a1+b2*medx.tengah
  yhat2atas=a1+b2*medx.atas
  e2bawah=yhat2bawah-ybawah
  e2tengah=yhat2tengah-ytengah
  e2atas=yhat2atas-yatas
  b2.aksen=(e2atas-e2bawah)/(medx.atas-medx.bawah)
  b3=(b2-b2.aksen)*((b2-b1)/(b2.aksen-b1.aksen))
  a3=(1/3)*((medy.bawah+medx.tengah+medy.atas)-(b3*(medx.bawah+medx.tengah+medx.atas)))
  rasio=b2.aksen/b1
  if (b2.aksen/b1<0.01){
    b2=b3
    a2=a3
  }
  else{
    a2=a1+((1/3)*(e2bawah+e2tengah+e2atas))
  }
  estimasi=cbind(a2,b2)
  print(estimasi)
}
