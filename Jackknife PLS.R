library(readxl)
library(plspm)
data=read_excel("data sarif muda pasaribu.xlsx",sheet = "Sheet2")
data = data.frame(data)
data = data[,-1]
n = length(data)
data = as.matrix(data)
B=1000
coef.pls=matrix(c(rep(0,15*B)),B,15) # Ada 15 jalur
wj=matrix(c(rep(0,18*B)),18,B) # Ada 18 variabel 
loadj=matrix(c(rep(0,18*B)),18,B)
Rj=matrix(c(rep(0,B*6)),B,6) # Ada 6 variabel laten
for (k in 1:B){
  #jack resampling
  idjack1=c(1:n)
  set.seed(k)
  idjack=sample(idjack1,replace=F)
  n3=n-3
  data1=matrix(c(rep(0,n*18)),n,18)
  for (i in 1:n){
    for (j in 1:18){
      data1[i,j]=data[idjack[i],j]
    }}
  Y1=matrix(c(rep(0,n3)),n3,1)
  Y2=matrix(c(rep(0,n3)),n3,1)
  Y3=matrix(c(rep(0,n3)),n3,1)
  X11=matrix(c(rep(0,n3)),n3,1)
  X12=matrix(c(rep(0,n3)),n3,1)
  X13=matrix(c(rep(0,n3)),n3,1)
  X21=matrix(c(rep(0,n3)),n3,1)
  X22=matrix(c(rep(0,n3)),n3,1)
  X23=matrix(c(rep(0,n3)),n3,1)
  X31=matrix(c(rep(0,n3)),n3,1)
  X32=matrix(c(rep(0,n3)),n3,1)
  X33=matrix(c(rep(0,n3)),n3,1)
  X41=matrix(c(rep(0,n3)),n3,1)
  X42=matrix(c(rep(0,n3)),n3,1)
  X43=matrix(c(rep(0,n3)),n3,1)
  X51=matrix(c(rep(0,n3)),n3,1)
  X52=matrix(c(rep(0,n3)),n3,1)
  X53=matrix(c(rep(0,n3)),n3,1)
  
  for (i in 1:n3){
    Y1[i]=data1[i,1]
    Y2[i]=data1[i,2]
    Y3[i]=data1[i,3]
    X11[i]=data1[i,4]
    X12[i]=data1[i,5]
    X13[i]=data1[i,6]
    X21[i]=data1[i,7]
    X22[i]=data1[i,8]
    X23[i]=data1[i,9]
    X31[i]=data1[i,10]
    X32[i]=data1[i,11]
    X33[i]=data1[i,12]
    X41[i]=data1[i,13]
    X42[i]=data1[i,14]
    X43[i]=data1[i,15]
    X51[i]=data1[i,16]
    X52[i]=data1[i,17]
    X53[i]=data1[i,18]
    
    }
  X18=data.frame(Y1,Y2,Y3,X11,X12,X13,X21,X22,X23,X31,X32,X33,X41,X42,X43,X51,X52,X53)
  for(i in 1:ncol(X18)){
    X18[,i] <- as.numeric(X18[,i])
  }
  
  #bentuk model plspm
  X1=c(0,0,0,0,0,0)
  X2=c(1,0,0,0,0,0)
  X3=c(1,1,0,0,0,0)
  X4=c(1,1,1,0,0,0)
  X5=c(1,1,1,1,0,0)
  Y=c(1,1,1,1,1,0)
  model_path=rbind(X1,X2,X3,X4,X5,Y)
  colnames(model_path)=rownames(model_path)
  model_path
  innerplot(model_path)
  path_blocks=list(4:6,7:9,10:12,13:15,16:18,1:3)
  path_modes=rep("B", 6)
  path_pls=plspm(X18,model_path,path_blocks, modes=path_modes)
  b11= path_pls$path_coefs[2,1]
  b21= path_pls$path_coefs[3,1]
  b22= path_pls$path_coefs[3,2]
  b31= path_pls$path_coefs[4,1]
  b32= path_pls$path_coefs[4,2]
  b33= path_pls$path_coefs[4,3]
  b41= path_pls$path_coefs[5,1]
  b42= path_pls$path_coefs[5,2]
  b43= path_pls$path_coefs[5,3]
  b44= path_pls$path_coefs[5,4]
  b51= path_pls$path_coefs[6,1]
  b52= path_pls$path_coefs[6,2]
  b53= path_pls$path_coefs[6,3]
  b54= path_pls$path_coefs[6,4]
  b55= path_pls$path_coefs[6,5]
  b=cbind(b11,b21,b21,b31,b32,b33,b41,b42,b43,b44,b51,b52,b53,b54,b55)
  w=path_pls$outer_model[,3]
  l=path_pls$outer_model[,4]
  R=path_pls$inner_summary[,2]
  for (ii in 1:15){
    coef.pls[k,ii]=b[ii]
  }
  for (iii in 1:18){
    wj[iii,k]=w[iii]
    loadj[iii,k]=l[iii]
  }
  for (iv in 1:6){
    Rj[k,iv]=R[iv]
  }
}

writexl::write_xlsx(path_pls$inner_summary,"Inner Summary.xlsx")

#uji signifikansi weight
w1=wj[1,]
w2=wj[2,]
w3=wj[3,]
w4=wj[4,]
w5=wj[5,]
w6=wj[6,]
w7=wj[7,]
w8=wj[8,]
w9=wj[9,]
w10=wj[10,]
w11=wj[11,]
w12=wj[12,]
w13=wj[13,]
w14=wj[14,]
w15=wj[15,]
w16=wj[16,]
w17=wj[17,]
w18=wj[18,]
for (i in 1:B)
{
  w1[i]=wj[1,i]
  w2[i]=wj[2,i]
  w3[i]=wj[3,i]
  w4[i]=wj[4,i]
  w5[i]=wj[5,i]
  w6[i]=wj[6,i]
  w7[i]=wj[7,i]
  w8[i]=wj[8,i]
  w9[i]=wj[9,i]
  w10[i]=wj[10,i]
  w11[i]=wj[11,i]
  w12[i]=wj[12,i]
  w13[i]=wj[13,i]
  w14[i]=wj[14,i]
  w15[i]=wj[15,i]
  w16[i]=wj[16,i]
  w17[i]=wj[17,i]
  w18[i]=wj[18,i]
}
w1.mean=mean(w1)
w1.sd=sd(w1)
w1.se=w1.sd/sqrt(B)
thit.w1=w1.mean/w1.se
Pvalue.w1=dt(thit.w1,999)

w2.mean=mean(w2)
w2.sd=sd(w2)
w2.se=w2.sd/sqrt(B)
thit.w2=w2.mean/w2.se
Pvalue.w2=dt(thit.w2,999)

w3.mean=mean(w3)
w3.sd=sd(w3)
w3.se=w3.sd/sqrt(B)
thit.w3=w3.mean/w3.se
Pvalue.w3=dt(thit.w3,999)

w4.mean=mean(w4)
w4.sd=sd(w4)
w4.se=w4.sd/sqrt(B)
thit.w4=w4.mean/w4.se
Pvalue.w4=dt(thit.w4,999)

w5.mean=mean(w5)
w5.sd=sd(w5)
w5.se=w5.sd/sqrt(B)
thit.w5=w5.mean/w5.se
Pvalue.w5=dt(thit.w5,999)

w6.mean=mean(w6)
w6.sd=sd(w6)
w6.se=w6.sd/sqrt(B)
thit.w6=w6.mean/w6.se
Pvalue.w6=dt(thit.w6,999)

w7.mean=mean(w7)
w7.sd=sd(w7)
w7.se=w7.sd/sqrt(B)
thit.w7=w7.mean/w7.se
Pvalue.w7=dt(thit.w7,999)

w8.mean=mean(w8)
w8.sd=sd(w8)
w8.se=w8.sd/sqrt(B)
thit.w8=w8.mean/w8.se
Pvalue.w8=dt(thit.w8,999)

w9.mean=mean(w9)
w9.sd=sd(w9)
w9.se=w9.sd/sqrt(B)
thit.w9=w9.mean/w9.se
Pvalue.w9=dt(thit.w9,999)

w10.mean=mean(w10)
w10.sd=sd(w10)
w10.se=w10.sd/sqrt(B)
thit.w10=w10.mean/w10.se
Pvalue.w10=dt(thit.w10,999)

w11.mean=mean(w11)
w11.sd=sd(w11)
w11.se=w11.sd/sqrt(B)
thit.w11=w11.mean/w11.se
Pvalue.w11=dt(thit.w11,999)

w12.mean=mean(w12)
w12.sd=sd(w12)
w12.se=w12.sd/sqrt(B)
thit.w12=w12.mean/w12.se
Pvalue.w12=dt(thit.w12,999)

w13.mean=mean(w13)
w13.sd=sd(w13)
w13.se=w13.sd/sqrt(B)
thit.w13=w13.mean/w13.se
Pvalue.w13=dt(thit.w13,999)

w14.mean=mean(w14)
w14.sd=sd(w14)
w14.se=w14.sd/sqrt(B)
thit.w14=w14.mean/w14.se
Pvalue.w14=dt(thit.w14,999)

w15.mean=mean(w15)
w15.sd=sd(w15)
w15.se=w15.sd/sqrt(B)
thit.w15=w15.mean/w15.se
Pvalue.w15=dt(thit.w15,999)

w16.mean=mean(w16)
w16.sd=sd(w16)
w16.se=w16.sd/sqrt(B)
thit.w16=w16.mean/w16.se
Pvalue.w16=dt(thit.w16,999)

w17.mean=mean(w17)
w17.sd=sd(w17)
w17.se=w17.sd/sqrt(B)
thit.w17=w17.mean/w17.se
Pvalue.w17=dt(thit.w17,999)

w18.mean=mean(w18)
w18.sd=sd(w18)
w18.se=w18.sd/sqrt(B)
thit.w18=w18.mean/w18.se
Pvalue.w18=dt(thit.w18,999)

Parameter=matrix(c("w1","w2","w3","w4","w5","w6","w7",
                   "w8","w9","w10","w11","w12","w13","w14","w15","w16","w17","w18"),18,1)
weight=matrix(c(w1.mean,w2.mean,w3.mean,w4.mean,w5.mean,w6.mean,w7.mean,w8.mean,w9.mean,w10.mean,w11.mean,w12.mean,w13.mean,w14.mean,w15.mean,w16.mean,w17.mean,w18.mean),18,1)
SE=matrix(c(w1.se,w2.se,w3.se,w4.se,w5.se,w6.se,w7.se,w8.se,w9.se,w10.se,w11.se,w12.se,w13.se,w14.se,w15.se,w16.se,w17.se,w18.se),18,1)
Thit=matrix(c(thit.w1,thit.w2,thit.w3,thit.w4,thit.w5,thit.w6,thit.w7,thit.w8,thit.w9,thit.w10,thit.w11,thit.w12,thit.w13,thit.w14,thit.w15,thit.w16,thit.w17,thit.w18),18,1)
Pvalue=matrix(c(Pvalue.w1,Pvalue.w2,Pvalue.w3,Pvalue.w4,Pvalue.w5,Pvalue.w6,Pvalue.w7,Pvalue.w8,Pvalue.w9,Pvalue.w10,Pvalue.w11,Pvalue.w12,Pvalue.w13,Pvalue.w14,Pvalue.w15,Pvalue.w16,Pvalue.w17,Pvalue.w18),18,1)
Hasilweight.Jackknife=data.frame(Parameter,weight,SE,Thit,Pvalue)
Hasilweight.Jackknife
writexl::write_xlsx(Hasilweight.Jackknife,"Hasil Weight Jackknife.xlsx")

#uji signifikansi Loadingfactor
load1=loadj[1,]
load2=loadj[2,]
load3=loadj[3,]
load4=loadj[4,]
load5=loadj[5,]
load6=loadj[6,]
load7=loadj[7,]
load8=loadj[8,]
load9=loadj[9,]
load10=loadj[10,]
load11=loadj[11,]
load12=loadj[12,]
load13=loadj[13,]
load14=loadj[14,]
load15=loadj[15,]
load16=loadj[16,]
load17=loadj[17,]
load18=loadj[18,]
for (i in 1:B)
{
  load1[i]=loadj[1,i]
  load2[i]=loadj[2,i]
  load3[i]=loadj[3,i]
  load4[i]=loadj[4,i]
  load5[i]=loadj[5,i]
  load6[i]=loadj[6,i]
  load7[i]=loadj[7,i]
  load8[i]=loadj[8,i]
  load9[i]=loadj[9,i]
  load10[i]=loadj[10,i]
  load11[i]=loadj[11,i]
  load12[i]=loadj[12,i]
  load13[i]=loadj[13,i]
  load14[i]=loadj[14,i]
  load15[i]=loadj[15,i]
  load16[i]=loadj[16,i]
  load17[i]=loadj[17,i]
  load18[i]=loadj[18,i]
}

load1.mean=mean(load1)
load1.sd=sd(load1)
load1.se=load1.sd/sqrt(B)
thit.load1=load1.mean/load1.se
Pvalue.load1=dt(thit.load1,999)

load2.mean=mean(load2)
load2.sd=sd(load2)
load2.se=load2.sd/sqrt(B)
thit.load2=load2.mean/load2.se
Pvalue.load2=dt(thit.load2,999)

load3.mean=mean(load3)
load3.sd=sd(load3)
load3.se=load3.sd/sqrt(B)
thit.load3=load3.mean/load3.se
Pvalue.load3=dt(thit.load3,999)

load4.mean=mean(load4)
load4.sd=sd(load4)
load4.se=load4.sd/sqrt(B)
thit.load4=load4.mean/load4.se
Pvalue.load4=dt(thit.load4,999)

load5.mean=mean(load5)
load5.sd=sd(load5)
load5.se=load5.sd/sqrt(B)
thit.load5=load5.mean/load5.se
Pvalue.load5=dt(thit.load5,999)

load6.mean=mean(load6)
load6.sd=sd(load6)
load6.se=load6.sd/sqrt(B)
thit.load6=load6.mean/load6.se
Pvalue.load6=dt(thit.load6,999)

load7.mean=mean(load7)
load7.sd=sd(load7)
load7.se=load7.sd/sqrt(B)
thit.load7=load7.mean/load7.se
Pvalue.load7=dt(thit.load7,999)

load8.mean=mean(load8)
load8.sd=sd(load8)
load8.se=load8.sd/sqrt(B)
thit.load8=load8.mean/load8.se
Pvalue.load8=dt(thit.load8,999)

load9.mean=mean(load9)
load9.sd=sd(load9)
load9.se=load9.sd/sqrt(B)
thit.load9=load9.mean/load9.se
Pvalue.load9=dt(thit.load9,999)

load10.mean=mean(load10)
load10.sd=sd(load10)
load10.se=load10.sd/sqrt(B)
thit.load10=load10.mean/load10.se
Pvalue.load10=dt(thit.load10,999)

load11.mean=mean(load11)
load11.sd=sd(load11)
load11.se=load11.sd/sqrt(B)
thit.load11=load11.mean/load11.se
Pvalue.load11=dt(thit.load11,999)

load12.mean=mean(load12)
load12.sd=sd(load12)
load12.se=load12.sd/sqrt(B)
thit.load12=load12.mean/load12.se
Pvalue.load12=dt(thit.load12,999)

load13.mean=mean(load13)
load13.sd=sd(load13)
load13.se=load13.sd/sqrt(B)
thit.load13=load13.mean/load13.se
Pvalue.load13=dt(thit.load13,999)

load14.mean=mean(load14)
load14.sd=sd(load14)
load14.se=load14.sd/sqrt(B)
thit.load14=load14.mean/load14.se
Pvalue.load14=dt(thit.load14,999)

load15.mean=mean(load15)
load15.sd=sd(load15)
load15.se=load15.sd/sqrt(B)
thit.load15=load15.mean/load15.se
Pvalue.load15=dt(thit.load15,999)

load16.mean=mean(load16)
load16.sd=sd(load16)
load16.se=load16.sd/sqrt(B)
thit.load16=load16.mean/load16.se
Pvalue.load16=dt(thit.load16,999)

load17.mean=mean(load17)
load17.sd=sd(load17)
load17.se=load17.sd/sqrt(B)
thit.load17=load17.mean/load17.se
Pvalue.load17=dt(thit.load17,999)

load18.mean=mean(load18)
load18.sd=sd(load18)
load18.se=load18.sd/sqrt(B)
thit.load18=load18.mean/load18.se
Pvalue.load18=dt(thit.load18,999)

Parameter=matrix(c("load1","load2","load3","load4","load5","load6","load7","load8","load9","load10","load11","load12","load13","load14","load15","load16","load17","load18"),18,1)
loading_factor=matrix(c(load1.mean,load2.mean,load3.mean,load4.mean,load5.mean,load6.mean,load7.mean,load8.mean,load9.mean,load10.mean,load11.mean,load12.mean,load13.mean,load14.mean,load15.mean,load16.mean,load17.mean,load18.mean),18,1)
SE=matrix(c(load1.se,load2.se,load3.se,load4.se,load5.se,load6.se,load7.se,load8.se,load9.se,load10.se,load11.se,load12.se,load13.se,load14.se,load15.se,load16.se,load17.se,load18.se),18,1)
Thit=matrix(c(thit.load1,thit.load2,thit.load3,thit.load4,thit.load5,thit.load6,thit.load7,thit.load8,thit.load9,thit.load10,thit.load11,thit.load12,thit.load13,thit.load14,thit.load15,thit.load16,thit.load17,thit.load18),18,1)
Pvalue=matrix(c(Pvalue.load1,Pvalue.load2,Pvalue.load3,Pvalue.load4,Pvalue.load5,Pvalue.load6,Pvalue.load7,Pvalue.load8,Pvalue.load9,Pvalue.load10,Pvalue.load11,Pvalue.load12,Pvalue.load13,Pvalue.load14,Pvalue.load15,Pvalue.load16,Pvalue.load17,Pvalue.load18),18,1)
Hasilload.Jackknife=data.frame(Parameter,loading_factor,SE,Thit,Pvalue)
Hasilload.Jackknife
writexl::write_xlsx(Hasilload.Jackknife,"Hasil Load Jackknife.xlsx")

#uji signifikansii koefisien
b11= path_pls$path_coefs[2,1]
b21= path_pls$path_coefs[3,1]
b22= path_pls$path_coefs[3,2]
b31= path_pls$path_coefs[4,1]
b32= path_pls$path_coefs[4,2]
b33= path_pls$path_coefs[4,3]
b41= path_pls$path_coefs[5,1]
b42= path_pls$path_coefs[5,2]
b43= path_pls$path_coefs[5,3]
b44= path_pls$path_coefs[5,4]
b51= path_pls$path_coefs[6,1]
b52= path_pls$path_coefs[6,2]
b53= path_pls$path_coefs[6,3]
b54= path_pls$path_coefs[6,4]
b55= path_pls$path_coefs[6,5]
for (i in 1:B)
{
  b11[i]=coef.pls[i,1]
  b21[i]=coef.pls[i,2]
  b22[i]=coef.pls[i,3]
  b31[i]=coef.pls[i,4]
  b32[i]=coef.pls[i,5]
  b33[i]=coef.pls[i,6]
  b41[i]=coef.pls[i,7]
  b42[i]=coef.pls[i,8]
  b43[i]=coef.pls[i,9]
  b44[i]=coef.pls[i,10]
  b51[i]=coef.pls[i,11]
  b52[i]=coef.pls[i,12]
  b53[i]=coef.pls[i,13]
  b54[i]=coef.pls[i,14]
  b55[i]=coef.pls[i,15]
}

b11.mean=mean(b11)
b11.sd=sd(b11)
b11.se=b11.sd/sqrt(B)
thit.b11=b11.mean/b11.se
Pvalue.b11=dt(thit.b11,999)

b21.mean=mean(b21)
b21.sd=sd(b21)
b21.se=b21.sd/sqrt(B)
thit.b21=b21.mean/b21.se
Pvalue.b21=dt(thit.b21,999)

b22.mean=mean(b22)
b22.sd=sd(b22)
b22.se=b22.sd/sqrt(B)
thit.b22=b22.mean/b22.se
Pvalue.b22=dt(thit.b22,999)

b31.mean=mean(b31)
b31.sd=sd(b31)
b31.se=b31.sd/sqrt(B)
thit.b31=b31.mean/b31.se
Pvalue.b31=dt(thit.b31,999)

b32.mean=mean(b32)
b32.sd=sd(b32)
b32.se=b32.sd/sqrt(B)
thit.b32=b32.mean/b32.se
Pvalue.b32=dt(thit.b32,999)

b33.mean=mean(b33)
b33.sd=sd(b33)
b33.se=b33.sd/sqrt(B)
thit.b33=b33.mean/b33.se
Pvalue.b33=dt(thit.b33,999)

b41.mean=mean(b41)
b41.sd=sd(b41)
b41.se=b41.sd/sqrt(B)
thit.b41=b41.mean/b41.se
Pvalue.b41=dt(thit.b41,999)

b42.mean=mean(b42)
b42.sd=sd(b42)
b42.se=b42.sd/sqrt(B)
thit.b42=b42.mean/b42.se
Pvalue.b42=dt(thit.b42,999)

b43.mean=mean(b43)
b43.sd=sd(b43)
b43.se=b43.sd/sqrt(B)
thit.b43=b43.mean/b43.se
Pvalue.b43=dt(thit.b43,999)

b44.mean=mean(b44)
b44.sd=sd(b44)
b44.se=b44.sd/sqrt(B)
thit.b44=b44.mean/b44.se
Pvalue.b44=dt(thit.b44,999)

b51.mean=mean(b51)
b51.sd=sd(b51)
b51.se=b51.sd/sqrt(B)
thit.b51=b51.mean/b51.se
Pvalue.b51=dt(thit.b51,999)

b52.mean=mean(b52)
b52.sd=sd(b52)
b52.se=b52.sd/sqrt(B)
thit.b52=b52.mean/b52.se
Pvalue.b52=dt(thit.b52,999)

b53.mean=mean(b53)
b53.sd=sd(b53)
b53.se=b53.sd/sqrt(B)
thit.b53=b53.mean/b53.se
Pvalue.b53=dt(thit.b53,999)

b54.mean=mean(b54)
b54.sd=sd(b54)
b54.se=b54.sd/sqrt(B)
thit.b54=b54.mean/b54.se
Pvalue.b54=dt(thit.b54,999)

b55.mean=mean(b55)
b55.sd=sd(b55)
b55.se=b55.sd/sqrt(B)
thit.b55=b55.mean/b55.se
Pvalue.b55=dt(thit.b55,999)

Parameter=matrix(c("Beta1","Beta2","Beta3","Beta4","Beta5","Beta6","Beta7","Beta8","Beta9","Beta10","Beta11","Beta12","Beta13","Beta14","Beta15"),15,1)
coefisien=matrix(c(b11.mean,b21.mean,b22.mean,b31.mean,b32.mean,b33.mean,b41.mean,b42.mean,b43.mean,b44.mean,b51.mean,b52.mean,b53.mean,b54.mean,b55.mean),15,1)
SE=matrix(c(b11.se,b21.se,b22.se,b31.se,b32.se,b33.se,b41.se,b42.se,b43.se,b44.se,b51.se,b52.se,b53.se,b54.se,b55.se),15,1)
Thit=matrix(c(thit.b11,thit.b21,thit.b22,thit.b31,thit.b32,thit.b33,thit.b41,thit.b42,thit.b43,thit.b44,thit.b51,thit.b52,thit.b53,thit.b54,thit.b55),15,1)
Pvalue=matrix(c(Pvalue.b11,Pvalue.b21,Pvalue.b22,Pvalue.b31,Pvalue.b32,Pvalue.b33,Pvalue.b41,Pvalue.b42,Pvalue.b43,Pvalue.b44,Pvalue.b51,Pvalue.b52,Pvalue.b53,Pvalue.b54,Pvalue.b55),15,1)
Hasilcoef.Jackknife=data.frame(Parameter,coefisien,SE,Thit,Pvalue)
Hasilcoef.Jackknife
writexl::write_xlsx(Hasilcoef.Jackknife,"Hasil Coef Jackknife.xlsx")

