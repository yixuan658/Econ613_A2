rm(list = ls())
#Exercise 1
library(bayesm)
data("margarine")
p <- margarine$choicePrice
d <- margarine$demos
hhid = matrix(p$hhid,ncol=1)
choice = matrix(p$choice,ncol=1)
ppks = matrix(p$PPk_Stk,ncol=1)
pbbs = matrix(p$PBB_Stk,ncol=1)
pfls = matrix(p$PFl_Stk,ncol=1)
phses = matrix(p$PHse_Stk,ncol=1)
pgens = matrix(p$PGen_Stk,ncol=1)
pimps = matrix(p$PImp_Stk,ncol=1)
psst = matrix(p$PSS_Tub,ncol=1)
ppkt = matrix(p$PPk_Tub,ncol=1)
pflt = matrix(p$PFl_Tub,ncol=1)
phset = matrix(p$PHse_Tub,ncol=1)

hhidd = matrix(d$hhid,ncol = 1)
income = matrix(d$Income,ncol = 1)
fs34 = matrix(d$Fs3_4,ncol = 1)
fs5 = matrix(d$Fs5.,ncol = 1)
fs = matrix(d$Fam_Size,ncol = 1)
college = matrix(d$college,ncol = 1)
wcollar = matrix(d$whtcollar,ncol = 1)
retire = matrix(d$retired,ncol = 1)
All = cbind(hhidd,income,fs34,fs5,fs,college,wcollar,retire)
colnames = c("hhid","income","fs34","fs5","fs","college","wcollar","retire")
#exercise1.1
#average and dispersion in product characteristic
ds_product <- basicStats(p[,3:ncol(p)])
ds_product <- ds_product[c("Mean","Median", "Stdev", "Variance"),]
#exercise 1.2
#Market share by product
dec = matrix(rep(0,4470*10),nrow = 4470,ncol = 10)
for(i in 1:4470){
  for(j in 1:10){
    if(choice[i] == j){
      dec[i,j] = 1
    }
  }
}
choiceprice = cbind(ppks,pbbs,pfls,phses,pgens,pimps,psst,ppkt,pflt,phset)
decprod = dec*choiceprice
share = matrix(apply(decprod,2,sum),nrow = 1)
colnames(share) = c("ppkstk","pbbstk","pflstk","phsestk","pgenstk","pimpstk","psstub","ppktub","pfltub","phsetub")
sum = sum(share)
mks_pro = share/sum #market share by product
View(mks_pro)
#market share by brand
decbrand = matrix(rep(0,4470*7),ncol = 7, nrow = 4470)
decbrand[,1] = decprod[,1] + decprod[,8]
decbrand[,2] = decprod[,2]
decbrand[,3] = decprod[,3] + decprod[,9]
decbrand[,4] = decprod[,4] + decprod[,10]
decbrand[,5] = decprod[,5]
decbrand[,6] = decprod[,6]
decbrand[,7] = decprod[,7]
share_brand = matrix(apply(decbrand,2,sum),nrow = 1)
all_brand = sum(share_brand)
mks_brand = share_brand/all_brand
colnames(mks_brand) = c("ppk","pbb","pfl","phse","pgen","pimp","pss")
View(mks_brand)
#market share by stk/tub
dectype = matrix(rep(0,4470*2),ncol = 2,nrow = 4470)
dectype[,1] = decprod[,1]+decprod[,2]+decprod[,3]+decprod[,4]+decprod[,5]+decprod[,6]
dectype[,2] = decprod[,7]+decprod[,8]+decprod[,9]+decprod[,10]
share_stk = matrix(apply(dectype,2,sum),nrow = 1)
all_stk = sum(share_stk)
mks_stk = share_stk/all_stk
colnames(mks_stk) = c("stk","tub")
View(mks_stk)
#exercise 1.3
#mapping
prod_all = cbind(hhid,choice,ppks,pbbs,pfls,phses,pgens,pimps,psst,ppkt,pflt,phset)
colnames(prod_all) = c("hhid","choice","ppks","pbbs","pfls","phses","pgens","pimps","psst","ppkt","pflt","phset")
ch1 = subset(prod_all,choice==1,select=c(hhid,ppks,choice))
ch2 = subset(prod_all,choice==2,select=c(hhid,pbbs,choice))
ch3 = subset(prod_all,choice==3,select=c(hhid,pfls,choice))
ch4 = subset(prod_all,choice==4,select=c(hhid,phses,choice))
ch5 = subset(prod_all,choice==5,select=c(hhid,pgens,choice))
ch6 = subset(prod_all,choice==6,select=c(hhid,pimps,choice))
ch7 = subset(prod_all,choice==7,select=c(hhid,psst,choice))
ch8 = subset(prod_all,choice==8,select=c(hhid,ppkt,choice))
ch9 = subset(prod_all,choice==9,select=c(hhid,pflt,choice))
ch10 = subset(prod_all,choice==10,select=c(hhid,phset,choice))
colnames(ch1) = c("hhid","product","choice")
colnames(ch2) = c("hhid","product","choice")
colnames(ch3) = c("hhid","product","choice")
colnames(ch4) = c("hhid","product","choice")
colnames(ch5) = c("hhid","product","choice")
colnames(ch6) = c("hhid","product","choice")
colnames(ch7) = c("hhid","product","choice")
colnames(ch8) = c("hhid","product","choice")
colnames(ch9) = c("hhid","product","choice")
colnames(ch10) = c("hhid","product","choice")
ch_all = rbind(ch1,ch2,ch3,ch4,ch5,ch6,ch7,ch8,ch9,ch10)
ex1.3 = matrix(cbind(hhidd,income),ncol = 2)
colnames(ex1.3) = c("hhid","income")
ex1.3 = merge(ch_all,ex1.3,by="hhid",all = T, sort = T)
colnames(ex1.3) = c("hhid","product","choice","income")
ex1.3 = ex1.3[,c(4,3)]
colnames(ex1.3) = c("income","choice")
sp = split(ex1.3,ex1.3[,c("income")],drop = TRUE)
mapping = as.data.frame(matrix(nrow = 0,ncol = 10))
colnames(mapping) = c("product1","product2","product3","product4","product5","product6","product7","product8","product9","product10")
for(i in 1:14){
  level = as.data.frame(sp[i])
  colnames(level) = c("income","choice")
  c = data.frame(matrix(nrow = 1,ncol = 0))
  for(j in 1:10){
    num = sum(level$choice==j)
    c = cbind.data.frame(c,num)
  }
  colnames(c) = c("product1","product2","product3","product4","product5","product6","product7","product8","product9","product10")
  mapping = rbind(mapping,c)
}
i = c("2.5","7.5","12.5","17.5","22.5","27.5","32.5","37.5","42.5","47.5","55","67.5","87.5","130")
mapping = cbind(i,map)
colnames(mapping) = c("income level","product1","product2","product3","product4","product5","product6","product7","product8","product9","product10")
View(mapping)

#Exercise 2
#Use multinomial model, Y is defined as 10 choices, X is defined as the price of each of the choices.
func <- function(x){
  x - ppks
}
df1 = as.matrix(cbind(ppks,pbbs,pfls,phses,pgens,pimps,psst,ppkt,pflt,phset),ncol=10)
df0 = apply(df1,2,func)
dec = matrix(rep(0,4470*10),nrow = 4470,ncol = 10)
for(i in 1:4470){
  for(j in 1:10){
    if(choice[i] == j){
      dec[i,j] = 1
    }
  }
}
likelihood <- function(theta){
  beta = as.numeric(theta[1])
  alpha = matrix(rep(theta[2:10],each = 4470),nrow = 4470,ncol = 9)
  z = c(rep(0,4470))
  alpha = matrix(cbind(z,alpha),nrow=4470,ncol=10)
  V <- df0*beta + alpha
  pro = matrix(c(rep(0,4470*10)),nrow = 4470,ncol = 10)
  lw = matrix(rep(0,4470),nrow = 4470, ncol = 1)
  V = exp(V)
  for(i in 1:4470){
    lw[i,1] = sum(V[i,])
  }
  for(i in 1:4470){
    for(j in 1:10)
      pro[i,j] = (V[i,j])/lw[i,1]
  }
  pro=log(pro)
  
  dec = dec * pro
  return(-sum(dec))
}
theta = c(-0.5,-0.5,-0.5,-0.5,-0.5,-0.5,-0.5,-0.5,-0.5,-0.5)
result = optim(par = theta,likelihood)$par
print(result)

#Exercise 3
m1 = matrix(cbind(hhid,choice),ncol = 2)
colnames(m1) = c("hhid","choice")
m2 = matrix(cbind(hhidd,income),ncol = 2)
colnames(m2) = c("hhid","income")
hhinc = as.data.frame(merge(m1,m2,by = "hhid",all = T,sort = T))
colnames(hhinc) = c("hhid","choice","income")
rincome = t(matrix(rep(t(hhinc$income),each = 10),nrow = 10,ncol = 4470))
dec = matrix(rep(0,4470*10),nrow = 4470,ncol = 10)
for(i in 1:4470){
  for(j in 1:10){
    if(choice[i] == j){
      dec[i,j] = 1
    }
  }
}
likelihood2 <- function(theta){
  z = matrix(rep(0,4470),ncol = 1)
  beta = matrix(rep(theta[1:9],each = 4470),nrow = 4470,ncol = 9)
  beta = matrix(cbind(z,beta),ncol = 10)
  alpha = matrix(rep(theta[10:18],each = 4470),nrow = 4470,ncol = 9)
  alpha = matrix(cbind(z,alpha),ncol = 10)
  V <- rincome*beta + alpha
  pro = matrix(c(rep(0,4470*10)),nrow = 4470,ncol = 10)
  lw = matrix(rep(0,4470),nrow = 4470, ncol = 1)
  
  V = exp(V)
  
  for(i in 1:4470){
    lw[i,1] = sum(V[i,])
  }
  for(i in 1:4470){
    for(j in 1:10)
      pro[i,j] = (V[i,j])/lw[i,1]
  }
  pro=log(pro)
  dec = dec * pro
  return(-sum(dec))
}
theta = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
#theta = c(-0.01,-0.5,0,0,0.5,0,0,0.01,0,-0.5,0,0,0,0,0,0,0,0)
#theta =  c(-0.51,-0.51,-0.51,-0.51,-0.51,-0.51,-0.51,-0.51,-0.51,-0.51)
result2 = optim(par = theta,likelihood2)$par
print(result2)

#Exercise 4
#Compute and interpret the marginal effect for the first and second models
#First model
be = result[1]
al = result[2:10]
z = c(rep(0,4470))
al = matrix(rep(result[2:10],each = 4470),nrow=4470,ncol=9)
al = cbind(z,al)
V4 <- df0 * be + al
pro4 = matrix(c(rep(0,4470*10)),nrow = 4470,ncol = 10)
lw4 = matrix(rep(0,4470),nrow = 4470, ncol = 1)
V4 = exp(V4)
for(i in 1:4470){
  lw4[i,1] = sum(V4[i,])
}
for(i in 1:4470){
  for(j in 1:10)
    pro4[i,j] = (V4[i,j])/lw4[i,1]
}

dec4 = matrix(rep(0,4470*10),nrow = 4470,ncol = 10)
for(i in 1:4470){
  for(j in 1:10){
    if(choice[i] == j){
      dec[i,j] = 1
    }
  }
}
S = matrix(rep(0,100),ncol = 10, nrow = 10)
A = diag(1,10,10)
for(i in 1:4470){
  Temp = matrix(rep(pro4[i,1:10],each = 10),ncol = 10,nrow = 10)
  S = S + t(Temp)*(A-Temp)*be
}
S = S/4470
colnames(S) = c("ME1","ME2","ME3","ME4","ME5","ME6","ME7","ME8","ME9","ME10")
View(S)

#Second Model
be2 = result2[1:9]
al2 = result2[10:18]
z = c(rep(0,4470))
be2 = matrix(rep(result2[1:9],each = 4470),nrow = 4470,ncol = 9)
al2 = matrix(rep(result2[10:18],each = 4470),nrow=4470,ncol=9)
al2 = cbind(z,al2)
be2 = cbind(z,be2)
V4_2 <- rincome*be2 + al2
pro4_2 = matrix(c(rep(0,4470*10)),nrow = 4470,ncol = 10)
lw4_2 = matrix(rep(0,4470),nrow = 4470, ncol = 1)
V4_2 = exp(V4_2)
for(i in 1:4470){
  lw4_2[i,1] = sum(V4_2[i,])
}
for(i in 1:4470){
  for(j in 1:10)
    pro4_2[i,j] = (V4_2[i,j])/lw4_2[i,1]
}
S_2 = matrix(rep(0,10),ncol = 10, nrow = 1)
be_2 = matrix(be2[1,],ncol = 10, nrow = 1)

for(i in 1:4470){
  Temp1 = matrix(pro4_2[i,1:10],ncol = 10,nrow = 1)
  betb = matrix(rep(Temp1 %*% t(be_2),10),nrow = 1, ncol = 10)
  S2 = S_2 + Temp1 * (be_2 - betb)
}
S2 = S2/4470
colnames(S2) = c("ME1*","ME2*","ME3*","ME4*","ME5*","ME6*","ME7*","ME8*","ME9*","ME10*")
View(S2)

#Exercise 5
#mixed logit model
dec = matrix(rep(0,4470*10),nrow = 4470,ncol = 10)
for(i in 1:4470){
  for(j in 1:10){
    if(choice[i] == j){
      dec[i,j] = 1
    }
  }
}
likelihood2 <- function(theta){
  beta = theta[1]
  alpha1 = matrix(rep(theta[2:10],each = 4470),nrow = 4470,ncol = 9)
  gama = matrix(rep(theta[11:19],each = 4770),nrow = 4470,ncol = 9)
  alpha2 = matrix(rep(theta[20:28],each = 4470),nrow = 4470,ncol = 9)
  z = c(rep(0,4470))
  alpha1 = matrix(cbind(z,alpha1),nrow=4470,ncol=10)
  alpha2 = matrix(cbind(z,alpha2),nrow = 4470,ncol = 10)
  gama = matrix(cbind(z,gama),nrow = 4470,ncol = 10)
  V_5 <- df0*beta + alpha1 + rincome*gama + alpha2
  V_5 = exp(V_5)
  pro_5 = matrix(c(rep(0,4470*10)),nrow = 4470,ncol = 10)
  lw_5 = matrix(rep(0,4470),nrow = 4470, ncol = 1)
  for(i in 1:4470){
    lw_5[i,1] = sum(V_5[i,])
  }
  for(i in 1:4470){
    for(j in 1:10)
      pro_5[i,j] = (V_5[i,j])/lw_5[i,1]
  }
  pro_5=log(pro_5)
  
  dec_5 = dec * pro_5
  return(-sum(dec_5))
}
theta = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
beta_all = optim(par = theta,likelihood2)$par
print(beta_all)

#remove choice 2
x = matrix(cbind(hhid,choice,ppks,pbbs,pfls,phses,pgens,pimps,psst,ppkt,pflt,phset),ncol = 12,nrow = 4470)
colnames(x) = c("hhid","choice","ppks","pbbs","pfls","phses","pgens","pimps","psst","ppkt","pflt","phset")
t2 = matrix(cbind(hhidd,income),ncol = 2)
colnames(t2) = c("hhid","income")
hx = as.matrix(merge(x,t2,by = "hhid",all = T,sort = T))
hx = matrix(cbind(hx,dec),nrow = 4470,ncol = 23)
colnames(hx) = c("hhid","choice","ppks","pbbs","pfls","phses","pgens","pimps","psst","ppkt","pflt","phset","income","d1","d2","d3","d4","d5","d6","d7","d8","d9","d10")

drophx = hx[!hx[,2] == 2,]
dropprod1 = subset(drophx,select=c(ppks,pfls,phses,pgens,pimps,psst,ppkt,pflt,phset))
dropinc1 = subset(drophx,select=c(income))
dropinc = t(matrix(rep(t(dropinc1),each = 9),ncol = 3771,nrow = 9))
dropdec = subset(drophx,select=c(d1,d3,d4,d5,d6,d7,d8,d9,d10))
func1 <- function(x){
  x - dropprod1[,1]
}
droppro = apply(dropprod1,2,func1)

likelihood3 <- function(theta){
  beta = theta[1]
  alpha1 = matrix(rep(theta[2:10],each = 3771),nrow = 3771,ncol = 9)
  gama = matrix(rep(theta[11:19],each = 3771),nrow = 3771,ncol = 9)
  alpha2 = matrix(rep(theta[20:27],each = 3771),nrow = 3771,ncol = 9)
  z = c(rep(0,3771))
  alpha1[,1] = z
  alpha2[,1] = z
  V_6 <- droppro * beta + alpha1 + dropinc*gama + alpha2
  V_6 = exp(V_6)
  pro_6 = matrix(c(rep(0,3771*9)),nrow = 3771,ncol = 9)
  lw_6 = matrix(rep(0,3771),nrow = 3771, ncol = 1)
  for(i in 1:3771){
    lw_6[i,1] = sum(V_6[i,])
  }
  for(i in 1:3771){
    for(j in 1:9)
      pro_6[i,j] = (V_6[i,j])/lw_6[i,1]
  }
  pro_6=log(pro_6)
  
  dec_6 = dropdec * pro_6
  return(-sum(dec_6))
}
theta = c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
beta_new = optim(par = theta,likelihood3)$par
print(beta_new)

#chi-sq test
MTT = -2 * (likelihood2(beta_all) - likelihood3(beta_new))
chi_2 = chisq.test(abs(beta_new))
print(chi_2)