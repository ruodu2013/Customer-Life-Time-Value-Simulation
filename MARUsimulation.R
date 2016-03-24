
load("C:/Users/Ruo/Downloads/customers.rdata") 

hist(customers,breaks=20)

library("fitdistrplus")
plotdist(customers, histo = TRUE, demp = TRUE)
descdist(customers, boot = 1000)


library(sn)
am<-rsn(n=1000, xi=29949.88, omega=4798.066, alpha=--0.1656298 , tau=0,  dp=NULL)
hist(am)


#rr follow left skewed distribution
rr1 <- rsn(n=100000, xi=0, omega=1, alpha=-2, tau=0,  dp=NULL)
hist(rr1)
#xi:mean, omega:sd,alpha:skew
min.rr1 = min(rr1)
max.rr1 = max(rr1)
rr1.skewnorm = (rr1 - min.rr1)/(max.rr1 - min.rr1)
rr1.mean = ( mean (rr1) - min.rr1)/(max.rr1 - min.rr1)
hist(rr1.skewnorm,breaks=20)


CLV1=am*(1+0.1)/(1+0.1-rr1.skewnorm)-50000
hist(CLV1,breaks=200)
hist(CLV1,breaks=200,xlim=c(-30000,100000))

NewCLV1<-sort(CLV1, decreasing = TRUE)
totalCLV=0
for (i in 1:100000){
    totalCLV=NewCLV1[i]+totalCLV

    if (totalCLV>sum(CLV1)/2){
        break()
    }

    
}
i/100000
sum(CLV1)/2
   

 
   


#rr follow normal distribution with low sd
rr2=rnorm(100000,0.6,0.01)
hist(rr2,xlim=c(0,1))
min(rr2)
max(rr2)
CLV2=am*(1+0.1)/(1+0.1-rr2)-50000
hist(CLV2,breaks=200)
hist(CLV2,breaks=200,xlim=c(-30000,100000))


NewCLV2<-sort(CLV2, decreasing = TRUE)
totalCLV=0
for (i in 1:100000){
    totalCLV=NewCLV2[i]+totalCLV
    
    if (totalCLV>sum(CLV2)/2){
        break()
    }
    
    
}
i/100000



#rr follow normal distribution with high sd
rr3=rnorm(100000,0.6,0.1)
hist(rr3,breaks=30)
min(rr3)
max(rr3)
CLV3=am*(1+0.1)/(1+0.1-rr3)-50000
hist(CLV3,breaks=200)
hist(CLV3,breaks=200,xlim=c(-30000,100000))



NewCLV3<-sort(CLV3, decreasing = TRUE)
totalCLV=0
for (i in 1:100000){
    totalCLV=NewCLV3[i]+totalCLV
    
    if (totalCLV>sum(CLV3)/2){
        break()
    }
    
    
}
i/100000


#rr follow UNIFORM distribution

rr4=runif(100000, min = 0.2, max = 1)
hist(rr4,breaks=30)
mean(rr4)
CLV4=am*(1+0.1)/(1+0.1-rr4)-50000
hist(CLV4,breaks=200)
hist(CLV4,breaks=200,xlim=c(-30000,200000))


NewCLV4<-sort(CLV4, decreasing = TRUE)
totalCLV=0
for (i in 1:100000){
    totalCLV=NewCLV4[i]+totalCLV
    
    if (totalCLV>sum(CLV4)/2){
        break()
    }
    
    
}
i/100000


#simulation for little leaguers
rr0=rnorm(100000,0.75,0.1)
hist(rr0,breaks=50)
am0<-rsn(n=100000, xi=5000, omega=500, alpha=--0.1656298 , tau=0,  dp=NULL)
#hist(am0)
CLV0=5000*(1+0.1)/(1+0.1-rr0)-10000
hist(CLV0,breaks=8000,xlim=c(-30000,200000))


NewCLV0<-sort(CLV0, decreasing = TRUE)
totalCLV=0
for (i in 1:100000){
    totalCLV=NewCLV0[i]+totalCLV
    
    if (totalCLV>sum(CLV0)/2){
        break()
    }
    
    
}
i/100000