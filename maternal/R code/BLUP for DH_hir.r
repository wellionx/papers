library(lme4)
#setwd("C:~/phenotype ver3")

#表型的最终版本，对数据进行了清洗。包括极端值，还有环境间差异较大的
read.table(file="hir_DH_4env_ver3.csv",header=T,sep=",")->mydata

plot(mydata$HIR)
qqnorm(mydata$HIR)
boxplot(mydata$HIR)

qqnorm(mydata$EmbR)
boxplot(mydata$EmbR)
names(mydata)
geno<- as.factor(mydata$geno)
block<- as.factor(mydata$block)
location<- as.factor(mydata$location)
replication<- as.factor(mydata$rep)
HIR<-mydata$HIR
EMB<-mydata$EmbR
result1<-lmer(HIR~1+replication+block+(1|location)+(1|geno)+(1|location:geno))
result2<-lmer(HIR~1+(1|geno) + (1|location)+ 
               (1|replication : location) + 
               (1|block : replication : location) + 
               (1|geno : location))
emb_result<-lmer(EMB~1+replication+block+(1|location)+(1|geno)+(1|location:geno))
#residual
par(mfrow=c(1,3))
resid(result)
hist(resid(result),main = "残差的柱状图")
qqnorm(resid(result))
fitted(result)
plot(fitted(result),resid(result))
shapiro.test(resid(result))
# geno<-ranef(result)$geno
geno<-coef(result)$geno  #BLUP of HIR
boxplot(geno)
write.table(geno,"hir_DH_4env_blup_allver3.csv",sep=",")
#### anove model ####
##假设总体满足独立、正态，考察方差齐次性（用bartlett检验）
bartlett.test(HIR~location,data=mydata)
qqPlot(lm(HIR~location,data=mydata),simulate=T,main="Q-Q plot",labels=F)
re1 <- aov(HIR~location + geno + location:geno, mydata)
aggregate(HIR,by=list(location),FUN=mean)#各组均值
re2 <- aov(HIR~location)
summary(re1) 
library(gplots)
plotmeans(HIR~location,xlab="location",ylab="HIR",
          main="mean plot\nwith 95% CI")
##检测离群点
library(car)
outlierTest(re1)
summary(re1)
anova(re1)
#single environment Shunyi Jinan Hainan shunyi
mydata2<- subset(mydata,location=="Shunyi")

qqnorm(mydata2$HIR)
boxplot(mydata2$HIR)

geno<- as.factor(mydata2$geno)
block<- as.factor(mydata2$block)
#location<- as.factor(mydata$location)
replication<- as.factor(mydata2$rep)
HIR<-mydata2$HIR
result<-lmer(HIR~1+block+replication+(1|geno)+(1|replication:geno))
geno<-coef(result)$geno  #BLUP of HIR
#geno<-ranef(result)$geno
boxplot(geno)
write.table(geno,"hir_DH_4env3_blup_shunyi.csv",sep=",")

