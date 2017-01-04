library(qtl)

mhir<-read.cross("csvsr",phefile="lwdh_pheno.csv",genfile="lwdh_geno.csv",na.strings =c("-","NA"),estimate.map = FALSE,genotypes=c("C","O"),alleles=c("C","O")) ## loading the data for R/qtl mapping
class(mhir)[1] <- "dh"
summary(mhir)
jittermap(mhir)
plot(mhir)
plot.missing(mhir)
plot.map(mhir)
plot.pheno(mhir, 2)
plot.pheno(mhir, 3)
plot.pheno(mhir, 4)
plot.pheno(mhir, 5)

#### Data Checking #####
# Phenotype
library(qtl)
library(qtlbook)
par(mfrow=c(3,2))
for(i in 2:6)
plot.pheno(mhir, pheno.col=i)

##### Counting crossovers ####
nxo <- countXO(mhir)
nxo[nxo>40]
plot(nxo, ylab="No. crossovers")
mean(nxo[3:139])
mean(nxo[-(3:139)])
countXO(mhir, bychr=TRUE)["GLW76",]

##### Simple interval mapping –plotting one trait ####
out.pheno2.em <-scanone (mhir, pheno.col=c("Shunyi"),method="em")
plot(out.pheno2.em,lodcolumn = c(1),lty=1,col=c("blue"))
legend("topleft",lty=1,lwd=2,legend = c("Shunyi"),col=c("blue"))
dev.off()
out.pheno3.em <-scanone (mhir, pheno.col=c("Jinan"),method="em")
plot(out.pheno3.em,lodcolumn = c(1),lty=1,col=c("blue"))
legend("topleft",lty=1,lwd=2,legend = c("Jinan"),col=c("blue"))
dev.off()
out.pheno4.em <-scanone (mhir, pheno.col=c("SJZ"),method="em")
plot(out.pheno4.em,lodcolumn = c(1),lty=1,col=c("blue"))
legend("topleft",lty=1,lwd=2,legend = c("SJZ"),col=c("blue"))
dev.off()
out.pheno5.em <-scanone (mhir, pheno.col=c(5),method="em")
plot(out.pheno5.em,lodcolumn = c(1),lty=1,col=c("blue"))
legend("topleft",lty=1,lwd=2,legend = c("Hainan"),col=c("blue"))
out.pheno6.em <-scanone (mhir, pheno.col=c(6),method="em")
plot(out.pheno6.em,lodcolumn = c(1),lty=1,col=c("black"))
legend("topleft",lty=1,lwd=2,legend = c("combine"),col=c("black"))
dev.off()

## Simple interval mapping –plotting one more trait
out.pheno_all.em <-scanone (mhir, pheno.col=c(2,3,4,5,6),method="em")

#lodcolumn can take a vector of up to three values
plot(out.pheno_all.em,lodcolumn = c(4,5),
     lty=1,col=c("#E41A1C", "#377EB8"))
plot(out.pheno_all.em,lodcolumn = c(1,2,3),
     lty=1,col=c("#4DAF4A", "#984EA3", "#FF7F00"),
     ylab="LOD score",add=TRUE)
legend("topleft",lty=1,lwd=2,
       legend = c("Shunyi","Jinan","SJZ","Hainan","combine"),
       col=c("#4DAF4A", "#984EA3", "#FF7F00","#E41A1C", "#377EB8"),bty = "n")
abline(h=2.5)
####zooming in on a particular chromosome ####
plot(out.pheno_all.em,lodcolumn = c(4,5),
     lty=1,col=c("#E41A1C", "#377EB8"),chr = c("9"))
plot(out.pheno_all.em,lodcolumn = c(1,2,3),
     lty=1,col=c("#4DAF4A", "#984EA3", "#FF7F00"),chr = c("9"),
     ylab="LOD score",add=TRUE)
legend("topright",lty=1,lwd=2,
       legend = c("Shunyi","Jinan","SJZ","Hainan","combine"),
       col=c("#4DAF4A", "#984EA3", "#FF7F00","#E41A1C", "#377EB8"),bty = "n")
#### CIM scan ####
out.shunyi_cim.em<-cim(mhir,pheno.col=c("Shunyi"),window=10,n.marcovar=5,method="em")
out.jinan_cim.em<-cim(mhir,pheno.col=c("Jinan"),window=10,n.marcovar=5,method="em")
out.sjz_cim.em<-cim(mhir,pheno.col=c("SJZ"),window=10,n.marcovar=5,method="em")
out.hainan_cim.em<-cim(mhir,pheno.col=c("Hainan"),window=10,n.marcovar=5,method="em")
out.combine_cim.em<-cim(mhir,pheno.col=c("combine"),window=10,n.marcovar=5,method="em")

plot(out.hainan_cim.em,out.combine_cim.em,ylab="LOD",
     col=c("#E41A1C", "#377EB8"),chr = c("9"))
plot(out.shunyi_cim.em,out.jinan_cim.em,
     out.sjz_cim.em,ylab="LOD",
     col=c("#4DAF4A", "#984EA3", "#FF7F00",
           "#E41A1C", "#377EB8"),chr = c("9"),add = TRUE)
legend("topright",lty=1,lwd=2,
       legend = c("Shunyi","Jinan","SJZ","Hainan","combine"),
       col=c("#4DAF4A", "#984EA3", "#FF7F00","#E41A1C", "#377EB8"),bty = "n")
### QTL interval
bayesint(out.shunyi_cim.em,chr=9,0.95)
## colors uese in the plot ####
library(RColorBrewer)
display.brewer.all(type = "qual")
brewer.pal(5,"Set1")
display.brewer.pal(5,"Set1")
## cutoff permutation
operm <- scanone(mhir, n.perm=1000, pheno.col=c("Shunyi","Jinan","SJZ","Hainan"),method="hk")
summary(operm, alpha=c(0.01, 0.05))

##### Identifying genotyping errors #####
# newmap <- est.map(mhir, error.prob=0.01)
# mhir <- replace.map(mhir, newmap)
# mhir <- calc.errorlod(mhir)

# top <- top.errorlod(mhir, cutoff=5)

require(mgcv)
load(file = "top_error_geno.rda")
top
plot.geno(mhir, 1, top$id[top$chr==1], cutoff=5)
################################### cim scan for all phenotye
source("cim_scan.r")
cim.scan(pheno.col="Shunyi",perm.cutoff=2.5,add.abline=T,int.m="bayesint")
