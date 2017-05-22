library("poppr")
library("pegas")
library("ape")
library("adegenet")
library("ade4")
require('mgcv')
### project of tunyu
genotype <- read.csv("genotyping1.csv",header = T)
genotype2 <- genotype[,-c(1,62:66)]
rownames(genotype2) <- genotype[,1]
geno <- as.data.frame(t(genotype2))
locus <- geno
ind <- rownames(geno)
### chagne the name
#ind2 <- read.table(text = ind, sep = ".", as.is = TRUE)$V1
ind3 <- substr(ind,2,3)
# rename the line
ind2 <- read.csv("check list.csv",header = T)
ind4 <- as.character(ind2$line)

colnames(locus) <- paste("SNP",1:191280)
Mydata1 <- df2genind(locus, ploidy = 2, ind.names = ind4,  sep="")
Mydist <- provesti.dist(Mydata1)

#save the results of distance
out <- as.matrix(Mydist)
write.csv(out,"distance from genotype.csv")

require(mgcv)
save(Mydist,file = "mydist.rda")

# Neighbor joining tree ####
set.seed(999)
theTree <- Mydist %>% 
           nj() %>%    # calculate neighbor-joining tree
           ladderize() # organize branches by clade
# plot the tree 
par( mar=c(2, 4, 2, 2) ) # margin size
plot(theTree,cex=0.6)
add.scale.bar(length = 0.05)

# PCA ####
library(ggplot2)
library(ggfortify)
# cmdscale(Mydist, k = 1, eig = TRUE, add = FALSE, x.ret = FALSE) #$GOF=0.1455807
# cmdscale(Mydist, k = 2, eig = TRUE, add = FALSE, x.ret = FALSE) #$GOF=0.2464832
ty.pca <- cmdscale(Mydist, k = 3, eig = TRUE, add = FALSE, x.ret = FALSE)
autoplot(ty.pca,label = T, data=dta_geno, label.size = 3,
         xlab="PC1(14.5%)",ylab="PC2(10.1%)",shape=F)+
  theme_light()+
  scale_color_grey()

# pca another ####
PCoA1 <- ty.pca$points
colnames(PCoA1) <- c("PC1","PC2","PC3")
PCoA1 <- as.data.frame(PCoA1)
plot(x=PCoA1$PC1,y=PCoA1$PC2)

plot(x=PCoA1$PC1,y=PCoA1$PC2,
     xlab="PC1(14.5%)",ylab="PC2(10.1%)")
