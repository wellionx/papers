#genetic distance calculation
#convert the SNP data file into genind objects
dta <- read.csv("Data_Geno_QC_trans.csv")
dim(dta)

library("poppr")
library("pegas")
library("ape")
library("adegenet")
library("ade4")

locus <- dta[, -1] 
ind <- dta$ind
Mydata1 <- df2genind(locus, ploidy = 2, ind.names = ind,  sep="")
Mydata1
#Mydata2 <-genind2loci(Mydata1)

#These distances can be visualized with heatmaps, dendrograms, or minimum spanning networks
#nei.dist(Mydata1) # can not handle missing data
Mydist <- provesti.dist(Mydata1)

out <- as.matrix(Mydist)
write.csv(out,"distance from genotype.csv")

set.seed(999)
theTree <- Mydist %>% 
           nj() %>%    # calculate neighbor-joining tree
           ladderize() # organize branches by clade
plot(theTree,hang=-1)
add.scale.bar(length = 0.05) # add a scale bar showing 5% difference.
#reference
#Prevosti A., Oca\~na J. and Alonso G. (1975) Distances between populations of Drosophila subobscura, based on chromosome arrangements frequencies. Theoretical and Applied Genetics, 45, 231–241. 
#http://grunwaldlab.github.io/Population_Genetics_in_R/Pop_Structure.html

#### Principal Components Analysis ####
library(ggplot2)
library(ggfortify)
lwqc.pca <- cmdscale(Mydist, k = 5, eig = TRUE, add = FALSE, x.ret = FALSE)
autoplot(lwqc.pca,label = T, label.size = 3,xlab="PC1",ylab="PC2") +
  theme_base()

