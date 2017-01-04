#read data of 4 env phenotype from the clipboard

dta <- read.delim("clipboard",header=T)

head(dta)
dim(dta)
class(dta)
library("factoextra")
library("ade4")
dta2 <- dta[,2:5]
rownames(dta2) <- rownames(dta)
head(dta2)
dim(dta2)
# The variable fenzu (index = 5) is removed
# before PCA analysis

group.pca <- dudi.pca(na.omit(dta2), scannf = FALSE, nf = 2)

# fviz_pca_ind(): Graph of individuals
# 
# fviz_pca_var(): Graph of variables
# 
# fviz_pca_biplot(): Biplot of individuals and variables
# 
# fviz_pca(): An alias of fviz_pca_biplot()

# pca graph of variables
library("ggthemes")
fviz_pca_var(group.pca,col.var = "red",col.circle = "NA")+
             #theme_few() + scale_colour_few() 
             theme_base()
fviz_pca_biplot(group.pca,addEllipses = FALSE,
                col.var = "red", alpha.var ="cos2",
                label = "var") +
  scale_color_brewer(palette="Dark2")+
  theme_minimal()

fviz_pca_ind(group.pca,addEllipses =TRUE, ellipse.level = 0.68) +
  theme_minimal()