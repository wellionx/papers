# row and plants with right order
setwd(choose.dir())
newdata = numeric(0)
n = 100 #row number
m = 10  #plant number 

for (i in 1:n)  
    {for (j in 1:m)
        {tmp=paste("CC",i,"-",j,sep = "")  # substitue "CC" with "***"
         newdata=rbind(newdata,tmp)}}
str(newdata)
colnames(newdata) <- "bianhao"
write.csv(newdata,file="bianhao.csv", row.names = FALSE)

# row number and plants seperately for zhongyu ####
newdata = numeric(0)
#plant number can also read from files 
# column1 is number row, column2 is number of plants 
rowpl = read.csv("row plants number.csv") 
n = length(rowpl$row) #how many rows
for (i in 1:n)  
   {cat(i,"\n")
    m = rowpl[i,2]  #how many plants
    for (j in 1:m)
      {tmp=paste("ZB17 ",rowpl[i,1],"-",j,sep = "")
       newdata=rbind(newdata,tmp)}}
colnames(newdata) <- "bianhao"
write.csv(newdata,file="bianhaozy.csv",row.names = FALSE)

# transform to rows with every 12 cell
to12 = numeric(0)
x=length(newdata) / 12 # 每行12个，可以根据情况更改
r = ceiling(x)  # calculate the rows
# r2 = trunc(x)
for (i in 1:r)
{tmp=t(newdata[(12*(i-1)+1):(12*i)])
 to12 = rbind(to12,tmp)}
write.csv(to12,file="bianhaozy2.csv",row.names = FALSE)

# transform to rows with every 12 cell with reading files
data <- read.csv("bianhaozy.csv")
str(data)
data2 <- as.character(data[,1])
to12 = numeric(0)
x=length(data[,1]) / 12 # 每行12个，可以根据情况更改
r = ceiling(x)  # calculate the rows
# r2 = trunc(x)
for (i in 1:r)
{tmp=t(data2[(12*(i-1)+1):(12*i)])
to12 = rbind(to12,tmp)}
write.csv(to12,file="zyto123.csv",row.names = FALSE) 

# anothoer methods from linlab
sink(paste('zyto122','.csv',sep=''))
for (j in 1:r)
{
  cat((as.character(data[12*(j-1)+seq(12),])),'\n')
}
sink()

