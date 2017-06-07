# row and plants with right order
setwd(choose.dir())
newdata = numeric(0)
n = 100 #row number
m = 10  #plant number 

for (i in 1:n)  
    {for (j in 1:m)
        {tmp=paste("CC",i,"-",j)  # substitue "CC" with "***"
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
      {tmp=paste("ZB17 ",rowpl[i,1],"-",j)
       newdata=rbind(newdata,tmp)}}
colnames(newdata) <- "bianhao"
write.csv(newdata,file="bianhaozy.csv",row.names = FALSE)
