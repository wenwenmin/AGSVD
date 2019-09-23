# ------------------------------
# R script by wenwen.min@qq.com
# ------------------------------
library(xtable)
datatypes = c("camp1","pollen","patel","yan")

TableList = list()
for(ii in 1:length(datatypes)){
  # "../F4_yanData/Result_yanData_moduleNum=10.RData"
  fileName = paste("../","F",ii,"_", datatypes[ii],"Data/Result_",datatypes[ii],"Data_moduleNum=2.RData",sep="")
  load(fileName)
  #print(round(res,3))
  print(ii)
  TableList[[ii]] = round(res,3)
}

Table2 = TableList[[1]]
for(j in 2:length(TableList)){
  temp = TableList[[j]]
  Table2 = cbind(Table2,temp[,2:4])
}
print(xtable(Table2), include.rownames=FALSE)
###################################################################################################################
###################################################################################################################
TableList = list()
for(ii in 1:length(datatypes)){
  # "../F4_yanData/Result_yanData_moduleNum=10.RData"
  fileName = paste("../","F",ii,"_", datatypes[ii],"Data/Result_",datatypes[ii],"Data_moduleNum=10.RData",sep="")
  load(fileName)
  #print(round(res,3))
  print(ii)
  TableList[[ii]] = round(res,3)
}

Table10 = TableList[[1]]
for(j in 2:length(TableList)){
  temp = TableList[[j]]
  Table10 = cbind(Table10,temp[,2:4])
}
print(xtable(Table10), include.rownames=FALSE)
###################################################################################################################
res = rbind(Table10,Table2)

write.table(res, file = "Result_Table4.csv", append = FALSE, quote = TRUE, sep = ",",
            eol = "\n", na = "NA", dec = ".", row.names = F, col.names = T)
###################################################################################################################




