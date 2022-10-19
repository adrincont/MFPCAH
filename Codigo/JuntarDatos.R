# Juntar excel
library("readxl")
library("plyr")
library("xlsx")
col_names_data = c("Item type","Authors","Title","Journal","Publication year","Volume","Pages","Date published","URLs"
                   ,"DOI","PMID","Arxiv ID","Abstract","Archive prefix","Eprint ID")
dat = data.frame(matrix(ncol = length(col_names_data), nrow = 0))
colnames(dat) = col_names_data
for (i in 0:68) {
  print(i)
  name_i = paste0("Partes xlsx/references (",i,").xlsx")
  dat_i = read_excel(name_i)
  dat =  rbind.fill(dat,dat_i)
}
write.xlsx(dat,'V_CaracteresExtraños.xlsx', col.names=TRUE, row.names=FALSE)