
org1 <- read_excel('../data/lit_dat.xlsx', 2)
df1 <- org1
df1 <- as.data.table(df1)

org2 <- read_excel('../data/acidification_dat_extra.xlsx', 1)
df2 <- org2
df2 <- as.data.table(df2)
