
# new studies
org <- read_excel('../data/lit_dat.xlsx', 2)
df <- org
df <- as.data.table(df)


# studies from 2014 GD 
orgOld <- read_excel('../data/data_2014.xlsx', 2)
dfOld <- orgOld
