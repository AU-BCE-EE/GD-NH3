
# data frame for acidification 
df1 <- df1[additive.class %in% c('acid', 'Acid')]

# removing laboratory studies
df1 <- df1[!grepl("fangueiro et al. \\(2017\\)", df1$study, ignore.case = TRUE), ]
df1 <- df1[!grepl("silva et al. \\(2022a\\)", df1$study, ignore.case = TRUE), ]
df1 <- df1[!grepl("silva et al. \\(2022b\\)", df1$study, ignore.case = TRUE), ]