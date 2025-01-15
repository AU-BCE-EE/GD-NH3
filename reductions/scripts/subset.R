
# data frame for application techniques
dfApp <- df[! df$app.ref2 == df$app.treat.general , ]
# selecting observations without slurry treatments (UI, acid, dilution etc.)
dfApp <- dfApp[dfApp$additive.class == 'n/a' | is.na(dfApp$additive.class), ]

# checking additives
unique(df$additive.class)

# data frame for acidification 
dfAcid <- df[additive.class %in% c('acid', 'Acid')]
# missing Wagner 2021, Seidel 2017, Anderson 2022, Nyord 2013
# use ranges from Fanguerio et al., 2015? 


# data frame for separation (selecting observations where additive.class is liquid fraction)
dfLiq <- df[additive.class %in% c('liquid fraction', 'liquid fraction.')]
# missing a lot! compare with plot from Pedersen et al., 2022 
# use ranges from Pedersen et al., 2022? 


# the data from Webb 2010 is not even in the database.... 

# check if the data from the original GD is in the database 


# trailing hose, trailing shoe, shallow injection, deep injection 
# separation 
# acidification 
# incorporation of solids
