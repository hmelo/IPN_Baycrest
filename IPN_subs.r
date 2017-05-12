#library("lme4", lib.loc="/Library/Frameworks/R.framework/Versions/3.1/Resources/library")
#library("lmerTest", lib.loc="/Library/Frameworks/R.framework/Versions/3.1/Resources/library")
#library(effects)

IPN.data = read.csv("Dataset.csv")

# send github invites

# Sample: 
# Subjects with neurological disorder with caregiver information (zarscore <99, cesscore<99 )
# (cgcc ==1|3, parkin==1, als==1, prstroke==1, epilepsy==1, ms==1) & zarscore <99, cesscore<99
# n=851

IPN.dataframe <- data.frame(IPN.data) 

subs <- IPN.dataframe[ IPN.dataframe$cgcc==1 | IPN.dataframe$cgcc==3 | IPN.dataframe$parkin==1 | IPN.dataframe$prstroke==1 | IPN.dataframe$als==1 |  IPN.dataframe$epilepsy==1 | IPN.dataframe$ms==1,] 
subs <- subs[ subs$zarscore<99 & subs$cesscore<99,] 

subs[1:10,1:10]

