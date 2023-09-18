#Create balance tables and histograms for Appendix

rm(list=ls())
library(plyr);library(dplyr, warn.conflicts = F)
library(tidyr)
library(ggplot2)
suppressMessages( library(lmtest) )
suppressMessages( library(multiwayvcov) )
suppressMessages(library(stargazer))

s = function(x){summary(factor(x))}
setwd('/Users/jeremyspater/Dropbox/duke/political economy core/prospectus/india part/2018_version/replication_files/scripts and data/balance-tables_histograms/')

A = readRDS('5-16-18_HH-KNearest_DeID_demed.RDS') #
##############################################################################################################################
#Balance table

A = A[A$A.A7_Area.Neighborhood %in% names(which(table(A$A.A7_Area.Neighborhood) >= 30)),]#DROP PLACES WITH <30 OBSERVATIONS

CandConjoint = A %>% filter(Wave %in% c('Bangalore 2016','Jai-Pat 2015'), is.na(L.Candidate_Question_1) == F)  #just the people in the analysis.
CandConjoint$HiSeg = CandConjoint$Nearest10_OwnReligion == 10 
CandConjoint$HiSeg_DeMed = CandConjoint$DeMedNearest10_OwnReligion >= 0
CandConjoint$LoSeg_DeMed = CandConjoint$DeMedNearest10_OwnReligion < 0

CandConjoint$LowCaste = CandConjoint$C.C8_Caste == 'SC/ST/RM'

CandConjoint$Muslim = CandConjoint$C.C6_Religion == 'Muslim'

CandConjoint$Male = CandConjoint$C.C5_Gender == 1

CandConjoint$Migrant = CandConjoint$C.C14_Permanent.Residence.of.Jaipur. == 0

CandConjoint$Jaipur = CandConjoint$City == 'Jaipur'
CandConjoint$Patna = CandConjoint$City == 'Patna'

CandConjoint$C.C4_Age = as.numeric(as.character(CandConjoint$C.C4_Age))

bal.vars = c('AssetSum','LowCaste','Muslim','Male','C.C4_Age', 'Migrant','Jaipur','Patna')
bal.table = data.frame('Segregated' = apply(CandConjoint[CandConjoint$HiSeg,bal.vars],2,function(x){mean(x,na.rm=T)}),
                       'Integrated' = apply(CandConjoint[!CandConjoint$HiSeg,bal.vars],2,function(x){mean(x,na.rm=T)}),
                       'p' = apply(CandConjoint[,bal.vars],2,function(x){t.test(x[CandConjoint$HiSeg],
                                                                                x[!CandConjoint$HiSeg])[['p.value']]}) ) %>%
  round(2) 
bal.table = rbind(bal.table, data.frame('Segregated' = sum(CandConjoint$HiSeg == 1, na.rm = T), 
                                        'Integrated' = sum(CandConjoint$HiSeg == 0, na.rm = T), 'p' = ''))
row.names(bal.table) = c('Asset Index','Low Caste','Muslim','Male','Age','Migrant','Jaipur','Patna','n')

out = stargazer(bal.table, summary = F, digits = 2,
                title = 'Balance Table, Segregated vs. Integrated',
                label = 'table:Nearest10Religion_Balance')
path0 = paste0(getwd(),'/',Sys.Date(),'/')
dir.create(path0)
#writeLines(out,con = paste0(path0,'Nearest10Religion_Balance.tex'));rm(out, bal.vars,bal.table)

#De-medianed
bal.vars = c('AssetSum','LowCaste','Muslim','Male','C.C4_Age', 'Migrant','Jaipur','Patna')
bal.table = data.frame('Segregated' = apply(CandConjoint[CandConjoint$HiSeg_DeMed,bal.vars],2,function(x){mean(x,na.rm=T)}),
                       'Integrated' = apply(CandConjoint[CandConjoint$LoSeg_DeMed,bal.vars],2,function(x){mean(x,na.rm=T)}),
                       'p' = apply(CandConjoint[,bal.vars],2,function(x){t.test(x[CandConjoint$HiSeg_DeMed],
                                                                                x[CandConjoint$LoSeg_DeMed])[['p.value']]}) ) %>%
  round(2) 
bal.table = rbind(bal.table, data.frame('Segregated' = sum(CandConjoint$HiSeg_DeMed == 1, na.rm = T), 
                                        'Integrated' = sum(CandConjoint$LoSeg_DeMed == 1, na.rm = T), 'p' = ''))
row.names(bal.table) = c('Asset Index','Low Caste','Muslim','Male','Age','Migrant','Jaipur','Patna','n')
bal.table

out = stargazer(bal.table, summary = F, digits = 2,
                title = 'Balance Table, Segregated vs. Integrated (De-Medianed)',
                label = 'table:Nearest10Religion_Balance_DeMed')
#writeLines(out,con = paste0(path0,'DeMed_Nearest10Religion_Balance.tex'));rm(out, bal.vars,bal.table)

########################################################################################
#Histograms

#Nearest 10 religion, full sample
ggplot(data=CandConjoint, aes(CandConjoint$Nearest10_OwnReligion)) + geom_bar(aes(y = (..count..)/sum(..count..))) + theme_minimal() +
  labs(x = '10-nearest same religion', y = 'Proportion') + theme(axis.title=element_text(size=14),
                                                                 axis.text = element_text(size = 12)) +
  ggtitle('10-nearest same religion, Full sample') + theme(plot.title = element_text(hjust = 0.5, size = 16))
#ggsave(filename = paste0(path0,'/Nearest10SameReligion.jpg'), height = 150, width = 150, units = 'mm')

#De-Medianned Nearest 10 religion, full sample
ggplot(data=CandConjoint, aes(CandConjoint$DeMedNearest10_OwnReligion)) + geom_bar(aes(y = (..count..)/sum(..count..))) + theme_minimal() +
  labs(x = '10-nearest same religion (de-medianed)', y = 'Proportion') + theme(axis.title=element_text(size=14),
                                                                 axis.text = element_text(size = 12)) +
  ggtitle('De-Medianed 10-nearest same religion,\n Full sample') + theme(plot.title = element_text(hjust = 0.5, size = 16))
#ggsave(filename = paste0(path0,'/DeMedNearest10SameReligion.jpg'), height = 150, width = 150, units = 'mm')

