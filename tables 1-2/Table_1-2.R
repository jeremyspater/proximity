######################################################################
rm(list=ls())

s = function(x){summary(factor(x))}
Num = function(x){as.numeric(as.factor(x))}

library(plyr);library(dplyr, warn.conflicts = FALSE)
library(tidyr);library(ggplot2)
suppressMessages(library(multiwayvcov, warn.conflicts = F))
suppressMessages(library(lmtest, warn.conflicts = F))
library(stargazer)

setwd('/Users/jeremyspater/Dropbox/duke/political economy core/prospectus/india part/2018_version/replication_files/scripts and data/tables 1-2')
A = readRDS('5-16-18_HH-KNearest_DeID_demed.RDS')

A = A[-which(A$Wave == 'Bangalore 2015'),] #6101

Aprime = A[A$A.A7_Area.Neighborhood %in% names(which(table(A$A.A7_Area.Neighborhood) >= 30)),]#DROP PLACES WITH <30 OBSERVATIONS; down to 6394 observations
Aprime = Aprime[which(!is.na(Aprime$A.A7_Area.Neighborhood)),] #drop NA neighborhoods
Aprime = Aprime[which(Aprime$A.A7_Area.Neighborhood != 'Bangalore NA'),]

A = Aprime #5250
######################################################################

##############################################################################

A$Muslim = A$C.C6_Religion == 'Muslim'

#Water
A$HtH = A$J.J3_Source.of.water == 4
WaterReg = glm('HtH ~ AssetSum + Muslim + City + A.A7_Area.Neighborhood', data = A, family = 'binomial')

#Voter
A$Voter = A$L.L8_Voter.ID == 1
VoterIDReg = glm('Voter ~ AssetSum + Muslim + City + A.A7_Area.Neighborhood', data = A, family = 'binomial')

#Ration Card
A$Ration = A$L.L9_Ration.card == 1
RationCardReg = glm('Ration ~ AssetSum + Muslim + City + A.A7_Area.Neighborhood', data = A, family = 'binomial')

#Security
A$Security = mapvalues(A$J.J24_Eviction,
                       from = c(888, 999),
                       to = c(NA, NA))
SecurityReg = lm('Security ~ AssetSum + Muslim + City + A.A7_Area.Neighborhood', data = A)

#Primary School
A$L20 = mapvalues(A$L.L20_Primary.School,
                  from = c(777, 888, 999),
                  to = c(NA, NA, NA))
PrimSchReg = lm('-L20 ~ AssetSum + Muslim + City + A.A7_Area.Neighborhood', data = A) #Switch scale so negative is LESS satisfied

#Secondary school
A$L21 = mapvalues(A$L.L21_Secondary.School,
                  from = c(777, 888, 999),
                  to = c(NA, NA, NA))
SecSchReg = lm('-L21 ~ AssetSum + Muslim + City + A.A7_Area.Neighborhood', data = A)

#Waste satisfaction
A$L24 = mapvalues(A$L.L24_Waste.Disposal,
                  from = c(777, 888, 999),
                  to = c(NA, NA, NA))
WasteSatReg = lm('-L24 ~ AssetSum + Muslim + City + A.A7_Area.Neighborhood', data = A)
##############################################################################

##############################################################################
#Make tables

#Neighborhood services
MuslimServices_1 = stargazer(WaterReg,
                           VoterIDReg,
                           RationCardReg,
                           dep.var.labels.include = T,
                           model.names = FALSE,
                           digits = 2, 
                           omit        = c("A.A7_Area.Neighborhood","City"),
                           omit.labels = c("Neighborhood dummies?","City dummies?"),
                           omit.stat = c("rsq","ll","ser","f"),
                           order=c(2,1,3),
                           covariate.labels = c('Muslim',
                                                'Assets',
                                                'Constant'),
                           dep.var.labels = c("Water Connection","Voter ID","Ration Card"), 
                           title = 'Public Services by Religion',
                           label = 'table:Muslim_Services_1')
writeLines(MuslimServices_1,con = 'Muslim_Services_1.tex')
#NOTE: Columns must be renamed manually in LaTeX file to match version in paper
#Replace this line: %\\[-1.8ex] & \multicolumn{3}{c}{Water Connection} \\ 
#With this line: \\[-1.8ex] & {Water Connection} & {Voter ID} & {Ration Card} \\ 

MuslimServices_2 = stargazer(SecurityReg,
                             PrimSchReg,
                             SecSchReg,
                             WasteSatReg,
                             dep.var.labels.include = T,
                             model.names = FALSE,
                             digits = 2, 
                             omit        = c("A.A7_Area.Neighborhood","City"),
                             omit.labels = c("Neighborhood dummies?","City dummies?"),
                             omit.stat = c("rsq","ll","ser","f"),
                             order=c(2,1,3),
                             covariate.labels = c('Muslim',
                                                  'Assets',
                                                  'Constant'),
                             dep.var.labels = c("Tenure Security","Prim. Sch. Satis.","Sec. Sch. Satis.", "Waste Remov. Satis."), #Why does only the first one show up? 
                             title = 'Services Satisfaction by Religion',
                             label = 'table:Muslim_Services_2')
writeLines(MuslimServices_2,con = 'Muslim_Services_2.tex')
#NOTE: Columns must be renamed manually in LaTeX file to match version in paper
#Replace this line: \\[-1.8ex] & \multicolumn{4}{c}{Tenure Security} \\ 
#With this line: \\[-1.8ex] & {Tenure Sec.} & {Prim. School} & {Sec. School} & {Waste Remov.} \\ 

#########################

#########################
#Hindu-Muslim support for same leader

A$L.L50_Neighborhood.Leader[which(A$L.L50_Neighborhood.Leader == '-999')] = NA #5 of these
A$L.L50_Neighborhood.Leader = factor(A$L.L50_Neighborhood.Leader)

L = A %>% group_by(A.A7_Area.Neighborhood) %>% summarize(
  HinLeaderName = names(sort(table(L.L50_Neighborhood.Leader[which(C.C6_Religion == 'Hindu')], useNA = 'no'),decreasing=T))[1],
  MusLeaderName = names(sort(table(L.L50_Neighborhood.Leader[which(C.C6_Religion == 'Muslim')], useNA = 'no'),decreasing=T))[1],
  nHin = sum(C.C6_Religion == 'Hindu', na.rm = T),
  nMus = sum(C.C6_Religion == 'Muslim', na.rm = T),
  nHinAns = sum(!is.na(L.L50_Neighborhood.Leader[which(C.C6_Religion == 'Hindu')])),
  nMusAns = sum(!is.na(L.L50_Neighborhood.Leader[which(C.C6_Religion == 'Muslim')])),
  nHinLeader = sort(table(L.L50_Neighborhood.Leader[which(C.C6_Religion == 'Hindu')], useNA = 'no'),decreasing=T)[1],
  nMusLeader = sort(table(L.L50_Neighborhood.Leader[which(C.C6_Religion == 'Muslim')], useNA = 'no'),decreasing=T)[1]
    ) 

L = L %>% data.frame() %>% 
  mutate(Match = HinLeaderName == MusLeaderName,
         PropHin = nHin / (nHin + nMus),
         PropMus = nMus / (nHin + nMus))

s(L$Match) #false 78 true 21
78 + 21 #99
20 + 45 + 34 #99; from table
s(L$Match[which(L$nMusLeader > 0)]) #false 39, true 21
39 / (39 + 21) # 0.65

#Neighborhoods where both Hindus and Muslims answer
s(L$Match[which(L$nMusLeader > 0 & L$nHinLeader > 0)]) #false 37, true 21
37 / (37 + 21) # 0.64
37 + 21 #58