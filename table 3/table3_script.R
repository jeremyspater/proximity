rm(list=ls())
setwd('/Users/jeremyspater/Dropbox/duke/political economy core/prospectus/india part/2018_version/replication_files/scripts and data/table 3')

library('plyr');library('dplyr')
library('stringr')

#India as a whole
InSc = read.csv('india_sc_2-21-19.csv',
                stringsAsFactors = F) %>%
  rename(PopSc = Population)
InTo = read.csv('india_pop_2-25-19.csv',
                stringsAsFactors = F)
In = merge(InSc, InTo, by = 'State') %>%
  mutate(PropSc = ( PopSc %>% str_replace_all(',','') %>% as.numeric() ) /
           ( Population %>% str_replace_all(',','') %>% as.numeric()  ),
         Pop = Population %>% str_replace_all(',','') %>% as.numeric(),
         PopSc = PopSc %>% str_replace_all(',','') %>% as.numeric()) %>%
  filter(State != 'India')
rm(InSc, InTo)

#Karnataka district data
KaL = readRDS('Karnataka_district_censusdata_2-22-19.RDS')
Ka = data.frame(matrix(unlist(KaL), nrow=length(KaL), byrow=T)) 
names = Ka[1,1:13]
Ka = Ka %>% select(X27:X39)
names(Ka) = names[1,] %>% apply(1,as.character)  
Ka = Ka %>% rename(STprF = 'Scheduled Tribes (ST) %',
                   SCprF = 'Scheduled Caste (SC) %')
Ka$Pop = Ka$Population %>% as.character() %>% as.numeric()
Ka$SCpr = Ka$SCprF %>% as.character() %>% as.numeric()
Ka$SCpo = Ka$SCpr * Ka$Pop
rm(names,KaL)

#Rajasthan district data
RaL = readRDS('Rajastan_district_censusdata_2-22-19.RDS')
Ra = data.frame(matrix(unlist(RaL), nrow=length(RaL), byrow=T)) 
names = Ra[1,1:13]
Ra = Ra %>% select(X27:X39)
names(Ra) = names[1,] %>% apply(1,as.character)  
Ra = Ra %>% rename(STprF = 'Scheduled Tribes (ST) %',
                   SCprF = 'Scheduled Caste (SC) %')
Ra$Pop = Ra$Population %>% as.character() %>% as.numeric()
Ra$SCpr = Ra$SCprF %>% as.character() %>% as.numeric()
Ra$SCpo = Ra$SCpr * Ra$Pop
rm(names,RaL)

#Bihar district data
BiL = readRDS('Bihar_district_censusdata_2-22-19.RDS')
Bi = data.frame(matrix(unlist(BiL), nrow=length(BiL), byrow=T)) 
names = Bi[1,1:13]
Bi = Bi %>% select(X27:X39)
names(Bi) = names[1,] %>% apply(1,as.character)  
Bi = Bi %>% rename(STprF = 'Scheduled Tribes (ST) %',
                   SCprF = 'Scheduled Caste (SC) %')
Bi$Pop = Bi$Population %>% as.character() %>% as.numeric()
Bi$SCpr = Bi$SCprF %>% as.character() %>% as.numeric()
Bi$SCpo = Bi$SCpr * Bi$Pop
rm(names,BiL)

#Bangalore-District data
BdL = readRDS('Bangalore_District_censusdata_2-25-19.RDS')
Bd = data.frame(matrix(unlist(BdL), nrow=length(BdL), byrow=T)) 
names = Bd[1,1:13]
Bd = Bd %>% select(X27:X39)
names(Bd) = names[1,] %>% apply(1,as.character)  
Bd = Bd %>% rename(STprF = 'Scheduled Tribes (ST) %',
                   SCprF = 'Scheduled Caste (SC) %')
Bd$Pop = Bd$Population %>% as.character() %>% as.numeric()
Bd$SCpr = Bd$SCprF %>% as.character() %>% as.numeric()
Bd$SCpo = Bd$SCpr * Bd$Pop
rm(names,BdL)

#Jaipur-District data
JdL = readRDS('Jaipur_District_censusdata_2-25-19.RDS')
Jd = data.frame(matrix(unlist(JdL), nrow=length(JdL), byrow=T)) 
names = Jd[1,1:13]
Jd = Jd %>% select(X27:X39)
names(Jd) = names[1,] %>% apply(1,as.character)  
Jd = Jd %>% rename(STprF = 'Scheduled Tribes (ST) %',
                   SCprF = 'Scheduled Caste (SC) %')
Jd$Pop = Jd$Population %>% as.character() %>% as.numeric()
Jd$SCpr = Jd$SCprF %>% as.character() %>% as.numeric()
Jd$SCpo = Jd$SCpr * Jd$Pop
rm(names,JdL)

#Patna-District data
PdL = readRDS('Bihar_District_censusdata_2-25-19.RDS')
Pd = data.frame(matrix(unlist(PdL), nrow=length(PdL), byrow=T)) 
names = Pd[1,1:13]
Pd = Pd %>% select(X27:X39)
names(Pd) = names[1,] %>% apply(1,as.character)  
Pd = Pd %>% rename(STprF = 'Scheduled Tribes (ST) %',
                   SCprF = 'Scheduled Caste (SC) %')
Pd$Pop = Pd$Population %>% as.character() %>% as.numeric()
Pd$SCpr = Pd$SCprF %>% as.character() %>% as.numeric()
Pd$SCpo = Pd$SCpr * Pd$Pop
rm(names,PdL)

#BBMP data
BaL = readRDS('BBMP_ward_censusdata_2-12-19.RDS')
Ba = data.frame(matrix(unlist(BaL), nrow=length(BaL), byrow=T)) 
names = Ba[1,1:11]
Ba = Ba %>% select(X23:X33)
names(Ba) = names[1,] %>% apply(1,as.character)  
Ba = Ba %>% rename(STprF = 'Scheduled Tribes (ST) %',
                   SCprF = 'Scheduled Caste (SC) %')
Ba$Pop = Ba$Population %>% as.character() %>% as.numeric()
Ba$SCpr = Ba$SCprF %>% as.character() %>% as.numeric()
Ba$SCpo = Ba$SCpr * Ba$Pop
rm(names,BaL)

#Jaipur data
JaL = readRDS('Jaipur_ward_censusdata_2-14-19.RDS')
Ja = data.frame(matrix(unlist(JaL), nrow=length(JaL), byrow=T)) 
names = Ja[1,1:11]
Ja = Ja %>% select(X23:X33)
names(Ja) = names[1,] %>% apply(1,as.character)  
Ja = Ja %>% rename(STprF = 'Scheduled Tribes (ST) %',
                   SCprF = 'Scheduled Caste (SC) %')
Ja$Pop = Ja$Population %>% as.character() %>% as.numeric()
Ja$SCpr = Ja$SCprF %>% as.character() %>% as.numeric()
Ja$SCpo = Ja$SCpr * Ja$Pop
rm(names,JaL)

#############################################################################################################
#SEGREGATION CALCULATIONS

#DISSIMILARITY INDEX FOR INDIA AS A WHOLE (SC)
#P: total minority proportion; ti: total pop in gridspace; pi: prop minority in gridspace
P = sum(In$PopSc) / sum(In$Pop)
T0 = sum(In$Pop)
In$D = In$Pop * abs(In$PropSc - P) / (2 * T0 * P * (1 - P))
D_India = sum(In$D) #This gives final value for dissimilarity index, based on SC population, for India as a whole
rm(T0,P)

#DISSIMILARITY INDEX FOR KARNATAKA AS A WHOLE (SC)
P = sum(Ka$SCpo) / sum(Ka$Pop)
T0 = sum(Ka$Pop)
Ka$D = Ka$Pop * abs(Ka$SCpr - P) / (2 * T0 * P * (1 - P))
D_Karnataka = sum(Ka$D)
rm(T0,P)

#DISSIMILARITY INDEX FOR RAJASTHAN AS A WHOLE (SC)
P = sum(Ra$SCpo) / sum(Ra$Pop)
T0 = sum(Ra$Pop)
Ra$D = Ra$Pop * abs(Ra$SCpr - P) / (2 * T0 * P * (1 - P))
D_Rajasthan = sum(Ra$D)
rm(T0,P)

#DISSIMILARITY INDEX FOR BIHAR AS A WHOLE (SC)
P = sum(Bi$SCpo) / sum(Bi$Pop)
T0 = sum(Bi$Pop)
Bi$D = Bi$Pop * abs(Bi$SCpr - P) / (2 * T0 * P * (1 - P))
D_Bihar = sum(Bi$D)
rm(T0,P)

#DISSIMILARITY INDEX FOR BANGALORE-DISTRICT BY TEHSIL
P = sum(Bd$SCpo) / sum(Bd$Pop)
T0 = sum(Bd$Pop)
Bd$D = Bd$Pop * abs(Bd$SCpr - P) / (2 * T0 * P * (1 - P))
D_BaD = sum(Bd$D) #
rm(T0,P)

#DISSIMILARITY INDEX FOR JAIPUR-DISTRICT BY TEHSIL
P = sum(Jd$SCpo) / sum(Jd$Pop)
T0 = sum(Jd$Pop)
Jd$D = Jd$Pop * abs(Jd$SCpr - P) / (2 * T0 * P * (1 - P))
D_JaD = sum(Jd$D)
rm(T0,P)

#DISSIMILARITY INDEX FOR PATNA-DISTRICT BY TEHSIL
P = sum(Pd$SCpo) / sum(Pd$Pop)
T0 = sum(Pd$Pop)
Pd$D = Pd$Pop * abs(Pd$SCpr - P) / (2 * T0 * P * (1 - P))
D_PdD = sum(Pd$D)
rm(T0,P)

#DISSIMILARITY INDEX FOR BBMP (198) BY WARD 
P = sum(Ba$SCpo) / sum(Ba$Pop)
T0 = sum(Ba$Pop)
Ba$D = Ba$Pop * abs(Ba$SCpr - P) / (2 * T0 * P * (1 - P))
D_Bbmp = sum(Ba$D) #very dissimilar; min 0 to max 0.5
rm(T0,P)

#DISSIMILARITY INDEX FOR JAIPUR BY WARD
P = sum(Ja$SCpo) / sum(Ja$Pop)
T0 = sum(Ja$Pop)
Ja$D = Ja$Pop * abs(Ja$SCpr - P) / (2 * T0 * P * (1 - P))
D_Jaipur = sum(Ja$D)
rm(T0,P)

#********THIS IS THE BASIS FOR TABLE 3********#
data.frame(Bangalore = c(D_India, D_Karnataka, D_BaD, D_Bbmp),
           Jaipur = c(D_India, D_Rajasthan, D_JaD, D_Jaipur),
           Patna = c(D_India, D_Bihar, D_PdD, NA))
#############################################################################################################

