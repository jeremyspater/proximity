#Table 4: Comparison of most common occupations for network and household data sets. 

rm(list=ls())
maindir='/Users/jeremyspater/Dropbox/duke/political economy core/prospectus/india part/2018_version/replication_files/scripts and data/table 4'
setwd(maindir)

library(plyr);library(dplyr, warn.conflicts = FALSE)
library(tidyr);library(ggplot2)
suppressMessages(library(multiwayvcov, warn.conflicts = F))
suppressMessages(library(lmtest, warn.conflicts = F))
library(stargazer)

s = function(x){summary(factor(x))}

#Read network data
A = readRDS('5-22-18_Network-KNearest_DeID_demed.RDS')
#Read and clean household sample data
H = read.csv('5-21-18_deid_nearestK.csv',
             na.strings=c('','NA'),strip.white=T,stringsAsFactors = F)
Hprime = H[H$A.A7_Area.Neighborhood %in% names(which(table(H$A.A7_Area.Neighborhood) >= 30)),]#Drop neighborhoods with < 30 observations
Hprime = Hprime[which(!is.na(Hprime$A.A7_Area.Neighborhood)),] #drop NA neighborhoods
Hprime = Hprime[which(Hprime$A.A7_Area.Neighborhood != 'Bangalore NA'),]
H = Hprime; rm(Hprime)

########################################################################################################################

############################################################################################################
#Map values to occupations
A$Job = mapvalues(A$D.D1_Occupation,
                  from = c(12,
                           13,
                           19,
                           2,
                           20,
                           21,
                           3,
                           4,
                           6,
                           7,
                           9),
                  to = c('Garbage',
                         'Gardener',
                         'Security',
                         'Butcher',
                         'Tailor',
                         'Vendor',
                         'Carpenter',
                         'Construction',
                         'Cook',
                         'Corporate', 
                         'Electrician'))

H$Job = mapvalues(H$D.D1_Occupation.,
                  from = c(1,
                           2,
                           3,
                           4,
                           5,
                           6,
                           7,
                           8,
                           9,
                           10,
                           11,
                           12,
                           13,
                           14,
                           15,
                           16,
                           17,
                           18,
                           19,
                           20,
                           21,
                           22,
                           23,
                           24,
                           25,
                           26, #B17: retired
                           27), #B17: unemployed
                  to = c('Agriculture',
                         'Butcher',
                         'Carpenter',
                         'Construction',
                         'Labour',
                         'Cook',
                         'Corporation', 
                         'Driver',
                         'Electrical',
                         'Factory',
                         'Flower',
                         'Garbage',
                         'Gardener',
                         'Maid',
                         'Mechanic',
                         'Painter',
                         'ProfessionalSvc',
                         'Grocessory',
                         'Security',
                         'Tailor',
                         'Vendor',
                         'Government',
                         'Housewife',
                         'Student',
                         'Other',
                         NA, NA )) #Map retired/unemployed to NA so they don't affect denominator

#Note that B17 includes retired/unemployed; these are mapped above to NA and dropped from table (and not included in denominator)

#Network
sort (s(A$Job) / sum(!is.na(A$Job )) * 100  , decreasing = T) %>% round(2)
#Household
sort (s(H$Job) / sum(!is.na(H$Job )) * 100  , decreasing = T) %>% round(2)
#These are basis for manually-created Table 4

################################################################################################
