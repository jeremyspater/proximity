#Table 5: Correlations of k-nearest-neighbors metric, 
#calculated from neighborhood census network data, 
#with the proportion of social links to members of the same group.

rm(list=ls())
library(plyr);library(dplyr, warn.conflicts = F)
library(tidyr)
library(igraph, warn.conflicts = F)
library(geosphere)
library(ggplot2)
library(tictoc)
maindir='/Users/jeremyspater/Dropbox/duke/political economy core/prospectus/india part/2018_version/replication_files/scripts and data/table 5'
setwd(maindir)

s = function(x){summary(factor(x))}

#Read data
dat3 = readRDS('5-22-18_Network-KNearest_DeID_demed.RDS')
##########################################################################################

#Relationships between census-nearest-K and census-contact
#These are basis for manually-created Table 5

cor(dat3$Nearest5_SameRel, dat3$AllLinksSameRelig, use = 'pairwise.complete.obs') #0.3779793
cor(dat3$Nearest10_SameRel, dat3$AllLinksSameRelig, use = 'pairwise.complete.obs') #0.4163651
cor(dat3$Nearest15_SameRel, dat3$AllLinksSameRelig, use = 'pairwise.complete.obs') #0.4222002

cor(dat3$Nearest5_SameCaste, dat3$AllLinksSameCaste, use = 'pairwise.complete.obs') #0.47723
cor(dat3$Nearest10_SameCaste, dat3$AllLinksSameCaste, use = 'pairwise.complete.obs') #0.5185767
cor(dat3$Nearest15_SameCaste, dat3$AllLinksSameCaste, use = 'pairwise.complete.obs') #0.5329627

