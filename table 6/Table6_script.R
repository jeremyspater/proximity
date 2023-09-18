rm(list=ls())
library(plyr);library(dplyr, warn.conflicts = F)
library(tidyr)
library(ggplot2)
suppressMessages( library(lmtest) )
suppressMessages( library(multiwayvcov) )
suppressMessages(library(stargazer))

s = function(x){summary(factor(x))}
setwd('/Users/jeremyspater/Dropbox/duke/political economy core/prospectus/india part/2018_version/replication_files/scripts and data/table 6')

#read data
A = read.csv('5-21-18_deid_nearestK.csv',
             na.strings=c('','NA'),strip.white=T,stringsAsFactors = F) 

####################################################################################

####################################################################################
#clean data
Aprime = A[A$A.A7_Area.Neighborhood %in% names(which(table(A$A.A7_Area.Neighborhood) >= 30)),]#Drop places with < 30 observations
Aprime = Aprime[which(!is.na(Aprime$A.A7_Area.Neighborhood)),] #drop NA neighborhoods
Aprime = Aprime[which(Aprime$A.A7_Area.Neighborhood != 'Bangalore NA'),] #Still 8273
A = Aprime; rm(Aprime)
####################################################################################

########################################
#Count observations and neighborhoods
length(levels(factor(A$A.A7_Area.Neighborhood))) #182
length(levels(factor(A$A.A7_Area.Neighborhood[which(A$Wave != 'Bangalore 2017')]))) #133

s(A$Wave) #These are basis for numbers in 10-2-19 wave table, except Bangalore 2017, which is from /Users/jeremyspater/Dropbox/duke/wibbels RA/segregation paper/combine bang jai patna/may_16_2018_analysis/neighbor_conjoint_3-11-19_task2-3_newdata.R
s(A$City) #Jaipur, Patna match exactly

length(levels(factor(A$A.A7_Area.Neighborhood[which(A$City == 'Jaipur')]))) #45
length(levels(factor(A$A.A7_Area.Neighborhood[which(A$City == 'Patna')]))) #34
length(levels(factor(A$A.A7_Area.Neighborhood[which(A$Wave == 'Bangalore 2016')]))) #20
length(levels(factor(A$A.A7_Area.Neighborhood[which(A$Wave == 'Bangalore 2017')]))) #49
