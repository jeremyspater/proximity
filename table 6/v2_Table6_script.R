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
Q = A[which(A$Wave == 'Bangalore 2017'),]

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

#THIS IS BASIS FOR TABLE 6

#2017 neighborhoods
length(levels(factor(Q$A.A7_Area.Neighborhood))) #50
#2017 respondents
nrow(Q) #1948

#Bangalore '16 neighborhoods
length(levels(factor(A$A.A7_Area.Neighborhood[which(A$Wave == 'Bangalore 2016')]))) #20
#Bangalore '16 respondents
nrow(A[which(A$Wave == 'Bangalore 2016'),]) #609

#Jaipur neighborhoods
length(levels(factor(A$A.A7_Area.Neighborhood[which(A$City == 'Jaipur')]))) #45
#Jaipur respondents
nrow(A[which(A$City == 'Jaipur'),]) #2669

#Patna neighborhoods
length(levels(factor(A$A.A7_Area.Neighborhood[which(A$City == 'Patna')]))) #34
#Patna respondents
nrow(A[which(A$City == 'Patna'),]) #1972