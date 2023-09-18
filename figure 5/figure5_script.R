#Figure 5
#Coethnic voting preferences in candidate conjoint experiment,
#comparing high- to low-exposure respondents in network dataset. 

rm(list=ls())
maindir='/Users/jeremyspater/Dropbox/duke/political economy core/prospectus/india part/2018_version/replication_files/scripts and data/figure 5'
setwd(maindir)

library(plyr);library(dplyr, warn.conflicts = FALSE)
library(tidyr);library(ggplot2)
suppressMessages(library(multiwayvcov, warn.conflicts = F))
suppressMessages(library(lmtest, warn.conflicts = F))
library(stargazer)

s = function(x){summary(factor(x))}

A = readRDS('5-22-18_Network-KNearest_DeID_demed.RDS')
########################################################################################################################
#Balance tables

A$HiSeg = A$Nearest10_SameRel == 10 
A$HiSeg_DeMed = A$DeMedNearest10_SameRel >= 0
A$LoSeg_DeMed = A$DeMedNearest10_SameRel < 0

A$LowCaste = A$C.C8_Caste == 'SC/ST'

A$Muslim = A$C.C6 == 'Muslim'

A$Male = A$C.C5_Gender == 'M'

A$Migrant = A$C.C14_Live.in.Jaipur. == 0

A$Jaipur = A$City == 'Jaipur'
A$Patna = A$City == 'Patna'

A$C.C4_Age = as.numeric(as.character(A$C.C4_Age))

A$Income1 = mapvalues(A$F.F1_Monthly.Income, from = c(-888, 0, 888, 999), to = c(NA, NA, NA, NA)) / 1000
A$Income2 = A$Income1 > 10000 

#Non-de-medianed
bal.vars = c('Income1','LowCaste','Muslim','Male','C.C4_Age', 'Migrant','Jaipur')
bal.table = data.frame('Segregated' = apply(A[A$HiSeg,bal.vars],2,function(x){mean(x,na.rm=T)}),
                       'Integrated' = apply(A[!A$HiSeg,bal.vars],2,function(x){mean(x,na.rm=T)}),
                       'p' = apply(A[,bal.vars],2,function(x){t.test(x[A$HiSeg],
                                                                     x[!A$HiSeg])[['p.value']]}) ) %>%
  round(2) 
bal.table = rbind(bal.table, data.frame('Segregated' = sum(A$HiSeg == 1, na.rm = T), 
                                        'Integrated' = sum(A$HiSeg == 0, na.rm = T), 'p' = ''))
row.names(bal.table) = c('Income (k INR/mo.)','Low Caste','Muslim','Male','Age','Migrant','Jaipur','n')
bal.table
out = stargazer(bal.table, summary = F, digits = 2,
                title = 'Balance Table, Segregated vs. Integrated',
                label = 'table:Nearest10Religion_Balance')
path0 = paste0(getwd(), '/', Sys.Date(),'/')
dir.create(path0)
#writeLines(out,con = paste0(path0,'Network_Nearest10Religion_Balance.tex'));rm(out, bal.vars,bal.table)
########################################################################################################################

########################################################################################################################
#Begin conjoint analysis

Q = A

#Rename variables

Q = Q %>% dplyr::rename(Q1A1 = I.Neta_Random_A1, #first question, candidate 1, characteristic A
                        Q1A2 = I.Neta_Random_A2, #first question, candidate 2, characteristic A
                        Q1B1 = I.Neta_Random_B1, #first question, candidate 1, characteristic B
                        Q1B2 = I.Neta_Random_B2, #first question, candidate 2, characteristic B
                        Q2A1 = I.Neta_Random_A3, #second question, candidate 1, characteristic A
                        Q2A2 = I.Neta_Random_A4, #second question, candidate 2, characteristic A
                        Q2B1 = I.Neta_Random_B3, #second question, candidate 1, characteristic B
                        Q2B2 = I.Neta_Random_B4, #second question, candidate 2, characteristic B
                        Q3A1 = I.Neta_Random_A5, #third question, candidate 1, characteristic A
                        Q3A2 = I.Neta_Random_A6, #third question, candidate 2, characteristic A
                        Q3B1 = I.Neta_Random_B5, #third question, candidate 1, characteristic B
                        Q3B2 = I.Neta_Random_B6, #third question, candidate 2, characteristic B
                        Q1 = I.Neta_Question_1, #first question, choose candidate 1 or 2
                        Q2 = I.Neta_Question_2, #first question, choose candidate 1 or 2
                        Q3 = I.Neta_Question_3 #first question, choose candidate 1 or 2
) 

#rearrange so one row is one conjoint observation. 3x as many rows as A
#new variables: A1, B1 are two traits for candidate 1; similar for 2; and y is responent's choice between candidates
B = Q %>% unite('Q1', matches('Q1')) %>% unite('Q2', matches('Q2')) %>% unite('Q3', matches('Q3')) %>%
  gather(Question, b, starts_with('Q')) %>% arrange(X) %>% separate( 'b', c('A1','A2','B1','B2','y') )

B = B %>% filter(! (A1 == A2 & B1 == B2)) #drop observations where candidates have same profile

#make new data frame where each row is one PROFILE, ie each question becomes two rows (one for each candidate)
#New variables: A1, A2 are combined as A: trait A for either candidate
C = B %>% unite('AB1',c(A1,B1)) %>% unite('AB2',c(A2,B2)) %>% gather(Neighbor, AB, c(AB1, AB2)) %>%
  arrange(X) %>% separate('AB', c('A','B')) %>% 
  mutate(Neighbor = as.numeric(mapvalues(Neighbor, from = c('AB1', 'AB2'), to = c(1,2))))


#function to make dummies for trait levels
ModFn = function(x,f){
  data.frame(x, model.matrix(as.formula(f), data=x)) }

C = C %>% ModFn('~ A - 1') %>% ModFn('~ B - 1') #function to make dummies for trait levels
C = C %>% mutate(Y = y == Neighbor) #1 when that candidate is picked
C = C %>% dplyr::rename(A_0 = A0, #reanme variables to be consistent with earlier version of code
                        A_1 = A1,
                        A_2 = A2,
                        A_3 = A3,
                        B_0 = B0,
                        B_1 = B1,
                        B_2 = B2,
                        B_3 = B3,
                        B_4 = B4)

B = C; rm(C) #rename to be consistent with earlier version
###########################################################################################

###########################################################################################
#do analysis and make plots

#regression formula
form1 = as.formula(paste0('Y ~ ',
                          paste(strsplit('A_0 A_1 A_2 A_3 B_0 B_1 B_2 B_3 B_4',
                                         split = ' ')[[1]], collapse = ' + ' )))

#Do regression
lm0 = lm(form1, data = B)
#Calculate clustered SE's
lm_clus = coeftest(lm0, cluster.vcov(lm0, B$X))
#Create data frame with confidence intervals
l_m_pl = data.frame(Parameter = rownames(lm_clus[-1,])) %>% #-1 drops intercept
  mutate(Coef = lm_clus[-1, 1]) %>%
  mutate(Lo = Coef - 1.96*lm_clus[-1,2]) %>%
  mutate(Hi = Coef + 1.96*lm_clus[-1,2]) %>%
  rbind(data.frame(Parameter = c('A_3','B_4'),
        Coef = c(0,0), Lo = c(0,0), Hi = c(0,0)) ) %>%
  mutate(Parameter = as.character(Parameter)) %>%
  arrange(Parameter)

###########################################################################################
#SUBGROUP ANALYSIS: do analysis and make plots

#function to make data frame from regression results
DF_C = function(l_m, id){
  l_m_pl = data.frame(Parameter = rownames(l_m[-1,])) %>% #-1 drops intercept
    mutate(Coef = l_m[-1,1]) %>%
    mutate(Lo = Coef - 1.96 * l_m[-1,2]) %>%
    mutate(Hi = Coef + 1.96 * l_m[-1,2]) %>%
    mutate(ID = id) %>%
    rbind(data.frame(Parameter = c('A_3','B_4'),
                     Coef = c(0,0), Lo = c(0,0), Hi = c(0,0), ID = c(id, id)) ) %>%
    mutate(Parameter = as.character(Parameter)) %>%
    arrange(Parameter)
}

#Make functions to extract p and z as a function of k
calc_p_relig = function(k, Dat){
  var <- paste0('Nearest',k,'_SameRel')
  if(median(Dat[,var],na.rm=T)==k){
    var_break <- k-1
  }else{var_break = floor(median(Dat[,var],na.rm=T))}
  dat_lo = Dat[ which(Dat[,var] <= var_break ) ,]
  dat_hi = Dat[ which(Dat[,var] > var_break ) ,]
  if(nrow(dat_lo) == 0 | nrow(dat_hi) == 0){p_a2 = NA
  }else{
    lm_lo = lm(form1, data = dat_lo )
    lm_hi = lm(form1, data = dat_hi )
    lm_clus_lo = coeftest(lm_lo, cluster.vcov(lm_lo, dat_lo[,c('X','A.A7')]))
    lm_clus_hi = coeftest(lm_hi, cluster.vcov(lm_hi, dat_hi[,c('X','A.A7')]))
    z_a2 = (lm_clus_lo[3,1] - lm_clus_hi[3,1]) / sqrt(lm_clus_lo[3,2]^2 + lm_clus_hi[3,2]^2)
    p_a2 = 2 * pnorm( abs(z_a2), mean = 0, sd = 1, lower.tail = F) 
    coef_lo = lm_clus_lo[3,1]; coef_hi = lm_clus_hi[3,1]
    sd_lo = lm_clus_lo[3,2]; sd_hi = lm_clus_hi[3,2]
    #return(p_a2) }
    return(data.frame(coef_lo, coef_hi, sd_lo, sd_hi, p_a2)) }
}

#Calculate for each k
relig_results = sapply(1:30, function(x) calc_p_relig(k=x, Dat = B)) %>% t() %>% data.frame()  %>%
  mutate(k = 1:30,
         dif = as.numeric(coef_lo) - as.numeric(coef_hi),
         dif_sd = sqrt(as.numeric(sd_lo)^2 + as.numeric(sd_hi)^2),
         dif_lobd = dif - 1.96*dif_sd,
         dif_hibd = dif + 1.96*dif_sd
  )

#Create data frame for plotting
rrc = 
  relig_results[seq(from = 2, to = 30, by = 3),] %>% 
  rename(Low = coef_lo, High = coef_hi) %>%
  select(-starts_with('dif')) %>% 
  gather(Seg, Coef, c(Low, High)) %>% mutate(Coef = as.numeric(Coef),
                                             sd_lo = as.numeric(sd_lo),
                                             sd_hi = as.numeric(sd_hi),
                                             p_a2 = as.numeric(p_a2),
                                             sd = sd_lo*(Seg == 'Low') + sd_hi*(Seg == 'High'),
                                             lo = Coef - 1.96*sd,
                                             hi = Coef + 1.96*sd,
                                             sig = factor(p_a2 < 0.05))

#Make plot, high vs low exposure
ggplot(data = rrc, aes(x = k, y = Coef)) + 
  geom_pointrange(aes(ymin = lo, ymax = hi, x = k, shape = Seg, alpha = sig), 
                  position = position_dodge(width = 0.9)) +
  scale_alpha_manual('p < 0.05', c(TRUE, FALSE), values=c(0.5, 1), labels = c('Yes','No')) +
  labs(shape = 'Segregation') +
  theme_minimal() + ylab('Coethnicity Coefficients') +
  theme(text = element_text(size = 16),
        plot.title = element_text(hjust = 0.5)) + 
  ggtitle('k-Nearest Own Religion') +
  guides(shape = guide_legend(order = 1), 
         alpha = guide_legend(order = 0))
#ggsave(filename = paste0(path0, 'k-z_coefficients_NETWORK.png'), height = 150, width = 150, units = 'mm'); rm(path0)
#This is result for paper, February 2019

#################################################################
ggplot(data = relig_results) + geom_point(aes(x = k, y = dif)) +
  geom_errorbar(aes(ymin = dif_lobd, ymax = dif_hibd, x = k)) + 
  theme_minimal() + theme(text = element_text(size = 18),
                          plot.title = element_text(hjust = 0.5)) + 
  ggtitle('k-Nearest Own Religion\nNetwork survey') + ylab('Difference in Coethnicity Coefficients,\nLow - High Seg.')
#ggsave(filename = paste0(path0, 'k-z_full_network.png'), height = 150, width = 150, units = 'mm'); rm(path0)
#This is result for paper

#6-4-18 add lines to put this in a table
relig_results_tab = relig_results %>% apply(2, function(x) as.numeric(x)) %>% data.frame() %>%
  select(k, coef_lo, sd_lo, coef_hi, sd_hi, p_a2) %>% 
  rename(Coef_LowSeg = coef_lo, SD_LowSeg = sd_lo, Coef_HiSeg = coef_hi, SD_HiSeg = sd_hi, p = p_a2) %>% 
  round(3)
out = stargazer(relig_results_tab, summary = F, rownames = F,
                title = 'Results for co-ethnicity attribute in candidate experiment compared between
                high- and low-segregation subsamples, based on religious segregation.',
                label = 'table:ReligResults_Network')
#writeLines(out,con = paste0(path0,'ReligResults_Network.tex'));rm(out, path0, relig_results_tab)

relig_results_hin = sapply(1:30, function(x) calc_p_relig(k=x, Dat = B[which(B$C.C6 == 'Hindu'),])) %>% t() %>% data.frame()  %>%
  mutate(k = 1:30,
         dif = as.numeric(coef_lo) - as.numeric(coef_hi),
         dif_sd = sqrt(as.numeric(sd_lo)^2 + as.numeric(sd_hi)^2),
         dif_lobd = dif - 1.96*dif_sd,
         dif_hibd = dif + 1.96*dif_sd
  )

ggplot(data = relig_results_hin) + geom_point(aes(x = k, y = dif)) +
  geom_errorbar(aes(ymin = dif_lobd, ymax = dif_hibd, x = k)) + 
  theme_minimal() + theme(text = element_text(size = 18),
                          plot.title = element_text(hjust = 0.5)) + 
  ggtitle('Nearest-k Own Religion (Hindu)\nNetwork survey') + ylab('Difference in Coethnicity Coefficients,\nLow - High Seg.')
#ggsave(filename = paste0(path0, 'k-z_hindu_network.png'), height = 150, width = 150, units = 'mm'); rm(path0)
#This is result for paper

relig_results_mus = sapply(1:30, function(x) calc_p_relig(k=x, Dat = B[which(B$C.C6 == 'Muslim'),])) %>% t() %>% data.frame()  %>%
  mutate(k = 1:30,
         dif = as.numeric(coef_lo) - as.numeric(coef_hi),
         dif_sd = sqrt(as.numeric(sd_lo)^2 + as.numeric(sd_hi)^2),
         dif_lobd = dif - 1.96*dif_sd,
         dif_hibd = dif + 1.96*dif_sd
  )

ggplot(data = relig_results_mus) + geom_point(aes(x = k, y = dif)) +
  geom_errorbar(aes(ymin = dif_lobd, ymax = dif_hibd, x = k)) + 
  theme_minimal() + theme(text = element_text(size = 18),
                          plot.title = element_text(hjust = 0.5)) + 
  ggtitle('Nearest-k Own Religion (Muslim)\nNetwork survey') + ylab('Difference in Coethnicity Coefficients,\nLow - High Seg.')
#ggsave(filename = paste0(path0, 'k-z_muslim_network.png'), height = 150, width = 150, units = 'mm'); rm(path0)
#This is result for paper


########################################################################################################################
#now de-medianed; based on 5-16-18_ApplyK.R

#calc_p_relig_demed = function(k, Dat){ 
calc_p_relig_demed_2 = function(k, Dat){ #version 2 adds a >= to dat_hi, so 0's are included in high-metric
  var <- paste0('DeMedNearest',k,'_SameRel')
  if(median(Dat[,var],na.rm=T)==k){
    var_break <- k-1
  }else{var_break = floor(median(Dat[,var],na.rm=T))}
  dat_lo = Dat[ which(Dat[,var] < var_break ) ,]
#  dat_hi = Dat[ which(Dat[,var] > var_break ) ,]
  dat_hi = Dat[ which(Dat[,var] >= var_break ) ,]
  #dat_lo = B[which(B$Nearest10_SameRel <= var_break ) , ]
  #dat_hi = B[which(B$Nearest10_SameRel > var_break ) , ]
  if(nrow(dat_lo) == 0 | nrow(dat_hi) == 0){p_a2 = NA
  }else{
    lm_lo = lm(form1, data = dat_lo )
    lm_hi = lm(form1, data = dat_hi )
    lm_clus_lo = coeftest(lm_lo, cluster.vcov(lm_lo, dat_lo[,c('X','A.A7')]))
    lm_clus_hi = coeftest(lm_hi, cluster.vcov(lm_hi, dat_hi[,c('X','A.A7')]))
    z_a2 = (lm_clus_lo[3,1] - lm_clus_hi[3,1]) / sqrt(lm_clus_lo[3,2]^2 + lm_clus_hi[3,2]^2)
    p_a2 = 2 * pnorm( abs(z_a2), mean = 0, sd = 1, lower.tail = F) 
    coef_lo = lm_clus_lo[3,1]; coef_hi = lm_clus_hi[3,1]
    sd_lo = lm_clus_lo[3,2]; sd_hi = lm_clus_hi[3,2]
    #return(p_a2) }
    return(data.frame(coef_lo, coef_hi, sd_lo, sd_hi, p_a2)) }
}

#whole sample
#relig_results_demed = sapply(c(5,10,15,20,25,30), function(x) calc_p_relig_demed(k=x, Dat = B)) %>% t() %>% data.frame()  %>%
relig_results_demed = sapply(c(5,10,15,20,25,30), function(x) calc_p_relig_demed_2(k=x, Dat = B)) %>% t() %>% data.frame()  %>%
  mutate(k = c(5,10,15,20,25,30),
         dif = as.numeric(coef_lo) - as.numeric(coef_hi),
         dif_sd = sqrt(as.numeric(sd_lo)^2 + as.numeric(sd_hi)^2),
         dif_lobd = dif - 1.96*dif_sd,
         dif_hibd = dif + 1.96*dif_sd,
         id = 'demed'
  )
relig_results = sapply(c(5,10,15,20,25,30), function(x) calc_p_relig(k=x, Dat = B)) %>% t() %>% data.frame()  %>%
  mutate(k = c(5,10,15,20,25,30),
         dif = as.numeric(coef_lo) - as.numeric(coef_hi),
         dif_sd = sqrt(as.numeric(sd_lo)^2 + as.numeric(sd_hi)^2),
         dif_lobd = dif - 1.96*dif_sd,
         dif_hibd = dif + 1.96*dif_sd,
         id = 'non-demed'
  )
out = rbind(relig_results_demed, relig_results)

ggplot(data = out, group = id, aes(x = k)) + geom_point(aes(y = dif, color = id), position=position_dodge( width = 2.5 )) +
  geom_errorbar(aes(ymin = dif_lobd, ymax = dif_hibd, group = id), position=position_dodge( width = 2.5 )) + 
  scale_color_discrete(name='Metric', labels=c('De-Med. K-Nearest', 'K-Nearest')) +
  theme_minimal() + theme(text = element_text(size = 18),
                          plot.title = element_text(hjust = 0.5)) + 
  ggtitle('k-Nearest Own Religion\nNetwork survey') + ylab('Difference in Coethnicity Coefficients,\nLow - High Seg.')
#ggsave(filename = paste0(path0, 'k-z_full_network_demed_incl0.png'), height = 150, width = 150, units = 'mm'); rm(path0)
#This is result for paper

#just hindus
#relig_results_demed_hindu = sapply(c(5,10,15,20,25,30), function(x) calc_p_relig_demed(k=x, Dat = B[which(B$C.C6 == 'Hindu'),])) %>% 
relig_results_demed_hindu = sapply(c(5,10,15,20,25,30), function(x) calc_p_relig_demed_2(k=x, Dat = B[which(B$C.C6 == 'Hindu'),])) %>% 
  t() %>% data.frame()  %>%
  mutate(k = c(5,10,15,20,25,30),
         dif = as.numeric(coef_lo) - as.numeric(coef_hi),
         dif_sd = sqrt(as.numeric(sd_lo)^2 + as.numeric(sd_hi)^2),
         dif_lobd = dif - 1.96*dif_sd,
         dif_hibd = dif + 1.96*dif_sd,
         id = 'demed'
  )
relig_results_hindu = sapply(c(5,10,15,20,25,30), function(x) calc_p_relig(k=x, Dat = B[which(B$C.C6 == 'Hindu'),])) %>% t() %>% data.frame()  %>%
  mutate(k = c(5,10,15,20,25,30),
         dif = as.numeric(coef_lo) - as.numeric(coef_hi),
         dif_sd = sqrt(as.numeric(sd_lo)^2 + as.numeric(sd_hi)^2),
         dif_lobd = dif - 1.96*dif_sd,
         dif_hibd = dif + 1.96*dif_sd,
         id = 'non-demed'
  )
out = rbind(relig_results_demed_hindu, relig_results_hindu)

ggplot(data = out, group = id, aes(x = k)) + geom_point(aes(y = dif, color = id), position=position_dodge( width = 2.5 )) +
  geom_errorbar(aes(ymin = dif_lobd, ymax = dif_hibd, group = id), position=position_dodge( width = 2.5 )) + 
  scale_color_discrete(name='Metric', labels=c('De-Med. K-Nearest', 'K-Nearest')) +
  theme_minimal() + theme(text = element_text(size = 18),
                          plot.title = element_text(hjust = 0.5)) + 
  ggtitle('k-Nearest Own Religion\nNetwork survey (Hindus)') + ylab('Difference in Coethnicity Coefficients,\nLow - High Seg.')
#ggsave(filename = paste0(path0, 'k-z_hindu_network_demed_incl0.png'), height = 150, width = 150, units = 'mm'); rm(path0)
#This is result for paper
########################################################################################################################


########################################################################################################################
#Add additional k-z plots for one side of unbalanced variables: first non-de-medianed:
#income, low caste, male, migrant, jaipur

#Income: median is 10k
relig_results_rich = sapply(1:30, function(x) calc_p_relig(k=x, Dat = B[which(B$Income1 >= 10),])) %>% t() %>% data.frame()  %>%
  mutate(k = 1:30,
         dif = as.numeric(coef_lo) - as.numeric(coef_hi),
         dif_sd = sqrt(as.numeric(sd_lo)^2 + as.numeric(sd_hi)^2),
         dif_lobd = dif - 1.96*dif_sd,
         dif_hibd = dif + 1.96*dif_sd
  )

ggplot(data = relig_results_rich) + geom_point(aes(x = k, y = dif)) +
  geom_errorbar(aes(ymin = dif_lobd, ymax = dif_hibd, x = k)) + 
  theme_minimal() + theme(text = element_text(size = 18),
                          plot.title = element_text(hjust = 0.5)) + 
  ggtitle('Nearest-k Own Religion\n(High Income), Network survey') + ylab('Difference in Coethnicity Coefficients,\nLow - High Seg.')
#ggsave(filename = paste0(path0, 'k-z_rich_network.png'), height = 150, width = 150, units = 'mm'); rm(path0)
#This is result for paper

#High Caste
relig_results_hicaste = sapply(1:30, function(x) calc_p_relig(k=x, Dat = B[which(B$LowCaste == FALSE),])) %>% t() %>% data.frame()  %>%
  mutate(k = 1:30,
         dif = as.numeric(coef_lo) - as.numeric(coef_hi),
         dif_sd = sqrt(as.numeric(sd_lo)^2 + as.numeric(sd_hi)^2),
         dif_lobd = dif - 1.96*dif_sd,
         dif_hibd = dif + 1.96*dif_sd
  )

ggplot(data = relig_results_hicaste) + geom_point(aes(x = k, y = dif)) +
  geom_errorbar(aes(ymin = dif_lobd, ymax = dif_hibd, x = k)) + 
  theme_minimal() + theme(text = element_text(size = 18),
                          plot.title = element_text(hjust = 0.5)) + 
  ggtitle('Nearest-k Own Religion (High Caste)\nNetwork survey') + ylab('Difference in Coethnicity Coefficients,\nLow - High Seg.')
#ggsave(filename = paste0(path0, 'k-z_hicaste_network.png'), height = 150, width = 150, units = 'mm'); rm(path0)
#This is result for paper

#Female
relig_results_female = sapply(1:30, function(x) calc_p_relig(k=x, Dat = B[which(B$Male == FALSE),])) %>% t() %>% data.frame()  %>%
  mutate(k = 1:30,
         dif = as.numeric(coef_lo) - as.numeric(coef_hi),
         dif_sd = sqrt(as.numeric(sd_lo)^2 + as.numeric(sd_hi)^2),
         dif_lobd = dif - 1.96*dif_sd,
         dif_hibd = dif + 1.96*dif_sd
  )

ggplot(data = relig_results_female) + geom_point(aes(x = k, y = dif)) +
  geom_errorbar(aes(ymin = dif_lobd, ymax = dif_hibd, x = k)) + 
  theme_minimal() + theme(text = element_text(size = 18),
                          plot.title = element_text(hjust = 0.5)) + 
  ggtitle('Nearest-k Own Religion (Female)\nNetwork survey') + ylab('Difference in Coethnicity Coefficients,\nLow - High Seg.')
#ggsave(filename = paste0(path0, 'k-z_female_network.png'), height = 150, width = 150, units = 'mm'); rm(path0)
#This is result for paper

#Migrant
relig_results_migrant = sapply(1:30, function(x) calc_p_relig(k=x, Dat = B[which(B$Migrant == TRUE),])) %>% t() %>% data.frame()  %>%
  mutate(k = 1:30,
         dif = as.numeric(coef_lo) - as.numeric(coef_hi),
         dif_sd = sqrt(as.numeric(sd_lo)^2 + as.numeric(sd_hi)^2),
         dif_lobd = dif - 1.96*dif_sd,
         dif_hibd = dif + 1.96*dif_sd
  )

ggplot(data = relig_results_migrant) + geom_point(aes(x = k, y = dif)) +
  geom_errorbar(aes(ymin = dif_lobd, ymax = dif_hibd, x = k)) + 
  theme_minimal() + theme(text = element_text(size = 18),
                          plot.title = element_text(hjust = 0.5)) + 
  ggtitle('Nearest-k Own Religion (Migrant)\nNetwork survey') + ylab('Difference in Coethnicity Coefficients,\nLow - High Seg.')
#ggsave(filename = paste0(path0, 'k-z_migrant_network.png'), height = 150, width = 150, units = 'mm'); rm(path0)
#This is result for paper

#Patna
relig_results_patna = sapply(1:30, function(x) calc_p_relig(k=x, Dat = B[which(B$City == 'Patna'),])) %>% t() %>% data.frame()  %>%
  mutate(k = 1:30,
         dif = as.numeric(coef_lo) - as.numeric(coef_hi),
         dif_sd = sqrt(as.numeric(sd_lo)^2 + as.numeric(sd_hi)^2),
         dif_lobd = dif - 1.96*dif_sd,
         dif_hibd = dif + 1.96*dif_sd
  )

ggplot(data = relig_results_patna) + geom_point(aes(x = k, y = dif)) +
  geom_errorbar(aes(ymin = dif_lobd, ymax = dif_hibd, x = k)) + 
  theme_minimal() + theme(text = element_text(size = 18),
                          plot.title = element_text(hjust = 0.5)) + 
  ggtitle('Nearest-k Own Religion (Patna)\nNetwork survey') + ylab('Difference in Coethnicity Coefficients,\nLow - High Seg.')
#ggsave(filename = paste0(path0, 'k-z_patna_network.png'), height = 150, width = 150, units = 'mm'); rm(path0)
#This is result for paper
########################################################################################################################

########################################################################################################################
#Add additional k-z plots for one side of unbalanced variables: first non-de-medianed:
#income, low caste, jaipur

relig_results_demed_rich = sapply(c(5,10,15,20,25,30), function(x) calc_p_relig_demed_2(k=x, Dat = B[which(B$Income1 >= 10),])) %>% 
  t() %>% data.frame()  %>%
  mutate(k = c(5,10,15,20,25,30),
         dif = as.numeric(coef_lo) - as.numeric(coef_hi),
         dif_sd = sqrt(as.numeric(sd_lo)^2 + as.numeric(sd_hi)^2),
         dif_lobd = dif - 1.96*dif_sd,
         dif_hibd = dif + 1.96*dif_sd,
         id = 'demed'
  )

ggplot(data = relig_results_demed_rich) + geom_point(aes(x = k, y = dif)) +
  geom_errorbar(aes(ymin = dif_lobd, ymax = dif_hibd, x = k)) + 
  theme_minimal() + theme(text = element_text(size = 18),
                          plot.title = element_text(hjust = 0.5)) + 
  ggtitle('De-med. Nearest-k Own Religion\n(High Income), Network survey') + ylab('Difference in Coethnicity Coefficients,\nLow - High Seg.')
#ggsave(filename = paste0(path0, 'k-z_rich_network_demed.png'), height = 150, width = 150, units = 'mm'); rm(path0)
#This is result for paper


relig_results_demed_hicaste = sapply(c(5,10,15,20,25,30), function(x) calc_p_relig_demed_2(k=x, Dat = B[which(B$LowCaste == FALSE),])) %>% 
  t() %>% data.frame()  %>%
  mutate(k = c(5,10,15,20,25,30),
         dif = as.numeric(coef_lo) - as.numeric(coef_hi),
         dif_sd = sqrt(as.numeric(sd_lo)^2 + as.numeric(sd_hi)^2),
         dif_lobd = dif - 1.96*dif_sd,
         dif_hibd = dif + 1.96*dif_sd,
         id = 'demed'
  )

ggplot(data = relig_results_demed_hicaste) + geom_point(aes(x = k, y = dif)) +
  geom_errorbar(aes(ymin = dif_lobd, ymax = dif_hibd, x = k)) + 
  theme_minimal() + theme(text = element_text(size = 18),
                          plot.title = element_text(hjust = 0.5)) + 
  ggtitle('De-med. Nearest-k Own Religion\n(High Caste), Network survey') + ylab('Difference in Coethnicity Coefficients,\nLow - High Seg.')
#ggsave(filename = paste0(path0, 'k-z_hicaste_network_demed.png'), height = 150, width = 150, units = 'mm'); rm(path0)
#This is result for paper

relig_results_demed_patna = sapply(c(5,10,15,20,25,30), function(x) calc_p_relig_demed_2(k=x, Dat = B[which(B$City == 'Patna'),])) %>% 
  t() %>% data.frame()  %>%
  mutate(k = c(5,10,15,20,25,30),
         dif = as.numeric(coef_lo) - as.numeric(coef_hi),
         dif_sd = sqrt(as.numeric(sd_lo)^2 + as.numeric(sd_hi)^2),
         dif_lobd = dif - 1.96*dif_sd,
         dif_hibd = dif + 1.96*dif_sd,
         id = 'demed'
  )

ggplot(data = relig_results_demed_patna) + geom_point(aes(x = k, y = dif)) +
  geom_errorbar(aes(ymin = dif_lobd, ymax = dif_hibd, x = k)) + 
  theme_minimal() + theme(text = element_text(size = 18),
                          plot.title = element_text(hjust = 0.5)) + 
  ggtitle('De-med. Nearest-k Own Religion\n(Patna), Network survey') + ylab('Difference in Coethnicity Coefficients,\nLow - High Seg.')
#ggsave(filename = paste0(path0, 'k-z_patna_network_demed.png'), height = 150, width = 150, units = 'mm'); rm(path0)
#This is result for paper
########################################################################################################################