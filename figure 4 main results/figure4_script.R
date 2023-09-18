#figure 4: Coethnic voting preferences in candidate conjoint experiment, 
#comparing high- to low-exposure respondents. 
#This also includes candidate-comparison results for Appendix

rm(list=ls())
setwd('/Users/jeremyspater/Dropbox/duke/political economy core/prospectus/india part/2018_version/replication_files/scripts and data/figure 4 main results')

s = function(x){summary(factor(x))}
Num = function(x){as.numeric(as.factor(x))}

library(plyr);library(dplyr, warn.conflicts = FALSE)
library(tidyr);library(ggplot2)
suppressMessages(library(multiwayvcov, warn.conflicts = F))
suppressMessages(library(lmtest, warn.conflicts = F))
library(stargazer)

A = readRDS('5-16-18_HH-KNearest_DeID_demed.RDS')

#Data cleaning
Aprime = A[A$A.A7_Area.Neighborhood %in% names(which(table(A$A.A7_Area.Neighborhood) >= 30)),]#DROP PLACES WITH <30 OBSERVATIONS; down to 6394 observations
Aprime = Aprime[which(!is.na(Aprime$A.A7_Area.Neighborhood)),] #drop NA neighborhoods, which unfortunately have been assigned k-nearest values . . . .
Aprime = Aprime[which(Aprime$A.A7_Area.Neighborhood != 'Bangalore NA'),]

Q = Aprime

###################################################################################################
#Conjoint analysis
#Rename variables
Q = Q %>% dplyr::rename(Q1A1 = L.Candidate_Random_A1_Candidate.Preferance, #first question, candidate 1, characteristic A
                        Q1A2 = L.Candidate_Random_A2_Candidate.Preferance, #first question, candidate 2, characteristic A
                        Q1B1 = L.Candidate_Random_B1_Candidate.Preferance, #first question, candidate 1, characteristic B
                        Q1B2 = L.Candidate_Random_B2_Candidate.Preferance, #first question, candidate 2, characteristic B
                        Q2A1 = L.Candidate_Random_A3, #second question, candidate 1, characteristic A
                        Q2A2 = L.Candidate_Random_A4, #second question, candidate 2, characteristic A
                        Q2B1 = L.Candidate_Random_B3, #second question, candidate 1, characteristic B
                        Q2B2 = L.Candidate_Random_B4, #second question, candidate 2, characteristic B
                        Q3A1 = L.Candidate_Random_A5, #third question, candidate 1, characteristic A
                        Q3A2 = L.Candidate_Random_A6, #third question, candidate 2, characteristic A
                        Q3B1 = L.Candidate_Random_B5, #third question, candidate 1, characteristic B
                        Q3B2 = L.Candidate_Random_B6, #third question, candidate 2, characteristic B
                        Q1 = L.Candidate_Question_1, #first question, choose candidate 1 or 2
                        Q2 = L.Candidate_Question_2, #first question, choose candidate 1 or 2
                        Q3 = L.Candidate_Question_3 #first question, choose candidate 1 or 2
) 

#rearrange so one row is one conjoint observation. 3x as many rows as A
#new variables: A1, B1 are two traits for candidate 1; similar for 2; and y is responent's choice between candidates
B = Q %>% unite('Q1', matches('Q1')) %>% unite('Q2', matches('Q2')) %>% unite('Q3', matches('Q3')) %>%
  gather(Question, b, starts_with('Q')) %>% arrange(X) %>% separate( 'b', c('A1','A2','B1','B2','y') )

B = B %>% filter(! (A1 == A2 & B1 == B2)) #drop observations where candidates have same profile

#make new data frame wheer each row is one PROFILE, ie each question becomes two rows (one for each candidate)
#New variables: A1, A2 are combined as A: trait A for either candidate
C = B %>% unite('AB1',c(A1,B1)) %>% unite('AB2',c(A2,B2)) %>% gather(Neighbor, AB, c(AB1, AB2)) %>%
  arrange(X) %>% separate('AB', c('A','B')) %>% 
  mutate(Neighbor = as.numeric(mapvalues(Neighbor, from = c('AB1', 'AB2'), to = c(1,2))))

#function to make dummies for trait levels
ModFn = function(x,f){
  data.frame(x, model.matrix(as.formula(f), data=x)) }

C = C %>% ModFn('~ A - 1') %>% ModFn('~ B - 1') #function to make dummies for trait levels
C = C %>% mutate(Y = y == Neighbor) #1 when that candidate is picked
C = C %>% dplyr::rename(A_0 = A0, #rename variables to be consistent with earlier version of code
                        A_1 = A1,
                        A_2 = A2,
                        A_3 = A3,
                        B_0 = B0,
                        B_1 = B1,
                        B_2 = B2,
                        B_3 = B3,
                        B_4 = B4)

B = C; rm(C) #rename
###########################################################################################

###########################################################################################
#Setup for subgroup analysis

#regression formula
form1 = as.formula(paste0('Y ~ ',
                          paste(strsplit('A_0 A_1 A_2 A_3 B_0 B_1 B_2 B_3 B_4',
                                         split = ' ')[[1]], collapse = ' + ' )))

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
###########################################################################################
 
###########################################################################################
#Make function to extract p- and z-values for each choice of k

calc_p_relig = function(k, Dat){
  var <- paste0('Nearest',k,'_OwnReligion')
  if(median(Dat[,var],na.rm=T)==k){
    var_break <- k-1
  }else{var_break = floor(median(Dat[,var],na.rm=T))}
  dat_lo = Dat[ which(Dat[,var] <= var_break ) ,]
  dat_hi = Dat[ which(Dat[,var] > var_break ) ,]
  if(nrow(dat_lo) == 0 | nrow(dat_hi) == 0){p_a2 = NA
  }else{
  lm_lo = lm(form1, data = dat_lo )
  lm_hi = lm(form1, data = dat_hi )
  lm_clus_lo = coeftest(lm_lo, cluster.vcov(lm_lo, dat_lo[,c('X','A.A7_Area.Neighborhood')]))
  lm_clus_hi = coeftest(lm_hi, cluster.vcov(lm_hi, dat_hi[,c('X','A.A7_Area.Neighborhood')]))
  z_a2 = (lm_clus_lo['A_2','Estimate'] - lm_clus_hi['A_2','Estimate']) / sqrt(lm_clus_lo['A_2','Std. Error']^2 + lm_clus_hi['A_2','Std. Error']^2)
  p_a2 = 2 * pnorm( abs(z_a2), mean = 0, sd = 1, lower.tail = F) 
  coef_lo = lm_clus_lo['A_2','Estimate']; coef_hi = lm_clus_hi['A_2','Estimate']
  sd_lo = lm_clus_lo['A_2','Std. Error']; sd_hi = lm_clus_hi['A_2','Std. Error']
  return(data.frame(coef_lo, coef_hi, sd_lo, sd_hi, p_a2)) }
  }

#Calculate results for k in 1:30
relig_results = sapply(1:30, function(x) calc_p_relig(k=x, Dat = B)) %>% t() %>% data.frame()  %>%
  mutate(k = 1:30,
         dif = as.numeric(coef_lo) - as.numeric(coef_hi),
         dif_sd = sqrt(as.numeric(sd_lo)^2 + as.numeric(sd_hi)^2),
         dif_lobd = dif - 1.96*dif_sd,
         dif_hibd = dif + 1.96*dif_sd
         )

#Create data frame for plotting
rrc = relig_results[seq(from = 2, to = 30, by = 3),] %>% 
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

#################################################################
#Figure 4: main result
ggplot(data = rrc, aes(x = k, y = Coef)) + 
  geom_pointrange(aes(ymin = lo, ymax = hi, x = k, shape = Seg, alpha = sig), 
                  position = position_dodge(width = 0.9)) +
  scale_shape_manual('Exposure', values = c(16, 17), labels = c('Low','High')) + 
  scale_alpha_manual('p < 0.05', c(TRUE, FALSE), values=c(0.5, 1), labels = c('Yes','No')) +
  labs(shape = 'Exposure') + 
  theme_minimal() + ylab('Coethnicity Coefficients') +
  theme(text = element_text(size = 16),
        plot.title = element_text(hjust = 0.5)) + 
  ggtitle('k-Nearest Own Religion') +
  guides(shape = guide_legend(order = 1), 
         alpha = guide_legend(order = 0))

path0 = paste0(getwd(), '/', Sys.Date(),'/')
dir.create(path0)
#ggsave(filename = paste0(path0, 'Fig_4_k-z_coefficients.png'), height = 150, width = 150, units = 'mm')

#################################################################

#################################################################
#Main result in table form

relig_results_tab = relig_results %>% apply(2, function(x) as.numeric(x)) %>% data.frame() %>%
  select(k, coef_lo, sd_lo, coef_hi, sd_hi, p_a2) %>% 
  rename(Coef_LowSeg = coef_lo, SD_LowSeg = sd_lo, Coef_HiSeg = coef_hi, SD_HiSeg = sd_hi, p = p_a2) %>% 
  round(3)
out = stargazer(relig_results_tab, summary = F, rownames = F,
                title = 'Results for co-ethnicity attribute in candidate experiment compared between
                high- and low-segregation subsamples, based on religious segregation.',
                label = 'table:ReligResults')
#writeLines(out,con = paste0(path0,'ReligResults.tex'));rm(out, path0, relig_results_tab)

#################################################################################################################################################

#################################################################################################################################################
#De-medianed version

zpk_relig = function(data, Split, k){ # nearest k same religion
  var = paste0('Nearest',k,'_OwnReligion')
  VarLo = Split# - 1 
  VarHi = Split# - 1
  lm_lo = lm( form1, data = data[which(data[,var] < VarLo),] ) 
  lm_hi = lm( form1, data = data[which(data[,var] >= VarHi),] )
  lm_lo_sum = coeftest(lm_lo, cluster.vcov(lm_lo, data[which(data[,var] < VarLo),c('X','A.A7_Area.Neighborhood')] ) )
  lm_hi_sum = coeftest(lm_hi, cluster.vcov(lm_hi, data[which(data[,var] >= VarHi),c('X','A.A7_Area.Neighborhood')] ) )
  diff = lm_lo_sum['A_2','Estimate'] - lm_hi_sum['A_2','Estimate']
  z = (lm_lo_sum['A_2','Estimate'] - lm_hi_sum['A_2','Estimate']) / sqrt(lm_lo_sum['A_2','Std. Error']^2 + lm_hi_sum['A_2','Std. Error']^2)
  p = (2*pnorm(abs(z), mean = 0, sd = 1, lower.tail = FALSE))
  return(data.frame(diff = diff,z=z,p=p,split=Split, k = k))
}
zpk_demed_relig = function(data, Split, k){ # 
  var = paste0('DeMedNearest',k,'_OwnReligion')
  VarLo = Split
  VarHi = Split
  lm_lo = lm( form1, data = data[which(data[,var] < VarLo),] ) 
  lm_hi = lm( form1, data = data[which(data[,var] >= VarHi),] ) #
  lm_lo_sum = coeftest(lm_lo, cluster.vcov(lm_lo, data[which(data[,var] < VarLo),c('X','A.A7_Area.Neighborhood')] ) )
  lm_hi_sum = coeftest(lm_hi, cluster.vcov(lm_hi, data[which(data[,var] >= VarHi),c('X','A.A7_Area.Neighborhood')] ) ) #
  diff = lm_lo_sum['A_2','Estimate'] - lm_hi_sum['A_2','Estimate']
  z = (lm_lo_sum['A_2','Estimate'] - lm_hi_sum['A_2','Estimate']) / sqrt(lm_lo_sum['A_2','Std. Error']^2 + lm_hi_sum['A_2','Std. Error']^2)
  p = (2*pnorm(abs(z), mean = 0, sd = 1, lower.tail = FALSE))
  return(data.frame(diff = diff,z=z,p=p,split=Split, k = k))
}

varlist = as.list(seq(5,30,5))
NR_out = ldply(varlist, function(x) zpk_relig(data = B %>% filter(Wave %in% c( 'Bangalore 2016','Jai-Pat 2015') ),
                                              Split = x,
                                              k = as.character(x)) )

NR_out_demed = ldply(varlist, function(x) zpk_demed_relig(data = B %>% filter(Wave %in% c( 'Bangalore 2016','Jai-Pat 2015') ),
                                                          Split = 0,
                                                          k = as.character(x)) )

NR_out$se =  NR_out$diff / NR_out$z
NR_out$lo = NR_out$diff - 1.96*NR_out$se
NR_out$hi = NR_out$diff + 1.96*NR_out$se
NR_out$id = 'k-Nearest Neighbors'

NR_out_demed$se =  NR_out_demed$diff / NR_out_demed$z
NR_out_demed$lo = NR_out_demed$diff - 1.96*NR_out_demed$se
NR_out_demed$hi = NR_out_demed$diff + 1.96*NR_out_demed$se
NR_out_demed$id = 'De-Medianed k-Nearest Neighbors'

out = rbind(NR_out, NR_out_demed)

#FIGURE A4
ggplot(out, group = id, aes(x=k)) +
  geom_point(aes(y = diff, color = id), position=position_dodge( width = 0.5 )) +
  geom_errorbar(aes(ymin = lo, ymax = hi, group = id), position=position_dodge( width = 0.5 )) +
  labs(x = 'k', y = 'Diff. of Coefficients (Lo - Hi Seg)') +
  scale_color_discrete(name='Metric', labels=c('De-Med. K-Nearest', 'K-Nearest')) +
  theme_minimal() + theme(text=element_text(size=16)) + theme(legend.title.align=0.5)
#ggsave(filename = paste0(path0, 'demed-k-z-full.png'), height = 150, width = 150, units = 'mm')
#################################################################################################################################################

#################################################################################################################################################
#Plots for non-balanced attributes

relig_results_hin = sapply(1:30, function(x) calc_p_relig(k=x, Dat = B[which(B$C.C6_Religion == 'Hindu'),])) %>% t() %>% data.frame()  %>%
  mutate(k = 1:30,
         dif = as.numeric(coef_lo) - as.numeric(coef_hi),
         dif_sd = sqrt(as.numeric(sd_lo)^2 + as.numeric(sd_hi)^2),
         dif_lobd = dif - 1.96*dif_sd,
         dif_hibd = dif + 1.96*dif_sd
  )

#FIGURE A6
ggplot(data = relig_results_hin) + geom_point(aes(x = k, y = dif)) +
  geom_errorbar(aes(ymin = dif_lobd, ymax = dif_hibd, x = k)) + 
  theme_minimal() + theme(text = element_text(size = 18),
                          plot.title = element_text(hjust = 0.5)) + 
  ggtitle('Nearest-k Own Religion (Hindu)') + ylab('Difference in Coethnicity Coefficients,\nLow - High Seg.')
#ggsave(filename = paste0(path0, 'k-z_hindu.png'), height = 150, width = 150, units = 'mm')

relig_results_mus = sapply(1:30, function(x) calc_p_relig(k=x, Dat = B[which(B$C.C6_Religion == 'Muslim'),])) %>% t() %>% data.frame()  %>%
  mutate(k = 1:30,
         dif = as.numeric(coef_lo) - as.numeric(coef_hi),
         dif_sd = sqrt(as.numeric(sd_lo)^2 + as.numeric(sd_hi)^2),
         dif_lobd = dif - 1.96*dif_sd,
         dif_hibd = dif + 1.96*dif_sd
  )

#FIGURE A7
ggplot(data = relig_results_mus) + geom_point(aes(x = k, y = dif)) +
  geom_errorbar(aes(ymin = dif_lobd, ymax = dif_hibd, x = k))  + 
  theme_minimal() + theme(text = element_text(size = 18), plot.title = element_text(hjust = 0.5)) + 
  ggtitle('Nearest-k Own Religion (Muslim)') + ylab('Difference in Coethnicity Coefficients,\nLow - High Seg.')
#ggsave(filename = paste0(path0, 'k-z_muslim.png'), height = 150, width = 150, units = 'mm')

relig_results_hiassets = sapply(1:30, function(x) calc_p_relig(k=x, Dat = B[which(B$AssetSum >= 10),])) %>% t() %>% data.frame()  %>%
  mutate(k = 1:30,
         dif = as.numeric(coef_lo) - as.numeric(coef_hi),
         dif_sd = sqrt(as.numeric(sd_lo)^2 + as.numeric(sd_hi)^2),
         dif_lobd = dif - 1.96*dif_sd,
         dif_hibd = dif + 1.96*dif_sd
  )

#FIGURE A8
ggplot(data = relig_results_hin) + geom_point(aes(x = k, y = dif)) +
  geom_errorbar(aes(ymin = dif_lobd, ymax = dif_hibd, x = k)) + 
  theme_minimal() + theme(text = element_text(size = 18),
                          plot.title = element_text(hjust = 0.5)) + 
  ggtitle('Nearest-k Own Religion (Hi Assets)') + ylab('Difference in Coethnicity Coefficients,\nLow - High Seg.')
#ggsave(filename = paste0(path0, 'k-z_hiassets.png'), height = 150, width = 150, units = 'mm')

relig_results_hicaste = sapply(1:30, function(x) calc_p_relig(k=x, Dat = B[which(B$C.C8_Caste == 'General/BC/OBC'),])) %>% t() %>% data.frame()  %>%
  mutate(k = 1:30,
         dif = as.numeric(coef_lo) - as.numeric(coef_hi),
         dif_sd = sqrt(as.numeric(sd_lo)^2 + as.numeric(sd_hi)^2),
         dif_lobd = dif - 1.96*dif_sd,
         dif_hibd = dif + 1.96*dif_sd
  )

#FIGURE A9
ggplot(data = relig_results_hicaste) + geom_point(aes(x = k, y = dif)) +
  geom_errorbar(aes(ymin = dif_lobd, ymax = dif_hibd, x = k)) + 
  theme_minimal() + theme(text = element_text(size = 18),
                          plot.title = element_text(hjust = 0.5)) + 
  ggtitle('Nearest-k Own Religion (Hi Caste)') + ylab('Difference in Coethnicity Coefficients,\nLow - High Seg.')
#ggsave(filename = paste0(path0, 'k-z_hicaste.png'), height = 150, width = 150, units = 'mm')

relig_results_jaipur = sapply(1:30, function(x) calc_p_relig(k=x, Dat = B[which(B$City == 'Jaipur'),])) %>% t() %>% data.frame()  %>%
  mutate(k = 1:30,
         dif = as.numeric(coef_lo) - as.numeric(coef_hi),
         dif_sd = sqrt(as.numeric(sd_lo)^2 + as.numeric(sd_hi)^2),
         dif_lobd = dif - 1.96*dif_sd,
         dif_hibd = dif + 1.96*dif_sd
  )

#FIGURE A10
ggplot(data = relig_results_jaipur) + geom_point(aes(x = k, y = dif)) +
  geom_errorbar(aes(ymin = dif_lobd, ymax = dif_hibd, x = k)) + 
  theme_minimal() + theme(text = element_text(size = 18),
                          plot.title = element_text(hjust = 0.5)) + 
  ggtitle('Nearest-k Own Religion (Jaipur)') + ylab('Difference in Coethnicity Coefficients,\nLow - High Seg.')
#ggsave(filename = paste0(path0, 'k-z_jaipur.png'), height = 150, width = 150, units = 'mm')

relig_results_patna = sapply(1:30, function(x) calc_p_relig(k=x, Dat = B[which(B$City == 'Patna'),])) %>% t() %>% data.frame()  %>%
  mutate(k = 1:30,
         dif = as.numeric(coef_lo) - as.numeric(coef_hi),
         dif_sd = sqrt(as.numeric(sd_lo)^2 + as.numeric(sd_hi)^2),
         dif_lobd = dif - 1.96*dif_sd,
         dif_hibd = dif + 1.96*dif_sd
  )

#FIGURE A11
ggplot(data = relig_results_patna) + geom_point(aes(x = k, y = dif)) +
  geom_errorbar(aes(ymin = dif_lobd, ymax = dif_hibd, x = k)) + 
  theme_minimal() + theme(text = element_text(size = 18),
                          plot.title = element_text(hjust = 0.5)) + 
  ggtitle('Nearest-k Own Religion (Patna)') + ylab('Difference in Coethnicity Coefficients,\nLow - High Seg.')
#ggsave(filename = paste0(path0, 'k-z_patna.png'), height = 150, width = 150, units = 'mm')

############################################################################################
