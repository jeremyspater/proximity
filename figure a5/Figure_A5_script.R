#Figure A5, results excluding Bangalore

rm(list=ls())

s = function(x){summary(factor(x))}
Num = function(x){as.numeric(as.factor(x))}

library(plyr);library(dplyr, warn.conflicts = FALSE)
library(tidyr);library(ggplot2)
suppressMessages(library(multiwayvcov, warn.conflicts = F))
suppressMessages(library(lmtest, warn.conflicts = F))

A = readRDS('/Users/jeremyspater/Dropbox/duke/wibbels RA/segregation paper/combine bang jai patna/may_11_2018_js_export/5-16-18_HH-KNearest_DeID_demed.RDS')

Aprime = A[A$A.A7_Area.Neighborhood %in% names(which(table(A$A.A7_Area.Neighborhood) >= 30)),]#DROP PLACES WITH <30 OBSERVATIONS
Aprime = Aprime[which(!is.na(Aprime$A.A7_Area.Neighborhood)),] #drop NA neighborhoods
Aprime = Aprime[which(Aprime$A.A7_Area.Neighborhood != 'Bangalore NA'),]

Q = Aprime

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

B = B %>% filter(! (A1 == A2 & B1 == B2)) #7767 -> 7346; drop observations where candidates have same profile

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

B = C; rm(C) #rename to be consistent with earlier version
###########################################################################################

###########################################################################################
#SUBGROUP ANALYSIS: Religion 10

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
#Make functions to extract p and z as a function of k

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

relig_results = sapply(1:30, function(x) calc_p_relig(k=x, Dat = B)) %>% t() %>% data.frame()  %>%
  mutate(k = 1:30,
         dif = as.numeric(coef_lo) - as.numeric(coef_hi),
         dif_sd = sqrt(as.numeric(sd_lo)^2 + as.numeric(sd_hi)^2),
         dif_lobd = dif - 1.96*dif_sd,
         dif_hibd = dif + 1.96*dif_sd
         )

#Here's standard error that we report in text, to justify null result for H2
relig_results$dif_sd[which(relig_results$k == 10)] #0.01856327

#Here's difference in coefficients that we compare to the biggest one we see in neighbor experiment
relig_results$dif[which(relig_results$k == 10)] #0.03726846

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

#Results without Bangalore
relig_results_2 = sapply(1:30, function(x) calc_p_relig(k=x, Dat = B[-which(B$Wave == 'Bangalore 2016'),])) %>% t() %>% data.frame()  %>%
  mutate(k = 1:30,
         dif = as.numeric(coef_lo) - as.numeric(coef_hi),
         dif_sd = sqrt(as.numeric(sd_lo)^2 + as.numeric(sd_hi)^2),
         dif_lobd = dif - 1.96*dif_sd,
         dif_hibd = dif + 1.96*dif_sd
  )

#Data frame for plotting
rrc_2 = 
  relig_results_2[seq(from = 2, to = 30, by = 3),] %>% 
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

#Figure A5
ggplot(data = rrc_2, aes(x = k, y = Coef)) + 
  geom_pointrange(aes(ymin = lo, ymax = hi, x = k, shape = Seg, alpha = sig), 
                  position = position_dodge(width = 0.9)) +
  scale_shape_manual('Exposure', values = c(16, 17), labels = c('Low','High')) +
  scale_alpha_manual('p < 0.05', c(TRUE, FALSE), values=c(0.5, 1), labels = c('Yes','No')) +
  labs(shape = 'Exposure') + 
  theme_minimal() + ylab('Coethnicity Coefficients') +
  theme(text = element_text(size = 16),
        plot.title = element_text(hjust = 0.5)) + 
  ggtitle('k-Nearest Own Religion,\nExcluding Bangalore') +
  guides(shape = guide_legend(order = 1), 
         alpha = guide_legend(order = 0))
#ggsave(filename = paste0(path0, 'k-z_coefficients_no-bangalore.png'), height = 150, width = 150, units = 'mm'); rm(path0)
