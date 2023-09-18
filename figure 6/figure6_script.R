#Figure 6: Preferences for non-coethnic neighbor in neighbor conjoint experiment, 
#comparing high- to low-exposure respondents. 

rm(list=ls())
library(plyr);library(dplyr, warn.conflicts = F)
library(tidyr)
library(ggplot2)
suppressMessages( library(lmtest) )
suppressMessages( library(multiwayvcov) )
suppressMessages(library(stargazer))

s = function(x){summary(factor(x))}
setwd('/Users/jeremyspater/Dropbox/duke/political economy core/prospectus/india part/2018_version/replication_files/scripts and data/figure 6')

Q = read.csv('5-21-18_deid_nearestK.csv',
             na.strings=c('','NA'),strip.white=T,stringsAsFactors = F)
Q = Q[which(Q$Wave == 'Bangalore 2017'),]

##############################################################################################################################

Q = Q %>% dplyr::rename(Q2A1 = L.Neighbor_Random_2_A1, #second question, candidate 1, characteristic A
                        Q2A2 = L.Neighbor_Random_2_A2,
                        Q2B1 = L.Neighbor_Random_2_B1,
                        Q2B2 = L.Neighbor_Random_2_B2,
                        Q2C1 = L.Neighbor_Random_2_C1,
                        Q2C2 = L.Neighbor_Random_2_C2,
                        Q3A1 = L.Neighbor_Random_3_A1,
                        Q3A2 = L.Neighbor_Random_3_A2,
                        Q3B1 = L.Neighbor_Random_3_B1,
                        Q3B2 = L.Neighbor_Random_3_B2,
                        Q3C1 = L.Neighbor_Random_3_C1,
                        Q3C2 = L.Neighbor_Random_3_C2,
                       # Q1 = L.Neighbor_Question_1,  #first question: choose candidate 1 or 2?
                        Q2 = L.Neighbor_Question_2,
                        Q3 = L.Neighbor_Question_3)

#rearrange so one row is one conjoint observation. 3x as many rows as A
#new variables: A1, B1 are two traits for candidate 1; similar for 2; and y is respondent's choice between candidates
B = Q %>% 
  unite('Q2',matches('Q2')) %>% unite('Q3',matches('Q3')) %>%
  gather(Question,b,starts_with('Q'))  %>% arrange(X) %>% separate('b', c('A1','A2','B1','B2','C1','C2','y'))

B = B %>% filter(!(A1 == A2 & B1 == B2 & C1 == C2)) #Drop observations where candidates have same profile

#Make new data frame where each row is one PROFILE, ie each question becomes two rows (one for each candidate)
#New variables: A1, A2 are combined as A: trait A for either candidate
C = B %>% unite('ABC1',c(A1,B1,C1)) %>% unite('ABC2',c(A2,B2,C2)) %>% gather(Neighbor, ABC, c(ABC1,ABC2)) %>%
  arrange(X) %>% separate('ABC',c('A','B','C')) %>% mutate(Neighbor = as.numeric(mapvalues(Neighbor, from = c('ABC1','ABC2'), to = c(1,2))))

C$B_revised = NA
C$B_revised[which(C$C.C6_Religion == 'Hindu' & C$B == 0)] = 1 #Respondent and neighbor both Hindu
C$B_revised[which(C$C.C6_Religion == 'Hindu' & C$B == 1)] = 0 #Respondent Hindu, neighbor Muslim
C$B_revised[which(C$C.C6_Religion == 'Hindu' & C$B == 2)] = 2 #non-kannada-speaker: keep it the same
C$B_revised[which(C$C.C6_Religion == 'Muslim' & C$B == 0)] = 0 #Respondent Muslim, neighbor Hindu
C$B_revised[which(C$C.C6_Religion == 'Muslim' & C$B == 1)] = 1 #Respondent Hindu, neighbor Muslim
C$B_revised[which(C$C.C6_Religion == 'Muslim' & C$B == 2)] = 2 #non-kannada-speaker: keep it the same
C$B = as.character(C$B_revised) #this is necessary so model-matrix step below works
#B0: other religion. B1: same religion. B2: non kannada speaker

#this introduces NA's (people who are not Hindus or Muslims); drop these here
C = C[which(!is.na(C$B)),]

#function to make dummies for trait levels
ModFn = function(x,f){
  data.frame(x, model.matrix(as.formula(f), data=x))}

C = C %>% ModFn('~ A - 1') %>% ModFn('~ B - 1') %>% ModFn('~ C - 1') #function to make dummies for trait levels
C = C %>% mutate(Y = y == Neighbor) #1 when that candidate is picked
C = C %>% dplyr::rename(A_0 = A0, #rename variables to be consistent with earlier version of code
                        A_1 = A1,
                        B_0 = B0,
                        B_1 = B1,
                        C_0 = C0,
                        C_1 = C1,
                        C_2 = C2,
                        C_3 = C3,
                        C_4 = C4)

B = C; rm(C) #rename variables to be consistent with earlier version of code
##############################################################################################################

##########do analysis and make plots######################################################################
#regression formula
form1 = as.formula(paste0('Y ~ ',
                          paste(strsplit('A_0 A_1 B_0 B_1 C_0 C_1 C_2 C_3 C_4', split = ' ')[[1]], collapse=' + ')))

DF_C_v2 = function(l_m,id){  #
  l_m_pl = data.frame(Parameter = rownames(l_m[-1,])) %>% #-1 drops intercept
    mutate(Coef = l_m[-1,1]) %>%
    mutate(Lo = Coef - 1.96*l_m[-1,2]) %>%
    mutate(Hi = Coef + 1.96*l_m[-1,2]) %>%
    rbind(data.frame(Parameter = c('A_3','B_4'),
                     Coef = c(0,0),
                     Lo = c(0,0),
                     Hi = c(0,0)
    )) %>%
  mutate(ID = id) %>% 
    mutate(Parameter = as.character(Parameter)) %>%
    arrange(Parameter)
}

# CoefPlotSubFn_5_21_18_Neighbor_23 = function(da,var,city,titl){ #
#   ggplot(data = da, group = ID, aes(x = Parameter)) +
#     geom_point( aes( y = Coef, color = ID ), position=position_dodge( width=0.3 ) ) + 
#     geom_errorbar( aes( ymin=Lo, ymax=Hi, group = ID), position=position_dodge( width=0.3 ) )  +
#     theme_minimal() + theme(text=element_text(size=16))+
#     labs(x = 'Attribute', y = 'Coefficient')+
#     scale_x_discrete(labels = c('Works for\nMunicipal\nCorporation','Owns a Tea\nStall [base]',
#                                 'Other\nReligion','Same\nReligion [base]',#'Is a\nNon-Kannada\nspeaker [base]',
#                                 'Is a Little\nRicher','Is a Little\nPoorer','Is a Lot\nPoorer','Is a Lot\nRicher','Has the Same\nIncome [base]')) +
#     scale_color_discrete(name='Metric') +
#     ggtitle('Neighbor Conjoint') + theme(plot.title = element_text(hjust = 0.5)) +
#     geom_vline(xintercept= 2.5, linetype=2) + geom_vline(xintercept= 4.5, linetype=2) +
#     coord_flip() 
#   #ggsave(file = paste0(paste(var,paste0(city,collapse=', '),sep='; '),'.png'), width = 7, height = 7, units = "in")
# }

# PlotSubFn_Cl_Value_5_18_Neighbor = function(var,Split){ # 
#   C = B 
#   VarLo = Split
#   VarHi = Split
#   lm_lo = lm( form1, data = C[which(C[,var] < VarLo),] ) 
#   lm_hi = lm( form1, data = C[which(C[,var] >= VarHi),] )
#   lm_lo_sum = coeftest(lm_lo, cluster.vcov(lm_lo, C[which(C[,var] < VarLo),c('X','A.A7_Area.Neighborhood')] ) )
#   lm_hi_sum = coeftest(lm_hi, cluster.vcov(lm_hi, C[which(C[,var] >= VarHi),c('X','A.A7_Area.Neighborhood')] ) )
#   CoefPlotSubFn_5_21_18_Neighbor_23( rbind( DF_C_v2(lm_lo_sum,'Low'),DF_C_v2(lm_hi_sum,'High') ), var, city ) 
# }

# PlotSubFn_Cl_Value_5_18_Neighbor_Subset = function(var,Split, relig){ # subset to people in one religion
#   C = B %>% filter(C.C6_Religion == relig)
#   VarLo = Split
#   VarHi = Split
#   lm_lo = lm( form1, data = C[which(C[,var] < VarLo),] ) 
#   lm_hi = lm( form1, data = C[which(C[,var] >= VarHi),] )
#   lm_lo_sum = coeftest(lm_lo, cluster.vcov(lm_lo, C[which(C[,var] < VarLo),c('X','A.A7_Area.Neighborhood')] ) )
#   lm_hi_sum = coeftest(lm_hi, cluster.vcov(lm_hi, C[which(C[,var] >= VarHi),c('X','A.A7_Area.Neighborhood')] ) )
#   CoefPlotSubFn_5_21_18_Neighbor_23( rbind( DF_C_v2(lm_lo_sum,'Low'),DF_C_v2(lm_hi_sum,'High') ), var, city ) 
# }


###########################################################################################
#Make functions to extract p and z as a function of k

calc_p_relig_neigh = function(k, Dat){ #
  var <- paste0('Nearest',k,'_SameReligion') #
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
    z_a2 = (lm_clus_lo[3,1] - lm_clus_hi[3,1]) / sqrt(lm_clus_lo[3,2]^2 + lm_clus_hi[3,2]^2)
    p_a2 = 2 * pnorm( abs(z_a2), mean = 0, sd = 1, lower.tail = F) 
    coef_lo = lm_clus_lo[3,1]; coef_hi = lm_clus_hi[3,1]
    sd_lo = lm_clus_lo[3,2]; sd_hi = lm_clus_hi[3,2]
    return(data.frame(coef_lo, coef_hi, sd_lo, sd_hi, p_a2)) }
}

relig_results = sapply(1:30, function(x) calc_p_relig_neigh(k=x, Dat = B)) %>% t() %>% data.frame()  %>%
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

#FIGURE 6
ggplot(data = rrc, aes(x = k, y = Coef)) + 
  geom_pointrange(aes(ymin = lo, ymax = hi, x = k, shape = Seg, alpha = sig), 
                  position = position_dodge(width = 0.9)) +
  scale_shape_manual('Exposure', values = c(16, 17), labels = c('Low','High')) + 
  scale_alpha_manual('p < 0.05', c(TRUE, FALSE), values=c(0.5, 1), labels = c('Yes','No')) +
  labs(shape = 'Exposure') +
  theme_minimal() + ylab('Non-Coethnicity Coefficients') +
  theme(text = element_text(size = 16),
        plot.title = element_text(hjust = 0.5)) + 
  ggtitle('k-Nearest Own Religion') +
  guides(shape = guide_legend(order = 1), 
         alpha = guide_legend(order = 0))
path0 = paste0('/Users/jeremyspater/Dropbox/duke/political economy core/prospectus/india part/2018_version/replication_files/scripts and data/figure 6/',
               Sys.Date(),'/')
dir.create(path0)
#ggsave(filename = paste0(path0, 'k-z_coefficients_NEIGHBOR.png'), height = 150, width = 150, units = 'mm')
################################################################################################################################

################################################################################################################################
#De-medianed version

calc_p_relig_neigh_demed = function(k, Dat){ 
  var <- paste0('DeMedNearest',k,'_SameReligion') 
  var_break = 0
  dat_lo = Dat[ which(Dat[,var] < var_break ) ,]
  dat_hi = Dat[ which(Dat[,var] >= var_break ) ,]
  if(nrow(dat_lo) == 0 | nrow(dat_hi) == 0){p_a2 = NA
  }else{
    lm_lo = lm(form1, data = dat_lo )
    lm_hi = lm(form1, data = dat_hi )
    lm_clus_lo = coeftest(lm_lo, cluster.vcov(lm_lo, dat_lo[,c('X','A.A7_Area.Neighborhood')]))
    lm_clus_hi = coeftest(lm_hi, cluster.vcov(lm_hi, dat_hi[,c('X','A.A7_Area.Neighborhood')]))
    z_a2 = (lm_clus_lo[3,1] - lm_clus_hi[3,1]) / sqrt(lm_clus_lo[3,2]^2 + lm_clus_hi[3,2]^2)
    p_a2 = 2 * pnorm( abs(z_a2), mean = 0, sd = 1, lower.tail = F) 
    coef_lo = lm_clus_lo[3,1]; coef_hi = lm_clus_hi[3,1]
    sd_lo = lm_clus_lo[3,2]; sd_hi = lm_clus_hi[3,2]
    return(data.frame(coef_lo, coef_hi, sd_lo, sd_hi, p_a2)) }
}

relig_results_demed = sapply(c(5,10,15,20,25,30), function(x) calc_p_relig_neigh_demed(k=x, Dat = B)) %>% t() %>% data.frame()  %>%
  mutate(k = c(5,10,15,20,25,30),
         dif = as.numeric(coef_lo) - as.numeric(coef_hi),
         dif_sd = sqrt(as.numeric(sd_lo)^2 + as.numeric(sd_hi)^2),
         dif_lobd = dif - 1.96*dif_sd,
         dif_hibd = dif + 1.96*dif_sd
  )

rrc_demed = relig_results_demed %>% 
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

#FIGURE A19
ggplot(data = rrc_demed, aes(x = k, y = Coef)) + 
  geom_pointrange(aes(ymin = lo, ymax = hi, x = k, shape = Seg, alpha = sig), 
                  position = position_dodge(width = 0.9)) +
  scale_shape_manual('Exposure', values = c(16, 17), labels = c('Low','High')) + 
  scale_alpha_manual('p < 0.05', c(TRUE, FALSE), values=c(0.5, 1), labels = c('Yes','No')) +
  labs(shape = 'Exposure') +
  theme_minimal() + ylab('Non-Coethnicity Coefficients') +
  theme(text = element_text(size = 16),
        plot.title = element_text(hjust = 0.5)) + 
  ggtitle('k-Nearest Own Religion (De-medianed)') +
  guides(shape = guide_legend(order = 1), 
         alpha = guide_legend(order = 0))
#ggsave(filename = paste0(path0, 'k-z_coefficients_NEIGHBOR_demed.png'), height = 150, width = 150, units = 'mm')

relig_results_tab = relig_results %>% apply(2, function(x) as.numeric(x)) %>% data.frame() %>%
  select(k, coef_lo, sd_lo, coef_hi, sd_hi, p_a2) %>% 
  rename(Coef_HiExp = coef_lo, SD_HiExp = sd_lo, Coef_LoExp = coef_hi, SD_LoExp = sd_hi, p = p_a2) %>% 
  round(3)
out = stargazer(relig_results_tab, summary = F, rownames = F,
                title = 'Results for co-ethnicity attribute in neighbor experiment compared between
                high- and low-exposure subsamples, based on religious exposure',
                label = 'table:ReligResults_Neighbor')
#writeLines(out,con = paste0(path0,'ReligResults_Neighbor.tex'));rm(out, relig_results_tab)
