####### study characteristics for all #######
load("U:/Studien/Empfind/Projekte/Quality_of_life_2020/yifeng/R data/ISE3_complete.Rdata")
attach(ISE3_complete)

load("U:/Studien/Empfind/Projekte/Quality_of_life_2020/yifeng/R data/MarieFu2.Rdata")
attach(Marie_fu2)

load("U:/Studien/Empfind/Projekte/Quality_of_life_2020/yifeng/R data/ISE1_tumor_included.Rdata")
attach(ISE1_tumour_included)

load("U:/Studien/Empfind/Projekte/Quality_of_life_2020/yifeng/R data/ISE1&3_complete.Rdata")
attach(ISE1_ISE3_1)

################# characteristics of ISE-3 at the fu and MARIE controls at the fu #######################
#########################################################################################################

## generate age variable ##

#ISE3 at the FU
days1 <- difftime(ISE3_complete$FB_Datum, ISE3_complete$FB_GebDatum, units = "days")
days1

ageapprox1 <- (days1/365.25)
ageapprox1

age1 <- as.numeric(ageapprox1)
age1

ISE3_complete$age1 <- age1
table(age1)
hist(age1)
summary(age1)
sd(age1, na.rm = T)

# categorization of age #
age_cat1 <- rep(NA, nrow(ISE3_complete))
age_cat1[age1<50] <- 1
age_cat1[age1>=50 & age1<65] <- 2
age_cat1[age1>=65 & age1<75] <- 3
age_cat1[age1>=75] <- 4

age_cat1 <- as.factor(age_cat1)
ISE3_complete$age_cat1 <- age_cat1
table(age_cat1)

#MARIE cotrol at the fu
days2 <- difftime(Marie_fu2$EingangStempel, Marie_fu2$gebdat, units = "days")
days2

ageapprox2 <- (days2/365.25)
ageapprox2

age2 <- as.numeric(ageapprox2)
age2

Marie_fu2$age2 <- age2
table(age2)
hist(age2)
summary(age2)
sd(age2, na.rm = T)

age_cat2 <- rep(NA, nrow(Marie_fu2))
age_cat2[age2<50] <- 1
age_cat2[age2>=50 & age2<65] <- 2
age_cat2[age2>=65 & age2<75] <- 3
age_cat2[age2>=75] <- 4

age_cat2 <- as.factor(age_cat2)
Marie_fu2$age_cat2 <- age_cat2
table(age_cat2)

## bmi ##

# ISE3 at the fu
summary(ISE3_complete$BMI)
sd(ISE3_complete$BMI, na.rm = T)
bmi_cat1 <- rep(NA, nrow(ISE3_complete))
bmi_cat1[ISE3_complete$BMI<25] <- 1
bmi_cat1[ISE3_complete$BMI>=25] <- 2

bmi_cat1 <- as.factor(bmi_cat1)
ISE3_complete$bmi_cat11 <- bmi_cat1
table(bmi_cat1)

# MARIE control at the fu
summary(bmi_jetzt)
sd(bmi_jetzt, na.rm = T)
bmi_cat2 <- rep(NA, nrow(Marie_fu2))
bmi_cat2[bmi_jetzt<25] <- 1
bmi_cat2[bmi_jetzt>=25] <- 2

bmi_cat2 <- as.factor(bmi_cat2)
Marie_fu2$bmi_cat2 <- bmi_cat2
table(bmi_cat2)

## comorbidities/chronic diseases ##
# ISE3 at the fu
table(Depress)
table(Bluthochd)
table(Schlaganf)
table(P_Diabetes)

# MARIE controls at the fu
table(k_blutdruck)
table(k_herz)
table(k_schlag)
table(k_diab)


## marital status/living alone ##
# ISE3 at the fu
ISE3_complete$P_FamStand <- as.integer(ISE3_complete$P_FamStand)

table(ISE3_complete$P_FamStand)
marital_bin1 <- ISE3_complete$P_FamStand
marital_bin1[which(ISE3_complete$P_FamStand==2|ISE3_complete$P_FamStand==4|ISE3_complete$P_FamStand==3|ISE3_complete$P_FamStand==5)] <- 0
marital_bin1[which(ISE3_complete$P_FamStand==1|ISE3_complete$P_FamStand==6)] <- 77
marital_bin1[which(marital_bin1 %in% c(77))] <- 1

marital_bin1 <- as.factor(marital_bin1)
table(marital_bin1)
ISE3_complete$marital_bin1 <- marital_bin1

#MARIE controls at the fu
table(lebensgem)




############# study characteristics of ISE-1 ############
#########################################################
load("U:/Studien/Empfind/Projekte/Quality_of_life_2020/yifeng/R data/ISE1_merged.Rdata")
attach(ISE1)
# bmi
bmi_cat3 <- rep(NA, nrow(ISE1))
bmi_cat3[ISE1$BMI<25] <- 1
bmi_cat3[ISE1$BMI>=25] <- 2

bmi_cat3 <- as.factor(bmi_cat3)
ISE1$bmi_cat13 <- bmi_cat3
table(bmi_cat3)

summary(ISE1$BMI)
sd(ISE1$BMI, na.rm = T)

# age 
days3 <- difftime(ISE1$Datum, ISE1$Geb_datum, units = "days")
days3

ageapprox3 <- (days3/365.25)
ageapprox3

age3 <- as.numeric(ageapprox3)
age3

ISE1$age3 <- age3
table(age3)
hist(age3)
summary(age3)
sd(age3, na.rm = T)

age_cat3 <- rep(NA, nrow(ISE1))
age_cat3[age3<50] <- 1
age_cat3[age3>=50 & age3<65] <- 2
age_cat3[age3>=65 & age3<75] <- 3
age_cat3[age3>=75] <- 4


age_cat3 <- as.factor(age_cat3)
ISE1$age_cat3 <- age_cat3
table(age_cat3)

# cormobidities/chronic diseases 
table(ISE1$Bluthoch)
table(ISE1$Diabetes)

# marital status/living alone

ISE1$Fam_stand <- as.integer(ISE1$Fam_stand)

table(ISE1$Fam_stand)
marital_bin1 <- ISE1$Fam_stand
marital_bin1[which(ISE1$Fam_stand==2|ISE1$Fam_stand==4|ISE1$Fam_stand==3|ISE1$Fam_stand==5)] <- 0
marital_bin1[which(ISE1$Fam_stand==1|ISE1$Fam_stand==6)] <- 77
marital_bin1[which(marital_bin1 %in% c(77))] <- 1

marital_bin1 <- as.factor(marital_bin1)
table(marital_bin1)
ISE1$marital_bin1 <- marital_bin1

# tumor characteristices/stage
table(ISE1$t_klass)

# hormone therapy
table(ISE1$hormonth)

# education
table(ISE1$Schulab)



################## ISE-3 patients characteristics at the baseline  ####################
#######################################################################################

# age
days0 <- difftime(ISE1_ISE3_1$LQdat_1, ISE1_ISE3_1$FB_GebDatum, units = "days")
days0

ageapprox0 <- (days0/365.25)
ageapprox0

age0 <- as.numeric(ageapprox0)
age0

ISE1_ISE3_1$age0 <- age0
table(age0)
hist(age0)
summary(age0)
sd(age0, na.rm = T)


age_cat0 <- rep(NA, nrow(ISE1_ISE3_1))
age_cat0[age0<50] <- 1
age_cat0[age0>=50 & age0<65] <- 2
age_cat0[age0>=65 & age0<75] <- 3
age_cat0[age0>=75] <- 4


age_cat0 <- as.factor(age_cat0)
ISE1_ISE3_1$age_cat0 <- age_cat0
table(age_cat0)

# bmi
bmi_cat0 <- rep(NA, nrow(ISE1_ISE3_1))
bmi_cat0[ISE1_ISE3_1$BMI.x <25] <- 1
bmi_cat0[ISE1_ISE3_1$BMI.x >=25] <- 2

bmi_cat0 <- as.factor(bmi_cat0)
ISE1_ISE3_1$bmi_cat0 <- bmi_cat0
table(bmi_cat0)

summary(ISE1_ISE3_1$BMI.x)
sd(ISE1_ISE3_1$BMI.x, na.rm = T)


# cormobidities/chronic diseases
table(ISE1_ISE3_1$Bluthoch)
table(ISE1_ISE3_1$Diabetes)

# marital status/living alone
ISE1_ISE3_1$Fam_stand <- as.integer(ISE1_ISE3_1$Fam_stand)

table(ISE1_ISE3_1$Fam_stand)
marital_bin0 <- ISE1_ISE3_1$Fam_stand
marital_bin0[which(ISE1_ISE3_1$Fam_stand==2|ISE1_ISE3_1$Fam_stand==4|ISE1_ISE3_1$Fam_stand==3|ISE1_ISE3_1$Fam_stand==5)] <- 0
marital_bin0[which(ISE1_ISE3_1$Fam_stand==1|ISE1_ISE3_1$Fam_stand==6)] <- 77
marital_bin0[which(marital_bin0 %in% c(77))] <- 1

marital_bin0 <- as.factor(marital_bin0)
table(marital_bin0)
ISE1_ISE3_1$marital_bin0 <- marital_bin0

# tumor characteristics/stages
table(ISE1_ISE3_1$t_klass)

# hormone therapy
table(ISE1_ISE3_1$hormonth)

# education
table(ISE1_ISE3_1$Schulab)



################3 drop outs - exclude the ISE3 IDs from ISE1 IDs ##################
###################################################################################
dropouts <- ISE1[!(ISE1$ID_ISE %in% ISE3_complete$ID_ISE),]

# bmi
bmi_cat4 <- rep(NA, nrow(dropouts))
bmi_cat4[dropouts$BMI<25] <- 1
bmi_cat4[dropouts$BMI>=25] <- 2

bmi_cat4 <- as.factor(bmi_cat4)
dropouts$bmi_cat14 <- bmi_cat4
table(bmi_cat4)

summary(dropouts$BMI)
sd(dropouts$BMI, na.rm = T)

# age 
days4 <- difftime(dropouts$Datum, dropouts$Geb_datum, units = "days")
days4

ageapprox4 <- (days4/365.25)
ageapprox4

age4 <- as.numeric(ageapprox4)
age4

dropouts$age4 <- age4
table(age4)
hist(age4)
summary(age4)
sd(age4, na.rm = T)

age_cat4 <- rep(NA, nrow(dropouts))
age_cat4[age4<50] <- 1
age_cat4[age4>=50 & age4<65] <- 2
age_cat4[age4>=65 & age4<75] <- 3
age_cat4[age4>=75] <- 4

age_cat4 <- as.factor(age_cat4)
dropouts$age_cat4 <- age_cat4
table(age_cat4)

# comorbidities/chronic diseases 
table(dropouts$Bluthoch)
table(dropouts$Diabetes)

# marital status/living alone

dropouts$Fam_stand <- as.integer(dropouts$Fam_stand)

table(dropouts$Fam_stand)
marital_bin4 <- dropouts$Fam_stand
marital_bin4[which(dropouts$Fam_stand==2|dropouts$Fam_stand==4|dropouts$Fam_stand==3|dropouts$Fam_stand==5)] <- 0
marital_bin4[which(dropouts$Fam_stand==1|dropouts$Fam_stand==6)] <- 77
marital_bin4[which(marital_bin4 %in% c(77))] <- 1

marital_bin4 <- as.factor(marital_bin4)
table(marital_bin4)
dropouts$marital_bin4 <- marital_bin4

# tumor characteristics/stages
table(dropouts$t_klass)

# education
table(dropouts$Schulab)
