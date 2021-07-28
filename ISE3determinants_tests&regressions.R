###### ISE3 determinants-tests and regressions ######
library(haven)
smoking_data <- read_sas("/Users/mac/Desktop/Remote/made/yifeng/Data/smoking.sas7bdat", 
                         NULL)

load("/Users/mac/Desktop/Remote/made/yifeng/Data/ISE3_complete.Rdata")

ISE3_merged <- merge(smoking_data, ISE3_complete, by ="ID_ISE", all = FALSE)

data <- ISE3_merged
attach(data)

#### participants' characteristics ####
## age 
days <- difftime(FB_Datum, FB_GebDatum, units = "days")
days

ageapprox <- (days/365.25)
ageapprox

age <- as.numeric(ageapprox)
age
data$age <- age
summary(age)

hist(age, 
     main = "Age distribution of ISE-3 cohort",
     xlab = "Age",
     xlim = c(40,100),
     freq = FALSE
)
summary(age)
sd(age)
age_cat <- rep(NA, nrow(data))
age_cat[age<65] <- 1
age_cat[age>=65 & age<75] <- 2
age_cat[age>=75] <- 3

age_cat <- as.factor(age_cat)
data$age_cat <- age_cat
table(age_cat)

## menstrual period
table(AktRegel)

## BMI
hist(BMI,
     main = "Body mass index distribution of ISE-3 cohort",
     xlab = "BMI",
     xlim = c(10,45),
     freq = FALSE)
summary(BMI)
sd(BMI, na.rm = TRUE)
table(bmicat)

bmi_cat <- rep(NA, nrow(data))
bmi_cat[BMI>=30] <- 1
bmi_cat[BMI<30 & BMI>=25] <- 2
bmi_cat[BMI<25 & BMI>=18.5] <- 3
bmi_cat[BMI<18.5] <- 4
table(bmi_cat)
bmi_cat <- as.factor(bmi_cat)
data$bmi_cat <- bmi_cat

bmi_over <- rep(NA, nrow(data))
bmi_over[BMI>=25] <- 1
bmi_over[BMI<25] <- 0
table(bmi_over)
bmi_over <- as.factor(bmi_over)
data$bmi_over <- bmi_over

## marital status
P_FamStand <- as.integer(P_FamStand)
data$P_FamStand <- P_FamStand

table(P_FamStand)
marital_bin <- P_FamStand
marital_bin[which(P_FamStand==2|P_FamStand==4|P_FamStand==3|P_FamStand==5)] <- 0
marital_bin[which(P_FamStand==1|P_FamStand==6)] <- 77
marital_bin[which(marital_bin %in% c(77))] <- 1

marital_bin <- as.factor(marital_bin)
table(marital_bin)
data$marital_bin <- marital_bin

## Get involved in social activities
table(persEngage)
persEngage_yes <- persEngage
persEngage_yes[which(persEngage==3)] <- 0
persEngage_yes[which(persEngage==1|persEngage==2)] <- 77
persEngage_yes[which(persEngage_yes %in% c(77))] <- 1
persEngage_yes[which(persEngage_yes %in% c(9))] <- NA
table(persEngage_yes)
data$persEngage_yes <- persEngage_yes

## recurrence or metastases
table(RezTuMe)

## smoking status at the fu
table(smokingStatus3)
smoking_cat <- rep(NA, nrow(data))
smoking_cat[smokingStatus3==2] <- 3
smoking_cat[smokingStatus3==1] <- 2
smoking_cat[smokingStatus3==0] <- 1

smoking_cat <- as.factor(smoking_cat)
data$smoking_cat <- smoking_cat
table(smoking_cat)

## drugs averting tumor
table(aromatase)
aromatase[which(aromatase %in% c(8, 9))] <- NA

table(mistel)
mistel[which(mistel %in% c(8, 9))] <- NA

table(tamox)
tamox[which(tamox %in% c(8, 9))] <- NA

## chornic dieases
table(Depress)
Depress[which(Depress %in% c(8, 9))] <- NA
table(P_Diabetes)
P_Diabetes[which(P_Diabetes %in% c(8, 9))] <- NA
table(Bluthochd)
Bluthochd[which(Bluthochd %in% c(8, 9))] <- NA
table(Schlaganf)
Schlaganf[which(Schlaganf %in% c(9))] <- NA

######### wilcoxon test #########
## for age 
wilcox.test(N_pf~age_dummy)  
wilcox.test(N_rf~age_dummy)
wilcox.test(N_cf~age_dummy)
wilcox.test(N_ef~age_dummy)   
wilcox.test(N_sf~age_dummy)
wilcox.test(N_qol~age_dummy)
wilcox.test(N_dys~age_dummy)
wilcox.test(N_pain~age_dummy)
wilcox.test(N_fatigue~age_dummy)
wilcox.test(N_ins~age_dummy)  
wilcox.test(N_al~age_dummy)
wilcox.test(N_nau_vomit~age_dummy)
wilcox.test(N_cons~age_dummy)
wilcox.test(N_dia~age_dummy)
wilcox.test(N_fd~age_dummy)

## for BMI (vital determinant)
wilcox.test(N_pf~bmi_over) 
wilcox.test(N_rf~bmi_over)
wilcox.test(N_cf~bmi_over)
wilcox.test(N_ef~bmi_over)   
wilcox.test(N_sf~bmi_over)
wilcox.test(N_qol~bmi_over)  
wilcox.test(N_dys~bmi_over)    
wilcox.test(N_pain~bmi_over)   
wilcox.test(N_fatigue~bmi_over)  
wilcox.test(N_ins~bmi_over)  
wilcox.test(N_al~bmi_over)
wilcox.test(N_nau_vomit~bmi_over)
wilcox.test(N_cons~bmi_over)
wilcox.test(N_dia~bmi_over)
wilcox.test(N_fd~bmi_over)

## for marital
wilcox.test(N_pf~marital_bin)  
wilcox.test(N_rf~marital_bin)
wilcox.test(N_cf~marital_bin)
wilcox.test(N_ef~marital_bin)   
wilcox.test(N_sf~marital_bin)
wilcox.test(N_qol~marital_bin)
wilcox.test(N_dys~marital_bin)
wilcox.test(N_pain~marital_bin)
wilcox.test(N_fatigue~marital_bin)
wilcox.test(N_ins~marital_bin)  
wilcox.test(N_al~marital_bin)
wilcox.test(N_nau_vomit~marital_bin)
wilcox.test(N_cons~marital_bin)
wilcox.test(N_dia~marital_bin)
wilcox.test(N_fd~marital_bin)

## for smoking
wilcox.test(N_pf~smoking_cat)  
wilcox.test(N_rf~smoking_cat)
wilcox.test(N_cf~smoking_cat)
wilcox.test(N_ef~smoking_cat)   
wilcox.test(N_sf~smoking_cat)
wilcox.test(N_qol~smoking_cat)
wilcox.test(N_dys~smoking_cat)
wilcox.test(N_pain~smoking_cat)
wilcox.test(N_fatigue~smoking_cat)
wilcox.test(N_ins~smoking_cat)  
wilcox.test(N_al~smoking_cat)
wilcox.test(N_nau_vomit~smoking_cat)
wilcox.test(N_cons~smoking_cat)
wilcox.test(N_dia~smoking_cat)
wilcox.test(N_fd~smoking_cat)

## for recurrence 
wilcox.test(N_pf~RezTuMe)  
wilcox.test(N_rf~RezTuMe)  
wilcox.test(N_cf~RezTuMe) 
wilcox.test(N_ef~RezTuMe)  
wilcox.test(N_sf~RezTuMe)   
wilcox.test(N_qol~RezTuMe)   
wilcox.test(N_dys~RezTuMe)    
wilcox.test(N_pain~RezTuMe)   
wilcox.test(N_fatigue~RezTuMe) 
wilcox.test(N_ins~RezTuMe)  
wilcox.test(N_al~RezTuMe)
wilcox.test(N_nau_vomit~RezTuMe)
wilcox.test(N_cons~RezTuMe)
wilcox.test(N_dia~RezTuMe)
wilcox.test(N_fd~RezTuMe)

## for smoking
wilcox.test(N_pf~smoking_cat)  
wilcox.test(N_rf~smoking_cat)  
wilcox.test(N_cf~smoking_cat)  
wilcox.test(N_ef~smoking_cat)   
wilcox.test(N_sf~smoking_cat)   
wilcox.test(N_qol~smoking_cat)  
wilcox.test(N_dys~smoking_cat)    
wilcox.test(N_pain~smoking_cat)   
wilcox.test(N_fatigue~smoking_cat)  
wilcox.test(N_ins~smoking_cat)  
wilcox.test(N_al~smoking_cat)
wilcox.test(N_nau_vomit~smoking_cat)
wilcox.test(N_cons~smoking_cat)
wilcox.test(N_dia~smoking_cat)
wilcox.test(N_fd~smoking_cat)

## for marital status
wilcox.test(N_pf~marital_bin)  
wilcox.test(N_rf~marital_bin)  
wilcox.test(N_cf~marital_bin)  
wilcox.test(N_ef~marital_bin)   
wilcox.test(N_sf~marital_bin)   
wilcox.test(N_qol~marital_bin)  
wilcox.test(N_dys~marital_bin)    
wilcox.test(N_pain~marital_bin)   
wilcox.test(N_fatigue~marital_bin)  
wilcox.test(N_ins~marital_bin)  
wilcox.test(N_al~marital_bin)
wilcox.test(N_nau_vomit~marital_bin)
wilcox.test(N_cons~marital_bin)
wilcox.test(N_dia~marital_bin)
wilcox.test(N_fd~marital_bin)

## for chronical diseases
# depression  (vital determinant)
wilcox.test(N_pf~Depress)  
wilcox.test(N_rf~Depress)  
wilcox.test(N_cf~Depress)  
wilcox.test(N_ef~Depress)   
wilcox.test(N_sf~Depress)   
wilcox.test(N_qol~Depress)   
wilcox.test(N_dys~Depress)    
wilcox.test(N_pain~Depress)   
wilcox.test(N_fatigue~Depress)  
wilcox.test(N_ins~Depress)    
wilcox.test(N_al~Depress)    
wilcox.test(N_nau_vomit~Depress)   
wilcox.test(N_cons~Depress)  
wilcox.test(N_dia~Depress)
wilcox.test(N_fd~Depress)  

# stroke
wilcox.test(N_pf~Schlaganf)  
wilcox.test(N_rf~Schlaganf)  
wilcox.test(N_cf~Schlaganf)  
wilcox.test(N_ef~Schlaganf)    
wilcox.test(N_sf~Schlaganf)   
wilcox.test(N_qol~Schlaganf)  
wilcox.test(N_dys~Schlaganf)    
wilcox.test(N_pain~Schlaganf)  
wilcox.test(N_fatigue~Schlaganf)  
wilcox.test(N_ins~Schlaganf)  
wilcox.test(N_al~Schlaganf)
wilcox.test(N_nau_vomit~Schlaganf)   
wilcox.test(N_cons~Schlaganf)
wilcox.test(N_dia~Schlaganf)
wilcox.test(N_fd~Schlaganf)

# hypertension
wilcox.test(N_pf~Bluthochd) 
wilcox.test(N_rf~Bluthochd)  
wilcox.test(N_cf~Bluthochd)  
wilcox.test(N_ef~Bluthochd)   
wilcox.test(N_sf~Bluthochd)   
wilcox.test(N_qol~Bluthochd)   
wilcox.test(N_dys~Bluthochd)   
wilcox.test(N_pain~Bluthochd)  
wilcox.test(N_fatigue~Bluthochd) 
wilcox.test(N_ins~Bluthochd)  
wilcox.test(N_al~Bluthochd)
wilcox.test(N_nau_vomit~Bluthochd)
wilcox.test(N_cons~Bluthochd)
wilcox.test(N_dia~Bluthochd)
wilcox.test(N_fd~Bluthochd)

# diabetes  
wilcox.test(N_pf~P_Diabetes)  
wilcox.test(N_rf~P_Diabetes)  
wilcox.test(N_cf~P_Diabetes)  
wilcox.test(N_ef~P_Diabetes)   
wilcox.test(N_sf~P_Diabetes)   
wilcox.test(N_qol~P_Diabetes)   
wilcox.test(N_dys~P_Diabetes)    
wilcox.test(N_pain~P_Diabetes)   
wilcox.test(N_fatigue~P_Diabetes)  
wilcox.test(N_ins~P_Diabetes)  
wilcox.test(N_al~P_Diabetes)
wilcox.test(N_nau_vomit~P_Diabetes)
wilcox.test(N_cons~P_Diabetes)
wilcox.test(N_dia~P_Diabetes)
wilcox.test(N_fd~P_Diabetes)

## medications to avert new tumor
# aromatase inhibitor
wilcox.test(N_pf~aromatase)  
wilcox.test(N_rf~aromatase)  
wilcox.test(N_cf~aromatase)  
wilcox.test(N_ef~aromatase)   
wilcox.test(N_sf~aromatase)   
wilcox.test(N_qol~aromatase)   
wilcox.test(N_dys~aromatase)    
wilcox.test(N_pain~aromatase)   
wilcox.test(N_fatigue~aromatase)  
wilcox.test(N_ins~aromatase)  
wilcox.test(N_al~aromatase)
wilcox.test(N_nau_vomit~aromatase)
wilcox.test(N_cons~aromatase)
wilcox.test(N_dia~aromatase)
wilcox.test(N_fd~aromatase)

# mistletoe supplements
wilcox.test(N_pf~mistel)  
wilcox.test(N_rf~mistel)  
wilcox.test(N_cf~mistel)  
wilcox.test(N_ef~mistel)   
wilcox.test(N_sf~mistel)   
wilcox.test(N_qol~mistel)   
wilcox.test(N_dys~mistel)    
wilcox.test(N_pain~mistel)   
wilcox.test(N_fatigue~mistel)  
wilcox.test(N_ins~mistel)  
wilcox.test(N_al~mistel)
wilcox.test(N_nau_vomit~mistel)
wilcox.test(N_cons~mistel)
wilcox.test(N_dia~mistel)
wilcox.test(N_fd~mistel)

# tamoxifen
wilcox.test(N_pf~tamox)  
wilcox.test(N_rf~tamox)  
wilcox.test(N_cf~tamox)  
wilcox.test(N_ef~tamox)   
wilcox.test(N_sf~tamox)   
wilcox.test(N_qol~tamox)   
wilcox.test(N_dys~tamox)    
wilcox.test(N_pain~tamox)   
wilcox.test(N_fatigue~tamox)  
wilcox.test(N_ins~tamox)  
wilcox.test(N_al~tamox)
wilcox.test(N_nau_vomit~tamox)
wilcox.test(N_cons~tamox)
wilcox.test(N_dia~tamox)
wilcox.test(N_fd~tamox)

######## regressions ########
library(MASS)
data_new <- data.frame(ID_ISE, N_qol, N_pf, N_rf, N_cf, N_ef, N_sf, N_dys, N_pain, N_fatigue, N_ins, N_al, N_nau_vomit, N_cons, N_dia, N_fd, age, age_cat, BMI, bmi_over, bmi_cat, smoking_cat, marital_bin, RezTuMe, Depress, Bluthochd, P_Diabetes, Schlaganf, aromatase, mistel, tamox)
data_new <- data_new[complete.cases(data_new), ]

# qol scale 
model_qol <-lm(N_qol~age_cat+bmi_over+marital_bin+Depress+P_Diabetes+Schlaganf+Bluthochd+smoking_cat+aromatase+mistel+tamox+RezTuMe, data = data_new)
summary(model_qol)
stepAIC(model_qol)

bestmodel_qol <- lm(N_qol~age_cat+bmi_over+marital_bin+Depress+P_Diabetes+Bluthochd+tamox+RezTuMe)
summary(bestmodel_qol)

# pf
model_pf <- lm(N_pf~age_cat+bmi_over+marital_bin+Depress+P_Diabetes+Schlaganf+Bluthochd+smoking_cat+aromatase+mistel+tamox+RezTuMe, data = data_new)
stepAIC(model_pf)
summary(model_pf)

bestmodel_pf <- lm(N_pf~bmi_over+marital_bin+Depress+Bluthochd+RezTuMe+aromatase)
summary(bestmodel_pf)

# rf
model_rf <- lm(N_rf~age_cat+bmi_over+marital_bin+Depress+P_Diabetes+Schlaganf+Bluthochd+smoking_cat+aromatase+mistel+tamox+RezTuMe, data = data_new)
stepAIC(model_rf)
summary(model_rf)

bestmodel_rf <- lm(N_rf~marital_bin+Depress+Bluthochd+RezTuMe+tamox)
summary(bestmodel_rf)

# cf
model_cf <- lm(N_cf~age_cat+bmi_over+marital_bin+Depress+P_Diabetes+Schlaganf+Bluthochd+smoking_cat+aromatase+mistel+tamox+RezTuMe, data = data_new)
stepAIC(model_cf)
summary(model_cf)

bestmodel_cf <- lm(N_cf~Depress+Schlaganf+smoking_cat)
summary(bestmodel_cf)

# ef 
model_ef <-lm(N_ef~age_cat+bmi_over+marital_bin+Depress+P_Diabetes+Schlaganf+Bluthochd+smoking_cat+aromatase+mistel+tamox+RezTuMe, data = data_new)
stepAIC(model_ef)
summary(model_ef)

bestmodel_ef <- lm(N_ef~Depress+Schlaganf+Bluthochd)
summary(bestmodel_ef)

# sf  
model_sf <-lm(N_sf~age_cat+bmi_over+marital_bin+Depress+P_Diabetes+Schlaganf+Bluthochd+smoking_cat+aromatase+mistel+tamox+RezTuMe, data = data_new)
stepAIC(model_sf)
summary(model_sf)

bestmodel_sf <- lm(N_sf~Depress+Bluthochd+RezTuMe)
summary(bestmodel_sf)

# dys  
model_dys <-lm(N_dys~age_cat+bmi_over+marital_bin+Depress+P_Diabetes+Schlaganf+Bluthochd+smoking_cat+aromatase+mistel+tamox+RezTuMe, data = data_new)
stepAIC(model_dys)
summary(model_dys)

bestmodel_dys <- lm(N_dys~age_cat+marital_bin+Depress+P_Diabetes+Schlaganf+Bluthochd+aromatase+tamox)
summary(bestmodel_dys)

# pain 
model_pain <-lm(N_pain~age_cat+bmi_over+marital_bin+Depress+P_Diabetes+Schlaganf+Bluthochd+smoking_cat+aromatase+mistel+tamox+RezTuMe, data = data_new)
stepAIC(model_pain)
summary(model_pain)

bestmodel_pain <- lm(N_pain~marital_bin+Depress+Bluthochd+tamox)
summary(bestmodel_pain)

# fatigue 
model_fat <-lm(N_fatigue~age_cat+bmi_over+marital_bin+Depress+P_Diabetes+Schlaganf+Bluthochd+smoking_cat+aromatase+mistel+tamox+RezTuMe, data = data_new)
stepAIC(model_fat)
summary(model_fat)

bestmodel_fat <- lm(N_fatigue~marital_bin+Depress+Bluthochd+aromatase+tamox)
summary(bestmodel_fat)

# insomnia 
model_ins <-lm(N_ins~age_cat+bmi_over+marital_bin+Depress+P_Diabetes+Schlaganf+Bluthochd+smoking_cat+aromatase+mistel+tamox+RezTuMe, data = data_new)
stepAIC(model_ins)
summary(model_ins)

bestmodel_ins <- lm(N_ins~Depress+Bluthochd)
summary(bestmodel_ins)

# appetite loss
model_al <-lm(N_al~age_cat+bmi_over+marital_bin+Depress+P_Diabetes+Schlaganf+Bluthochd+smoking_cat+aromatase+mistel+tamox+RezTuMe, data = data_new)
stepAIC(model_al)
summary(model_al)

bestmodel_al <- lm(N_al~age_cat+Depress)
summary(bestmodel_al)

# nausea vomitting
model_nv <-lm(N_nau_vomit~age_cat+bmi_over+marital_bin+Depress+P_Diabetes+Schlaganf+Bluthochd+smoking_cat+aromatase+mistel+tamox+RezTuMe, data = data_new)
stepAIC(model_nv)
summary(model_nv)

bestmodel_nv <- lm(N_nau_vomit~Depress+Schlaganf+tamox)
summary(bestmodel_nv)

# constipation
model_cons <-lm(N_cons~age_cat+bmi_over+marital_bin+Depress+P_Diabetes+Schlaganf+Bluthochd+smoking_cat+aromatase+mistel+tamox+RezTuMe, data = data_new)
stepAIC(model_cons)
summary(model_cons)

bestmodel_cons <- lm(N_cons~Depress+mistel)
summary(bestmodel_cons)

# diarrhea 
model_dia <-lm(N_dia~age_cat+bmi_over+marital_bin+Depress+P_Diabetes+Schlaganf+Bluthochd+smoking_cat+aromatase+mistel+tamox+RezTuMe, data = data_new)
stepAIC(model_dia)
summary(model_dia)

bestmodel_dia <- lm(N_dia~age_cat+bmi_over+P_Diabetes+mistel+tamox)
summary(bestmodel_dia)

# fd
model_fd <-lm(N_fd~age_cat+bmi_over+marital_bin+Depress+P_Diabetes+Schlaganf+Bluthochd+smoking_cat+aromatase+mistel+tamox+RezTuMe, data = data_new)
stepAIC(model_fd)
summary(model_fd)

bestmodel_fd <- lm(N_fd~age_cat+marital_bin+Depress+Bluthochd+tamox)
summary(bestmodel_fd)  

# 95% CI
confint(bestmodel_qol)
confint(bestmodel_pf)
confint(bestmodel_rf)
confint(bestmodel_cf)
confint(bestmodel_ef)
confint(bestmodel_sf)

confint(bestmodel_dys)
confint(bestmodel_pain)
confint(bestmodel_fat)
confint(bestmodel_ins)
confint(bestmodel_al)
confint(bestmodel_nv)
confint(bestmodel_cons)
confint(bestmodel_dia)
confint(bestmodel_fd)
