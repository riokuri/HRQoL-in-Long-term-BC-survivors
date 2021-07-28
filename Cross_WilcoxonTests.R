######## cross-sectional: Wilcoxon tests for patients and controls #########
load("U:/Studien/Empfind/Projekte/Quality_of_life_2020/yifeng/R data/ISE3_complete.Rdata")
attach(ISE3_complete)

load("U:/Studien/Empfind/Projekte/Quality_of_life_2020/yifeng/R data/MarieFu2.Rdata")
attach(Marie_fu2)

## generate age variable ##
#ISE3
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


#MARIE cotrol
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


### new data frame
Population <- as.factor(c(rep_len("ISE3", 292), rep_len("MARIE_FU2", 1680)))

pf <- c(ISE3_complete$N_pf, Marie_fu2$QoLPhysF_fu2) 
rf <- c(ISE3_complete$N_rf, Marie_fu2$QoLRoleF_fu2) 
cf <- c(ISE3_complete$N_cf, Marie_fu2$QoLCogF_fu2) 
ef <- c(ISE3_complete$N_ef, Marie_fu2$QoLEmoF_fu2) 
sf <- c(ISE3_complete$N_sf, Marie_fu2$QoLSocF_fu2) 
qol <- c(ISE3_complete$N_qol, Marie_fu2$QoLGHS_fu2)
dys <- c(ISE3_complete$N_dys, Marie_fu2$QoLDysp_fu2) 
pain <- c(ISE3_complete$N_pain, Marie_fu2$QoLPain_fu2) 
fat <- c(ISE3_complete$N_fatigue, Marie_fu2$QoLFat_fu2) 
inso <- c(ISE3_complete$N_ins, Marie_fu2$QoLInso_fu2) 
al <- c(ISE3_complete$N_al, Marie_fu2$QoLAppL_fu2) 
nv <- c(ISE3_complete$N_nau_vomit, Marie_fu2$QoLNV_fu2) 
cons <- c(ISE3_complete$N_cons, Marie_fu2$QoLCons_fu2) 
diar <- c(ISE3_complete$N_dia, Marie_fu2$QoLDiar_fu2) 
fin <- c(ISE3_complete$N_fd, Marie_fu2$QoLFin_fu2) 

age <- c(ISE3_complete$age1, Marie_fu2$age2)

ISE_MARIE <- data.frame(Population, age, pf, rf, cf, ef, sf, qol, dys, pain, fat, inso, al, nv, cons, diar, fin)
attach(ISE_MARIE)

age_cat <- rep(NA, nrow(ISE_MARIE))
age_cat[age<65] <- 1
age_cat[age>=65 & age<75] <- 2
age_cat[age>=75] <- 3

age_cat <- as.factor(age_cat)
ISE_MARIE$age_cat <- age_cat
table(age_cat)

isISE3 <- as.factor(ISE_MARIE$Population)
levels(isISE3) <- c(1,0)
ISE_MARIE$isISE3 <- isISE3

###
aggregate(x=qol, by=list(isISE3), FUN=mean, na.rm=T)
aggregate(x=pf, by=list(isISE3), FUN=mean, na.rm=T)
aggregate(x=rf, by=list(isISE3), FUN=mean, na.rm=T)
aggregate(x=cf, by=list(isISE3), FUN=mean, na.rm=T)
aggregate(x=ef, by=list(isISE3), FUN=mean, na.rm=T)
aggregate(x=sf, by=list(isISE3), FUN=mean, na.rm=T)
aggregate(x=dys, by=list(isISE3), FUN=mean, na.rm=T)
aggregate(x=pain, by=list(isISE3), FUN=mean, na.rm=T)
aggregate(x=fat, by=list(isISE3), FUN=mean, na.rm=T)
aggregate(x=inso, by=list(isISE3), FUN=mean, na.rm=T)
aggregate(x=al, by=list(isISE3), FUN=mean, na.rm=T)
aggregate(x=nv, by=list(isISE3), FUN=mean, na.rm=T)
aggregate(x=cons, by=list(isISE3), FUN=mean, na.rm=T)
aggregate(x=diar, by=list(isISE3), FUN=mean, na.rm=T)
aggregate(x=fin, by=list(isISE3), FUN=mean, na.rm=T)

###non-parametric two-sample t-test (before age matching)
wilcox.test(qol~isISE3)
wilcox.test(pf~isISE3) 
wilcox.test(rf~isISE3) 
wilcox.test(cf~isISE3) 
wilcox.test(ef~isISE3)
wilcox.test(sf~isISE3) 
wilcox.test(dys~isISE3)
wilcox.test(al~isISE3) 
wilcox.test(pain~isISE3) 
wilcox.test(fin~isISE3) 
wilcox.test(fat~isISE3)
wilcox.test(diar~isISE3)
wilcox.test(inso~isISE3)
wilcox.test(cons~isISE3) 
wilcox.test(nv~isISE3) 

###non-parametric two-sample t-test (after age matching)
agegroup1 <- data.frame(p1=Population[which(age_cat==1)], pf1=pf[which(age_cat==1)], rf1=rf[which(age_cat==1)], cf1=cf[which(age_cat==1)], ef1=ef[which(age_cat==1)], sf1=sf[which(age_cat==1)], qol1=qol[which(age_cat==1)], dys1=dys[which(age_cat==1)], pain1=pain[which(age_cat==1)], fat1=fat[which(age_cat==1)], inso1=inso[which(age_cat==1)], al1=al[which(age_cat==1)], nv1=nv[which(age_cat==1)], cons1=cons[which(age_cat==1)], diar1=diar[which(age_cat==1)], fin1=fin[which(age_cat==1)])
agegroup2 <- data.frame(p2=Population[which(age_cat==2)], pf2=pf[which(age_cat==2)], rf2=rf[which(age_cat==2)], cf2=cf[which(age_cat==2)], ef2=ef[which(age_cat==2)], sf2=sf[which(age_cat==2)], qol2=qol[which(age_cat==2)], dys2=dys[which(age_cat==2)], pain2=pain[which(age_cat==2)], fat2=fat[which(age_cat==2)], inso2=inso[which(age_cat==2)], al2=al[which(age_cat==2)], nv2=nv[which(age_cat==2)], cons2=cons[which(age_cat==2)], diar2=diar[which(age_cat==2)], fin2=fin[which(age_cat==2)])
agegroup3 <- data.frame(p3=Population[which(age_cat==3)], pf3=pf[which(age_cat==3)], rf3=rf[which(age_cat==3)], cf3=cf[which(age_cat==3)], ef3=ef[which(age_cat==3)], sf3=sf[which(age_cat==3)], qol3=qol[which(age_cat==3)], dys3=dys[which(age_cat==3)], pain3=pain[which(age_cat==3)], fat3=fat[which(age_cat==3)], inso3=inso[which(age_cat==3)], al3=al[which(age_cat==3)], nv3=nv[which(age_cat==3)], cons3=cons[which(age_cat==3)], diar3=diar[which(age_cat==3)], fin3=fin[which(age_cat==3)])

isISE3_1 <- as.factor(agegroup1$p1)
levels(isISE3_1) <- c(1,0)
agegroup1$isISE3_1 <- isISE3_1

isISE3_2 <- as.factor(agegroup2$p2)
levels(isISE3_2) <- c(1,0)
agegroup2$isISE3_2 <- isISE3_2

isISE3_3 <- as.factor(agegroup3$p3)
levels(isISE3_3) <- c(1,0)
agegroup3$isISE3_3 <- isISE3_3

attach(agegroup1)
attach(agegroup2)
attach(agegroup3)
summary(agegroup1)
summary(agegroup2)
summary(agegroup3)

## for age<65
#mean for patients and controls
aggregate(x=qol1, by=list(isISE3_1), FUN=mean, na.rm=T)
aggregate(x=pf1, by=list(isISE3_1), FUN=mean, na.rm=T)
aggregate(x=rf1, by=list(isISE3_1), FUN=mean, na.rm=T)
aggregate(x=cf1, by=list(isISE3_1), FUN=mean, na.rm=T)
aggregate(x=ef1, by=list(isISE3_1), FUN=mean, na.rm=T)
aggregate(x=sf1, by=list(isISE3_1), FUN=mean, na.rm=T)
aggregate(x=dys1, by=list(isISE3_1), FUN=mean, na.rm=T)
aggregate(x=pain1, by=list(isISE3_1), FUN=mean, na.rm=T)
aggregate(x=fat1, by=list(isISE3_1), FUN=mean, na.rm=T)
aggregate(x=inso1, by=list(isISE3_1), FUN=mean, na.rm=T)
aggregate(x=al1, by=list(isISE3_1), FUN=mean, na.rm=T)
aggregate(x=nv1, by=list(isISE3_1), FUN=mean, na.rm=T)
aggregate(x=cons1, by=list(isISE3_1), FUN=mean, na.rm=T)
aggregate(x=diar1, by=list(isISE3_1), FUN=mean, na.rm=T)
aggregate(x=fin1, by=list(isISE3_1), FUN=mean, na.rm=T)

## test difference by age group 1
wilcox.test(qol1~isISE3_1)
wilcox.test(rf1~isISE3_1)
wilcox.test(cf1~isISE3_1)
wilcox.test(sf1~isISE3_1)
wilcox.test(dys1~isISE3_1)
wilcox.test(pain1~isISE3_1) 
wilcox.test(al1~isISE3_1)
wilcox.test(diar1~isISE3_1)
wilcox.test(fin1~isISE3_1)
wilcox.test(cons1~isISE3_1)
wilcox.test(pf1~isISE3_1)
wilcox.test(ef1~isISE3_1) 
wilcox.test(fat1~isISE3_1) 
wilcox.test(inso1~isISE3_1) 
wilcox.test(nv1~isISE3_1) 

## for 65-74
#mean for patients and controls
aggregate(x=qol2, by=list(isISE3_2), FUN=mean, na.rm=T)
aggregate(x=pf2, by=list(isISE3_2), FUN=mean, na.rm=T)
aggregate(x=rf2, by=list(isISE3_2), FUN=mean, na.rm=T)
aggregate(x=cf2, by=list(isISE3_2), FUN=mean, na.rm=T)
aggregate(x=ef2, by=list(isISE3_2), FUN=mean, na.rm=T)
aggregate(x=sf2, by=list(isISE3_2), FUN=mean, na.rm=T)
aggregate(x=dys2, by=list(isISE3_2), FUN=mean, na.rm=T)
aggregate(x=pain2, by=list(isISE3_2), FUN=mean, na.rm=T)
aggregate(x=fat2, by=list(isISE3_2), FUN=mean, na.rm=T)
aggregate(x=inso2, by=list(isISE3_2), FUN=mean, na.rm=T)
aggregate(x=al2, by=list(isISE3_2), FUN=mean, na.rm=T)
aggregate(x=nv2, by=list(isISE3_2), FUN=mean, na.rm=T)
aggregate(x=cons2, by=list(isISE3_2), FUN=mean, na.rm=T)
aggregate(x=diar2, by=list(isISE3_2), FUN=mean, na.rm=T)
aggregate(x=fin2, by=list(isISE3_2), FUN=mean, na.rm=T)

## test study difference by age group 2
wilcox.test(qol2~isISE3_2)
wilcox.test(cf2~isISE3_2)
wilcox.test(sf2~isISE3_2)
wilcox.test(dys2~isISE3_2)
wilcox.test(pain2~isISE3_2) 
wilcox.test(al2~isISE3_2)
wilcox.test(diar2~isISE3_2) 
wilcox.test(fin2~isISE3_2)
wilcox.test(pf2~isISE3_2) 
wilcox.test(rf2~isISE3_2) 
wilcox.test(ef2~isISE3_2) 
wilcox.test(fat2~isISE3_2)  
wilcox.test(inso2~isISE3_2)
wilcox.test(nv2~isISE3_2) 
wilcox.test(cons2~isISE3_2) 

## for >=75
#mean for patients and controls
aggregate(x=qol3, by=list(isISE3_3), FUN=mean, na.rm=T)
aggregate(x=pf3, by=list(isISE3_3), FUN=mean, na.rm=T)
aggregate(x=rf3, by=list(isISE3_3), FUN=mean, na.rm=T)
aggregate(x=cf3, by=list(isISE3_3), FUN=mean, na.rm=T)
aggregate(x=ef3, by=list(isISE3_3), FUN=mean, na.rm=T)
aggregate(x=sf3, by=list(isISE3_3), FUN=mean, na.rm=T)
aggregate(x=dys3, by=list(isISE3_3), FUN=mean, na.rm=T)
aggregate(x=pain3, by=list(isISE3_3), FUN=mean, na.rm=T)
aggregate(x=fat3, by=list(isISE3_3), FUN=mean, na.rm=T)
aggregate(x=inso3, by=list(isISE3_3), FUN=mean, na.rm=T)
aggregate(x=al3, by=list(isISE3_3), FUN=mean, na.rm=T)
aggregate(x=nv3, by=list(isISE3_3), FUN=mean, na.rm=T)
aggregate(x=cons3, by=list(isISE3_3), FUN=mean, na.rm=T)
aggregate(x=diar3, by=list(isISE3_3), FUN=mean, na.rm=T)
aggregate(x=fin3, by=list(isISE3_3), FUN=mean, na.rm=T)

## test study difference by age group 3
wilcox.test(qol3~isISE3_3)
wilcox.test(pf3~isISE3_3)
wilcox.test(rf3~isISE3_3)
wilcox.test(cf3~isISE3_3)
wilcox.test(sf3~isISE3_3)
wilcox.test(dys3~isISE3_3)
wilcox.test(pain3~isISE3_3) 
wilcox.test(fat3~isISE3_3)
wilcox.test(al3~isISE3_3)
wilcox.test(cons3~isISE3_3)
wilcox.test(diar3~isISE3_3)
wilcox.test(fin3~isISE3_3)
wilcox.test(ef3~isISE3_3)
wilcox.test(nv3~isISE3_3) 
wilcox.test(inso3~isISE3_3)