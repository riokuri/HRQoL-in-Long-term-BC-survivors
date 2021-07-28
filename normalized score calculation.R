#### normalized scores calculation for QLQ-C30 scales in patients at the FU ####

ISE3_LQ_variables <- read.delim("U:/Studien/Empfind/Projekte/Quality_of_life_2020/yifeng/raw data/ISE3_LQ_variables.txt")
attach(ISE3_LQ_variables)

## data cleaning - convert missing values to NA ##
ISE3_LQ_variables$LQ01[which(ISE3_LQ_variables$LQ01 %in% c(-10))] <- NA
ISE3_LQ_variables$LQ02[which(ISE3_LQ_variables$LQ02 %in% c(-10))] <- NA
ISE3_LQ_variables$LQ03[which(ISE3_LQ_variables$LQ03 %in% c(-10))] <- NA
ISE3_LQ_variables$LQ04[which(ISE3_LQ_variables$LQ04 %in% c(-10))] <- NA
ISE3_LQ_variables$LQ05[which(ISE3_LQ_variables$LQ05 %in% c(-10))] <- NA
ISE3_LQ_variables$LQ06[which(ISE3_LQ_variables$LQ06 %in% c(-10))] <- NA
ISE3_LQ_variables$LQ07[which(ISE3_LQ_variables$LQ07 %in% c(-10))] <- NA
ISE3_LQ_variables$LQ08[which(ISE3_LQ_variables$LQ08 %in% c(-10, -100))] <- NA
ISE3_LQ_variables$LQ09[which(ISE3_LQ_variables$LQ09 %in% c(-10))] <- NA
ISE3_LQ_variables$LQ10[which(ISE3_LQ_variables$LQ10 %in% c(-10, -100))] <- NA
ISE3_LQ_variables$LQ11[which(ISE3_LQ_variables$LQ11 %in% c(-10))] <- NA
ISE3_LQ_variables$LQ12[which(ISE3_LQ_variables$LQ12 %in% c(-10, -100))] <- NA
ISE3_LQ_variables$LQ13[which(ISE3_LQ_variables$LQ13 %in% c(-10))] <- NA
ISE3_LQ_variables$LQ14[which(ISE3_LQ_variables$LQ14 %in% c(-10))] <- NA
ISE3_LQ_variables$LQ15[which(ISE3_LQ_variables$LQ15 %in% c(-10))] <- NA
ISE3_LQ_variables$LQ16[which(ISE3_LQ_variables$LQ16 %in% c(-10, -100))] <- NA
ISE3_LQ_variables$LQ17[which(ISE3_LQ_variables$LQ17 %in% c(-10))] <- NA
ISE3_LQ_variables$LQ18[which(ISE3_LQ_variables$LQ18 %in% c(-10))] <- NA
ISE3_LQ_variables$LQ19[which(ISE3_LQ_variables$LQ19 %in% c(-10))] <- NA
ISE3_LQ_variables$LQ20[which(ISE3_LQ_variables$LQ20 %in% c(-10))] <- NA
ISE3_LQ_variables$LQ21[which(ISE3_LQ_variables$LQ21 %in% c(-10))] <- NA
ISE3_LQ_variables$LQ22[which(ISE3_LQ_variables$LQ22 %in% c(-10))] <- NA
ISE3_LQ_variables$LQ23[which(ISE3_LQ_variables$LQ23 %in% c(-10, -100))] <- NA
ISE3_LQ_variables$LQ24[which(ISE3_LQ_variables$LQ24 %in% c(-10, -100))] <- NA
ISE3_LQ_variables$LQ25[which(ISE3_LQ_variables$LQ25 %in% c(-10))] <- NA
ISE3_LQ_variables$LQ26[which(ISE3_LQ_variables$LQ26 %in% c(-10))] <- NA
ISE3_LQ_variables$LQ27[which(ISE3_LQ_variables$LQ27 %in% c(-10))] <- NA
ISE3_LQ_variables$LQ28[which(ISE3_LQ_variables$LQ28 %in% c(-10))] <- NA
ISE3_LQ_variables$LQ29[which(ISE3_LQ_variables$LQ29 %in% c(-10))] <- NA
ISE3_LQ_variables$LQ30[which(ISE3_LQ_variables$LQ30 %in% c(-10))] <- NA
ISE3_LQ_variables$LQ31[which(ISE3_LQ_variables$LQ31 %in% c(-10))] <- NA
ISE3_LQ_variables$LQ32[which(ISE3_LQ_variables$LQ32 %in% c(-10))] <- NA
ISE3_LQ_variables$LQ33[which(ISE3_LQ_variables$LQ33 %in% c(-10))] <- NA
ISE3_LQ_variables$LQ34[which(ISE3_LQ_variables$LQ34 %in% c(-10))] <- NA
ISE3_LQ_variables$LQ35[which(ISE3_LQ_variables$LQ35 %in% c(-10))] <- NA
ISE3_LQ_variables$LQ36[which(ISE3_LQ_variables$LQ36 %in% c(-10))] <- NA
ISE3_LQ_variables$LQ37[which(ISE3_LQ_variables$LQ37 %in% c(-10))] <- NA
ISE3_LQ_variables$LQ38[which(ISE3_LQ_variables$LQ38 %in% c(-10))] <- NA
ISE3_LQ_variables$LQ39[which(ISE3_LQ_variables$LQ39 %in% c(-10))] <- NA
ISE3_LQ_variables$LQ40[which(ISE3_LQ_variables$LQ40 %in% c(-10))] <- NA
ISE3_LQ_variables$LQ41[which(ISE3_LQ_variables$LQ41 %in% c(-10))] <- NA

##### scoring
#### functioning scale
###physical functioning
#raw scores
DM_pf=apply(is.na(ISE3_LQ_variables[,c(3:7)]),1,sum)
rs_pf=apply(ISE3_LQ_variables[,c(3:7)],1,sum,na.rm=TRUE)
rs_pf=rs_pf/(5-DM_pf)
rs_pf[DM_pf>2] <- NA
ISE3_LQ_variables$rs_pf <- rs_pf

#normalized scores (5 items, will have value only when missing items <=2 for each individual)
ISE3_LQ_variables$N_pf = (1-(ISE3_LQ_variables$rs_pf-1)/3)*100

###role functioning
#raw scores
DM_rf=apply(is.na(ISE3_LQ_variables[,c(8:9)]),1,sum)
rs_rf=apply(ISE3_LQ_variables[,c(8:9)],1,sum,na.rm=TRUE)
rs_rf=rs_rf/(2-DM_rf)
rs_rf[DM_rf>1] <- NA
ISE3_LQ_variables$rs_rf <- rs_rf

#normalized scores (2 items, will have value only when missing items <=1 for each individual)
ISE3_LQ_variables$N_rf = (1-(ISE3_LQ_variables$rs_rf-1)/3)*100

###cognitive functioning
#raw scores
DM_cf=apply(is.na(ISE3_LQ_variables[,c(22,27)]),1,sum)
rs_cf=apply(ISE3_LQ_variables[,c(22,27)],1,sum,na.rm=TRUE)
rs_cf=rs_cf/(2-DM_cf)
rs_cf[DM_cf>1] <- NA
ISE3_LQ_variables$rs_cf <- rs_cf

#normalized scores (2 items, will have value only when missing items <=1 for each individual)
ISE3_LQ_variables$N_cf = (1-(ISE3_LQ_variables$rs_cf-1)/3)*100

###emotional functioning
#raw scores
DM_ef=apply(is.na(ISE3_LQ_variables[,c(23:26)]),1,sum)
rs_ef=apply(ISE3_LQ_variables[,c(23:26)],1,sum,na.rm=TRUE)
rs_ef=rs_ef/(4-DM_ef)
rs_ef[DM_ef>2] <- NA
ISE3_LQ_variables$rs_ef <- rs_ef

#normalized scores (4 items, will have value only when missing items <=2 for each individual)
ISE3_LQ_variables$N_ef = (1-(ISE3_LQ_variables$rs_ef-1)/3)*100

###social functioning
#raw scores
DM_sf=apply(is.na(ISE3_LQ_variables[,c(28:29)]),1,sum)
rs_sf=apply(ISE3_LQ_variables[,c(28:29)],1,sum,na.rm=TRUE)
rs_sf=rs_sf/(2-DM_sf)
rs_sf[DM_sf>1] <- NA
ISE3_LQ_variables$rs_sf <- rs_sf

#normalized scores (2 items, will have value only when missing items <=1 for each individual)
ISE3_LQ_variables$N_sf = (1-(ISE3_LQ_variables$rs_sf-1)/3)*100

#### GHS/QoL scale
#raw scores
DM_qol=apply(is.na(ISE3_LQ_variables[,c(31:32)]),1,sum)
rs_qol=apply(ISE3_LQ_variables[,c(31:32)],1,sum,na.rm=TRUE)
rs_qol=rs_qol/(2-DM_qol)
rs_qol[DM_qol>1] <- NA
ISE3_LQ_variables$rs_qol <- rs_qol

#normalized scores (2 items, will have value only when missing items <=1 for each individual)
ISE3_LQ_variables$N_qol = ((ISE3_LQ_variables$rs_qol-1)/6)*100

#### Symptom scales
### dyspnea
#raw scores
rs_dys <- ISE3_LQ_variables$LQ08
ISE3_LQ_variables$rs_dys <- rs_dys

#normalized scores (1 item)
ISE3_LQ_variables$N_dys = ((ISE3_LQ_variables$rs_dys-1)/3)*100

###pain
#raw scores
DM_pain=apply(is.na(ISE3_LQ_variables[,c(11,21)]),1,sum)
rs_pain=apply(ISE3_LQ_variables[,c(11,21)],1,sum,na.rm=TRUE)
rs_pain=rs_pain/(2-DM_pain)
rs_pain[DM_pain>1] <- NA
ISE3_LQ_variables$rs_pain <- rs_pain

#normalized scores (2 items, will have value only when missing items <=1 for each individual)
ISE3_LQ_variables$N_pain = ((ISE3_LQ_variables$rs_pain-1)/3)*100

###fatigue
#raw scores
DM_fatigue=apply(is.na(ISE3_LQ_variables[,c(12,14,20)]),1,sum)
rs_fatigue=apply(ISE3_LQ_variables[,c(12,14,20)],1,sum,na.rm=TRUE)
rs_fatigue=rs_fatigue/(3-DM_fatigue)
rs_fatigue[DM_fatigue>1] <- NA
ISE3_LQ_variables$rs_fatigue <- rs_fatigue

#normalized scores (3 items, will have value only when missing items <=1 for each individual)
ISE3_LQ_variables$N_fatigue = ((ISE3_LQ_variables$rs_fatigue-1)/3)*100

### insomnia
#raw scores
rs_ins <- ISE3_LQ_variables$LQ11
ISE3_LQ_variables$rs_ins <- rs_ins

#normalized scores (1 item)
ISE3_LQ_variables$N_ins = ((ISE3_LQ_variables$rs_ins-1)/3)*100

###appetite loss
#raw scores
rs_al <- ISE3_LQ_variables$LQ13
ISE3_LQ_variables$rs_al <- rs_al

#normalized scores (1 item)
ISE3_LQ_variables$N_al = ((ISE3_LQ_variables$rs_al-1)/3)*100

###nausea/vomiting
#raw scores
DM_nau_vomit=apply(is.na(ISE3_LQ_variables[,c(16:17)]),1,sum)
rs_nau_vomit=apply(ISE3_LQ_variables[,c(16:17)],1,sum,na.rm=TRUE)
rs_nau_vomit=rs_nau_vomit/(2-DM_nau_vomit)
rs_nau_vomit[DM_nau_vomit>1] <- NA
ISE3_LQ_variables$rs_nau_vomit <- rs_nau_vomit

#normalized scores (2 items, will have value only when missing items <=1 for each individual)
ISE3_LQ_variables$N_nau_vomit = ((ISE3_LQ_variables$rs_nau_vomit-1)/3)*100

### constipation
#raw scores
rs_cons <- ISE3_LQ_variables$LQ16
ISE3_LQ_variables$rs_cons <- rs_cons

#normalized scores (1 item)
ISE3_LQ_variables$N_cons = ((ISE3_LQ_variables$rs_cons-1)/3)*100

### diarrhea
#raw scores
rs_dia <- ISE3_LQ_variables$LQ17
ISE3_LQ_variables$rs_dia <- rs_dia

#normalized scores (1 item)
ISE3_LQ_variables$N_dia = ((ISE3_LQ_variables$rs_dia-1)/3)*100

### fin.diff.
#raw scores
rs_fd <- ISE3_LQ_variables$LQ28
ISE3_LQ_variables$rs_fd <- rs_fd

#normalized scores (1 item)
ISE3_LQ_variables$N_fd = ((ISE3_LQ_variables$rs_fd-1)/3)*100


save(ISE3_LQ_variables, file = "U:/Studien/Empfind/Projekte/Quality_of_life_2020/yifeng/R data/ISE3_LQ_variables.Rdata")

