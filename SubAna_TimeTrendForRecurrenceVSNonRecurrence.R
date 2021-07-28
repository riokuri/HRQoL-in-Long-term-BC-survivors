#### SUBGROUP analysis - time trend for recurrence group vs non-re group ####

load("U:/Studien/Empfind/Projekte/Quality_of_life_2020/yifeng/R data/ISE1&3_merged.Rdata")
attach(ISE1_ISE3)
dt <- ISE1_ISE3

table(RezTuMe)

# for timepoint "follow-up" #
w1 <- aggregate(x=dt$N_qol, by=list(RezTuMe), FUN=mean, na.rm=T)
w2 <- aggregate(x=dt$N_pf, by=list(RezTuMe), FUN=mean, na.rm=T)
w3 <- aggregate(x=dt$N_rf, by=list(RezTuMe), FUN=mean, na.rm=T)
w4 <- aggregate(x=dt$N_cf, by=list(RezTuMe), FUN=mean, na.rm=T)
w5 <- aggregate(x=dt$N_ef, by=list(RezTuMe), FUN=mean, na.rm=T)
w6 <- aggregate(x=dt$N_sf, by=list(RezTuMe), FUN=mean, na.rm=T)

w7 <- aggregate(x=dt$N_dys, by=list(RezTuMe), FUN=mean, na.rm=T)
w8 <- aggregate(x=dt$N_pain, by=list(RezTuMe), FUN=mean, na.rm=T)
w9 <- aggregate(x=dt$N_fatigue, by=list(RezTuMe), FUN=mean, na.rm=T)
w10 <- aggregate(x=dt$N_ins, by=list(RezTuMe), FUN=mean, na.rm=T)
w11 <- aggregate(x=dt$N_al, by=list(RezTuMe), FUN=mean, na.rm=T)
w12 <- aggregate(x=dt$N_nau_vomit, by=list(RezTuMe), FUN=mean, na.rm=T)
w13 <- aggregate(x=dt$N_cons, by=list(RezTuMe), FUN=mean, na.rm=T)
w14 <- aggregate(x=dt$N_dia, by=list(RezTuMe), FUN=mean, na.rm=T)
w15 <- aggregate(x=dt$N_fd, by=list(RezTuMe), FUN=mean, na.rm=T)

dt10 <- data.frame(w1, w2, w3, w4, w5, w6, w7, w8, w9, w10, w11, w12, w13, w14, w15)
attach(dt10)

r10_qol <- dt10$x
r10_pf <- dt10$x.1
r10_rf <- dt10$x.2
r10_cf <- dt10$x.3
r10_ef <- dt10$x.4
r10_sf <- dt10$x.5
r10_dys <- dt10$x.6
r10_pain <- dt10$x.7
r10_fat <- dt10$x.8
r10_inso <- dt10$x.9
r10_al <- dt10$x.10
r10_nv <- dt10$x.11
r10_cons <- dt10$x.12
r10_diar <- dt10$x.13
r10_fin <- dt10$x.14

Subgroups <- c("No Recurrence/metastasis (n=256)", "Recurrence/metastasis (n=33)")
timepoint <- c("10", "10")
timepoint <- as.numeric(timepoint)

dt10_new <- data.frame(Subgroups, timepoint, r10_pf, r10_rf, r10_cf, r10_ef, r10_sf, r10_qol, r10_dys, r10_pain, r10_fat, r10_inso, r10_al, r10_nv, r10_cons, r10_diar, r10_fin)


# for timepoint "baseline" #

e1 <- aggregate(x=dt$QoLGHS_1, by=list(RezTuMe), FUN=mean, na.rm=T)
e2 <- aggregate(x=dt$QoLPhysF_1, by=list(RezTuMe), FUN=mean, na.rm=T)
e3 <- aggregate(x=dt$QoLRoleF_1, by=list(RezTuMe), FUN=mean, na.rm=T)
e4 <- aggregate(x=dt$QoLCogF_1, by=list(RezTuMe), FUN=mean, na.rm=T)
e5 <- aggregate(x=dt$QoLEmoF_1, by=list(RezTuMe), FUN=mean, na.rm=T)
e6 <- aggregate(x=dt$QoLSocF_1, by=list(RezTuMe), FUN=mean, na.rm=T)

e7 <- aggregate(x=dt$QoLDysp_1, by=list(RezTuMe), FUN=mean, na.rm=T)
e8 <- aggregate(x=dt$QoLPain_1, by=list(RezTuMe), FUN=mean, na.rm=T)
e9 <- aggregate(x=dt$QoLFat_1, by=list(RezTuMe), FUN=mean, na.rm=T)
e10 <- aggregate(x=dt$QoLInso_1, by=list(RezTuMe), FUN=mean, na.rm=T)
e11 <- aggregate(x=dt$QoLAppL_1, by=list(RezTuMe), FUN=mean, na.rm=T)
e12 <- aggregate(x=dt$QoLNV_1, by=list(RezTuMe), FUN=mean, na.rm=T)
e13 <- aggregate(x=dt$QoLCons_1, by=list(RezTuMe), FUN=mean, na.rm=T)
e14 <- aggregate(x=dt$QoLDiar_1, by=list(RezTuMe), FUN=mean, na.rm=T)
e15 <- aggregate(x=dt$QoLFin_1, by=list(RezTuMe), FUN=mean, na.rm=T)

dt1 <- data.frame(e1, e2, e3, e4, e5, e6, e7, e8, e9, e10, e11, e12, e13, e14, e15)
attach(dt1)

r1_qol <- dt1$x
r1_pf <- dt1$x.1
r1_rf <- dt1$x.2
r1_cf <- dt1$x.3
r1_ef <- dt1$x.4
r1_sf <- dt1$x.5
r1_dys <- dt1$x.6
r1_pain <- dt1$x.7
r1_fat <- dt1$x.8
r1_inso <- dt1$x.9
r1_al <- dt1$x.10
r1_nv <- dt1$x.11
r1_cons <- dt1$x.12
r1_diar <- dt1$x.13
r1_fin <- dt1$x.14

timepoint_ba <- c("1", "1")
timepoint_ba <- as.numeric(timepoint_ba)

dt1_new <- data.frame(Subgroups, timepoint_ba, r1_pf, r1_rf, r1_cf, r1_ef, r1_sf, r1_qol, r1_dys, r1_pain, r1_fat, r1_inso, r1_al, r1_nv, r1_cons, r1_diar, r1_fin)

# for beginning of RT

b1 <- aggregate(x=dt$QoLGHS_2, by=list(RezTuMe), FUN=mean, na.rm=T)
b2 <- aggregate(x=dt$QoLPhysF_2, by=list(RezTuMe), FUN=mean, na.rm=T)
b3 <- aggregate(x=dt$QoLRoleF_2, by=list(RezTuMe), FUN=mean, na.rm=T)
b4 <- aggregate(x=dt$QoLCogF_2, by=list(RezTuMe), FUN=mean, na.rm=T)
b5 <- aggregate(x=dt$QoLEmoF_2, by=list(RezTuMe), FUN=mean, na.rm=T)
b6 <- aggregate(x=dt$QoLSocF_2, by=list(RezTuMe), FUN=mean, na.rm=T)

b7 <- aggregate(x=dt$QoLDysp_2, by=list(RezTuMe), FUN=mean, na.rm=T)
b8 <- aggregate(x=dt$QoLPain_2, by=list(RezTuMe), FUN=mean, na.rm=T)
b9 <- aggregate(x=dt$QoLFat_2, by=list(RezTuMe), FUN=mean, na.rm=T)
b10 <- aggregate(x=dt$QoLInso_2, by=list(RezTuMe), FUN=mean, na.rm=T)
b11 <- aggregate(x=dt$QoLAppL_2, by=list(RezTuMe), FUN=mean, na.rm=T)
b12 <- aggregate(x=dt$QoLNV_2, by=list(RezTuMe), FUN=mean, na.rm=T)
b13 <- aggregate(x=dt$QoLCons_2, by=list(RezTuMe), FUN=mean, na.rm=T)
b14 <- aggregate(x=dt$QoLDiar_2, by=list(RezTuMe), FUN=mean, na.rm=T)
b15 <- aggregate(x=dt$QoLFin_2, by=list(RezTuMe), FUN=mean, na.rm=T)

dt2 <- data.frame(b1, b2, b3, b4, b5, b6, b7, b8, b9, b10, b11, b12, b13, b14, b15)
attach(dt2)

r2_qol <- dt2$x
r2_pf <- dt2$x.1
r2_rf <- dt2$x.2
r2_cf <- dt2$x.3
r2_ef <- dt2$x.4
r2_sf <- dt2$x.5
r2_dys <- dt2$x.6
r2_pain <- dt2$x.7
r2_fat <- dt2$x.8
r2_inso <- dt2$x.9
r2_al <- dt2$x.10
r2_nv <- dt2$x.11
r2_cons <- dt2$x.12
r2_diar <- dt2$x.13
r2_fin <- dt2$x.14

timepoint_be <- c("2", "2")
timepoint_be <- as.numeric(timepoint_be)

dt2_new <- data.frame(Subgroups, timepoint_be, r2_pf, r2_rf, r2_cf, r2_ef, r2_sf, r2_qol, r2_dys, r2_pain, r2_fat, r2_inso, r2_al, r2_nv, r2_cons, r2_diar, r2_fin)

# during RT

d1 <- aggregate(x=dt$QoLGHS_3, by=list(RezTuMe), FUN=mean, na.rm=T)
d2 <- aggregate(x=dt$QoLPhysF_3, by=list(RezTuMe), FUN=mean, na.rm=T)
d3 <- aggregate(x=dt$QoLRoleF_3, by=list(RezTuMe), FUN=mean, na.rm=T)
d4 <- aggregate(x=dt$QoLCogF_3, by=list(RezTuMe), FUN=mean, na.rm=T)
d5 <- aggregate(x=dt$QoLEmoF_3, by=list(RezTuMe), FUN=mean, na.rm=T)
d6 <- aggregate(x=dt$QoLSocF_3, by=list(RezTuMe), FUN=mean, na.rm=T)

d7 <- aggregate(x=dt$QoLDysp_3, by=list(RezTuMe), FUN=mean, na.rm=T)
d8 <- aggregate(x=dt$QoLPain_3, by=list(RezTuMe), FUN=mean, na.rm=T)
d9 <- aggregate(x=dt$QoLFat_3, by=list(RezTuMe), FUN=mean, na.rm=T)
d10 <- aggregate(x=dt$QoLInso_3, by=list(RezTuMe), FUN=mean, na.rm=T)
d11 <- aggregate(x=dt$QoLAppL_3, by=list(RezTuMe), FUN=mean, na.rm=T)
d12 <- aggregate(x=dt$QoLNV_3, by=list(RezTuMe), FUN=mean, na.rm=T)
d13 <- aggregate(x=dt$QoLCons_3, by=list(RezTuMe), FUN=mean, na.rm=T)
d14 <- aggregate(x=dt$QoLDiar_3, by=list(RezTuMe), FUN=mean, na.rm=T)
d15 <- aggregate(x=dt$QoLFin_3, by=list(RezTuMe), FUN=mean, na.rm=T)

dt3 <- data.frame(d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12, d13, d14, d15)
attach(dt3)

r3_qol <- dt3$x
r3_pf <- dt3$x.1
r3_rf <- dt3$x.2
r3_cf <- dt3$x.3
r3_ef <- dt3$x.4
r3_sf <- dt3$x.5
r3_dys <- dt3$x.6
r3_pain <- dt3$x.7
r3_fat <- dt3$x.8
r3_inso <- dt3$x.9
r3_al <- dt3$x.10
r3_nv <- dt3$x.11
r3_cons <- dt3$x.12
r3_diar <- dt3$x.13
r3_fin <- dt3$x.14

timepoint_du <- c("3", "3")
timepoint_du <- as.numeric(timepoint_du)

dt3_new <- data.frame(Subgroups, timepoint_du, r3_pf, r3_rf, r3_cf, r3_ef, r3_sf, r3_qol, r3_dys, r3_pain, r3_fat, r3_inso, r3_al, r3_nv, r3_cons, r3_diar, r3_fin)

# end of RT

n1 <- aggregate(x=dt$QoLGHS_4, by=list(RezTuMe), FUN=mean, na.rm=T)
n2 <- aggregate(x=dt$QoLPhysF_4, by=list(RezTuMe), FUN=mean, na.rm=T)
n3 <- aggregate(x=dt$QoLRoleF_4, by=list(RezTuMe), FUN=mean, na.rm=T)
n4 <- aggregate(x=dt$QoLCogF_4, by=list(RezTuMe), FUN=mean, na.rm=T)
n5 <- aggregate(x=dt$QoLEmoF_4, by=list(RezTuMe), FUN=mean, na.rm=T)
n6 <- aggregate(x=dt$QoLSocF_4, by=list(RezTuMe), FUN=mean, na.rm=T)

n7 <- aggregate(x=dt$QoLDysp_4, by=list(RezTuMe), FUN=mean, na.rm=T)
n8 <- aggregate(x=dt$QoLPain_4, by=list(RezTuMe), FUN=mean, na.rm=T)
n9 <- aggregate(x=dt$QoLFat_4, by=list(RezTuMe), FUN=mean, na.rm=T)
n10 <- aggregate(x=dt$QoLInso_4, by=list(RezTuMe), FUN=mean, na.rm=T)
n11 <- aggregate(x=dt$QoLAppL_4, by=list(RezTuMe), FUN=mean, na.rm=T)
n12 <- aggregate(x=dt$QoLNV_4, by=list(RezTuMe), FUN=mean, na.rm=T)
n13 <- aggregate(x=dt$QoLCons_4, by=list(RezTuMe), FUN=mean, na.rm=T)
n14 <- aggregate(x=dt$QoLDiar_4, by=list(RezTuMe), FUN=mean, na.rm=T)
n15 <- aggregate(x=dt$QoLFin_4, by=list(RezTuMe), FUN=mean, na.rm=T)

dt4 <- data.frame(n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12, n13, n14, n15)
attach(dt4)

r4_qol <- dt4$x
r4_pf <- dt4$x.1
r4_rf <- dt4$x.2
r4_cf <- dt4$x.3
r4_ef <- dt4$x.4
r4_sf <- dt4$x.5
r4_dys <- dt4$x.6
r4_pain <- dt4$x.7
r4_fat <- dt4$x.8
r4_inso <- dt4$x.9
r4_al <- dt4$x.10
r4_nv <- dt4$x.11
r4_cons <- dt4$x.12
r4_diar <- dt4$x.13
r4_fin <- dt4$x.14

timepoint_en <- c("4", "4")
timepoint_en <- as.numeric(timepoint_en)

dt4_new <- data.frame(Subgroups, timepoint_en, r4_pf, r4_rf, r4_cf, r4_ef, r4_sf, r4_qol, r4_dys, r4_pain, r4_fat, r4_inso, r4_al, r4_nv, r4_cons, r4_diar, r4_fin)

# 6 weeks after RT

a1 <- aggregate(x=dt$QoLGHS_5, by=list(RezTuMe), FUN=mean, na.rm=T)
a2 <- aggregate(x=dt$QoLPhysF_5, by=list(RezTuMe), FUN=mean, na.rm=T)
a3 <- aggregate(x=dt$QoLRoleF_5, by=list(RezTuMe), FUN=mean, na.rm=T)
a4 <- aggregate(x=dt$QoLCogF_5, by=list(RezTuMe), FUN=mean, na.rm=T)
a5 <- aggregate(x=dt$QoLEmoF_5, by=list(RezTuMe), FUN=mean, na.rm=T)
a6 <- aggregate(x=dt$QoLSocF_5, by=list(RezTuMe), FUN=mean, na.rm=T)

a7 <- aggregate(x=dt$QoLDysp_5, by=list(RezTuMe), FUN=mean, na.rm=T)
a8 <- aggregate(x=dt$QoLPain_5, by=list(RezTuMe), FUN=mean, na.rm=T)
a9 <- aggregate(x=dt$QoLFat_5, by=list(RezTuMe), FUN=mean, na.rm=T)
a10 <- aggregate(x=dt$QoLInso_5, by=list(RezTuMe), FUN=mean, na.rm=T)
a11 <- aggregate(x=dt$QoLAppL_5, by=list(RezTuMe), FUN=mean, na.rm=T)
a12 <- aggregate(x=dt$QoLNV_5, by=list(RezTuMe), FUN=mean, na.rm=T)
a13 <- aggregate(x=dt$QoLCons_5, by=list(RezTuMe), FUN=mean, na.rm=T)
a14 <- aggregate(x=dt$QoLDiar_5, by=list(RezTuMe), FUN=mean, na.rm=T)
a15 <- aggregate(x=dt$QoLFin_5, by=list(RezTuMe), FUN=mean, na.rm=T)

dt5 <- data.frame(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11, a12, a13, a14, a15)
attach(dt5)

r5_qol <- dt5$x
r5_pf <- dt5$x.1
r5_rf <- dt5$x.2
r5_cf <- dt5$x.3
r5_ef <- dt5$x.4
r5_sf <- dt5$x.5
r5_dys <- dt5$x.6
r5_pain <- dt5$x.7
r5_fat <- dt5$x.8
r5_inso <- dt5$x.9
r5_al <- dt5$x.10
r5_nv <- dt5$x.11
r5_cons <- dt5$x.12
r5_diar <- dt5$x.13
r5_fin <- dt5$x.14

timepoint_af <- c("5", "5")
timepoint_af <- as.numeric(timepoint_af)

dt5_new <- data.frame(Subgroups, timepoint_af, r5_pf, r5_rf, r5_cf, r5_ef, r5_sf, r5_qol, r5_dys, r5_pain, r5_fat, r5_inso, r5_al, r5_nv, r5_cons, r5_diar, r5_fin)

## 
### prepare a new dataset

Subgroups = c("All ISE-3 paients (n=292)", "No recurrence/metastasis (n=256)", "Recurrence/metastasis (n=33)", "All ISE-3 paients (n=292)", "No recurrence/metastasis (n=256)", "Recurrence/metastasis (n=33)", "All ISE-3 paients (n=292)", "No recurrence/metastasis (n=256)", "Recurrence/metastasis (n=33)", "All ISE-3 paients (n=292)", "No recurrence/metastasis (n=256)", "Recurrence/metastasis (n=33)", "All ISE-3 paients (n=292)", "No recurrence/metastasis (n=256)", "Recurrence/metastasis (n=33)", "All ISE-3 paients (n=292)", "No recurrence/metastasis (n=256)", "Recurrence/metastasis (n=33)")

Time =  c("1", "1", "1", "2", "2", "2", "3", "3", "3", "4", "4", "4", "5", "5", "5", "10", "10", "10")

Time <- as.numeric(Time)


ByRecurrence <- data.frame(Subgroups, Time, 
                           PF = c(c(mean(dt$QoLPhysF_1, na.rm=T), r1_pf), c(mean(dt$QoLPhysF_2, na.rm=T), r2_pf), c(mean(dt$QoLPhysF_3, na.rm=T), r3_pf), c(mean(dt$QoLPhysF_4, na.rm=T), r4_pf), c(mean(dt$QoLPhysF_5, na.rm=T), r5_pf), c(mean(dt$N_pf, na.rm=T), r10_pf)),
                           RF = c(c(mean(dt$QoLRoleF_1, na.rm=T), r1_rf), c(mean(dt$QoLRoleF_2, na.rm=T), r2_rf), c(mean(dt$QoLRoleF_3, na.rm=T), r3_rf), c(mean(dt$QoLRoleF_4, na.rm=T), r4_rf), c(mean(dt$QoLRoleF_5, na.rm=T), r5_rf), c(mean(dt$N_rf, na.rm=T), r10_rf)),
                           CF = c(c(mean(dt$QoLCogF_1, na.rm=T), r1_cf), c(mean(dt$QoLCogF_2, na.rm=T), r2_cf), c(mean(dt$QoLCogF_3, na.rm=T), r3_cf), c(mean(dt$QoLCogF_4, na.rm=T), r4_cf), c(mean(dt$QoLCogF_5, na.rm=T), r5_cf), c(mean(dt$N_cf, na.rm=T), r10_cf)),
                           EF = c(c(mean(dt$QoLEmoF_1, na.rm=T), r1_ef), c(mean(dt$QoLEmoF_2, na.rm=T), r2_ef), c(mean(dt$QoLEmoF_3, na.rm=T), r3_ef), c(mean(dt$QoLEmoF_4, na.rm=T), r4_ef), c(mean(dt$QoLEmoF_5, na.rm=T), r5_ef), c(mean(dt$N_ef, na.rm=T), r10_ef)),
                           SF = c(c(mean(dt$QoLSocF_1, na.rm=T), r1_sf), c(mean(dt$QoLSocF_2, na.rm=T), r2_sf), c(mean(dt$QoLSocF_3, na.rm=T), r3_sf), c(mean(dt$QoLSocF_4, na.rm=T), r4_sf), c(mean(dt$QoLSocF_5, na.rm=T), r5_sf), c(mean(dt$N_sf, na.rm=T), r10_sf)),
                           QoL = c(c(mean(dt$QoLGHS_1, na.rm=T), r1_qol), c(mean(dt$QoLGHS_2, na.rm=T), r2_qol), c(mean(dt$QoLGHS_3, na.rm=T), r3_qol), c(mean(dt$QoLGHS_4, na.rm=T), r4_qol), c(mean(dt$QoLGHS_5, na.rm=T), r5_qol), c(mean(dt$N_qol, na.rm=T), r10_qol)),
                           Dys = c(c(mean(dt$QoLDysp_1, na.rm=T), r1_dys), c(mean(dt$QoLDysp_2, na.rm=T), r2_dys), c(mean(dt$QoLDysp_3, na.rm=T), r3_dys), c(mean(dt$QoLDysp_4, na.rm=T), r4_dys), c(mean(dt$QoLDysp_5, na.rm=T), r5_dys), c(mean(dt$N_dys, na.rm=T), r10_dys)),
                           Pain = c(c(mean(dt$QoLPain_1, na.rm=T), r1_pain), c(mean(dt$QoLPain_2, na.rm=T), r2_pain), c(mean(dt$QoLPain_3, na.rm=T), r3_pain), c(mean(dt$QoLPain_4, na.rm=T), r4_pain), c(mean(dt$QoLPain_5, na.rm=T), r5_pain), c(mean(dt$N_pain, na.rm=T), r10_pain)),
                           Fati = c(c(mean(dt$QoLFat_1, na.rm=T), r1_fat), c(mean(dt$QoLFat_2, na.rm=T), r2_fat), c(mean(dt$QoLFat_3, na.rm=T), r3_fat), c(mean(dt$QoLFat_4, na.rm=T), r4_fat), c(mean(dt$QoLFat_5, na.rm=T), r5_fat), c(mean(dt$N_fatigue, na.rm=T), r10_fat)),
                           Inso = c(c(mean(dt$QoLInso_1, na.rm=T), r1_inso), c(mean(dt$QoLInso_2, na.rm=T), r2_inso), c(mean(dt$QoLInso_3, na.rm=T), r3_inso), c(mean(dt$QoLInso_4, na.rm=T), r4_inso), c(mean(dt$QoLInso_5, na.rm=T), r5_inso), c(mean(dt$N_ins, na.rm=T), r10_inso)),
                           AL = c(c(mean(dt$QoLAppL_1, na.rm=T), r1_al), c(mean(dt$QoLAppL_2, na.rm=T), r2_al), c(mean(dt$QoLAppL_3, na.rm=T), r3_al), c(mean(dt$QoLAppL_4, na.rm=T), r4_al), c(mean(dt$QoLAppL_5, na.rm=T), r5_al), c(mean(dt$N_al, na.rm=T), r10_al)),
                           NV = c(c(mean(dt$QoLNV_1, na.rm=T), r1_nv), c(mean(dt$QoLNV_2, na.rm=T), r2_nv), c(mean(dt$QoLNV_3, na.rm=T), r3_nv), c(mean(dt$QoLNV_4, na.rm=T), r4_nv), c(mean(dt$QoLNV_5, na.rm=T), r5_nv), c(mean(dt$N_nau_vomit, na.rm=T), r10_nv)),
                           Cons = c(c(mean(dt$QoLCons_1, na.rm=T), r1_cons), c(mean(dt$QoLCons_2, na.rm=T), r2_cons), c(mean(dt$QoLCons_3, na.rm=T), r3_cons), c(mean(dt$QoLCons_4, na.rm=T), r4_cons), c(mean(dt$QoLCons_5, na.rm=T), r5_cons), c(mean(dt$N_cons, na.rm=T), r10_cons)),
                           Diar = c(c(mean(dt$QoLDiar_1, na.rm=T), r1_diar), c(mean(dt$QoLDiar_2, na.rm=T), r2_diar), c(mean(dt$QoLDiar_3, na.rm=T), r3_diar), c(mean(dt$QoLDiar_4, na.rm=T), r4_diar), c(mean(dt$QoLDiar_5, na.rm=T), r5_diar), c(mean(dt$N_dia, na.rm=T), r10_diar)),
                           Fin = c(c(mean(dt$QoLFin_1, na.rm=T), r1_fin), c(mean(dt$QoLFin_2, na.rm=T), r2_fin), c(mean(dt$QoLFin_3, na.rm=T), r3_fin), c(mean(dt$QoLFin_4, na.rm=T), r4_fin), c(mean(dt$QoLFin_5, na.rm=T), r5_fin), c(mean(dt$N_fd, na.rm=T), r10_fin)))

### Plotting
## Functioning scale & GHS/QoL scale
# physical functioning
library(ggplot2)
library(cowplot)
pf <- ggplot(ByRecurrence, aes(x=Time, y=PF, shape=Subgroups, color=Subgroups))+
  geom_point()+
  geom_line()+
  scale_x_continuous(limits = c(0,11),
                     expand = c(0,0),
                     breaks = seq(0,11,1))+
  scale_color_manual(values=c("plum", "gold", "aquamarine4"))+
  ylim(50,100)+
  labs(x="Timepoints", y="Mean score", title = "Physical functioning")+
  theme(legend.position = c(.95, .95),         
        legend.justification = c("right", "top"),         
        legend.box.just = "right",         
        legend.margin = margin(6, 6, 6, 6))

pf

rlang::last_error()

# role functioning
rf <- ggplot(ByRecurrence, aes(x=Time, y=RF, shape=Subgroups, color=Subgroups))+
  geom_point()+
  geom_line()+
  scale_x_continuous(limits = c(0,11),
                     expand = c(0,0),
                     breaks = seq(0,11,1))+
  scale_color_manual(values=c("plum", "gold", "aquamarine4"))+
  ylim(50,100)+
  labs(x="Timepoints", y="Mean score", title = "Role functioning")+
  theme(legend.position = c(.95, .95),         
        legend.justification = c("right", "top"),         
        legend.box.just = "right",         
        legend.margin = margin(6, 6, 6, 6))

rf

# cognitive functioning
cf <- ggplot(ByRecurrence, aes(x=Time, y=CF, shape=Subgroups, color=Subgroups))+
  geom_point()+
  geom_line()+
  scale_x_continuous(limits = c(0,11),
                     expand = c(0,0),
                     breaks = seq(0,11,1))+
  scale_color_manual(values=c("plum", "gold", "aquamarine4"))+
  ylim(50,100)+
  labs(x="Timepoints", y="Mean score", title = "Cognitive functioning")+
  theme(legend.position = c(.95, .95),         
        legend.justification = c("right", "top"),         
        legend.box.just = "right",         
        legend.margin = margin(6, 6, 6, 6))

cf

# emotional functioning
ef <- ggplot(ByRecurrence, aes(x=Time, y=EF, shape=Subgroups, color=Subgroups))+
  geom_point()+
  geom_line()+
  scale_x_continuous(limits = c(0,11),
                     expand = c(0,0),
                     breaks = seq(0,11,1))+
  scale_color_manual(values=c("plum", "gold", "aquamarine4"))+
  ylim(50,100)+
  labs(x="Timepoints", y="Mean score", title = "Emotional functioning")+
  theme(legend.position = c(.95, .95),         
        legend.justification = c("right", "top"),         
        legend.box.just = "right",         
        legend.margin = margin(6, 6, 6, 6))

ef

# social functiong
sf <- ggplot(ByRecurrence, aes(x=Time, y=SF, shape=Subgroups, color=Subgroups))+
  geom_point()+
  geom_line()+
  scale_x_continuous(limits = c(0,11),
                     expand = c(0,0),
                     breaks = seq(0,11,1))+
  scale_color_manual(values=c("plum", "gold", "aquamarine4"))+
  ylim(50,100)+
  labs(x="Timepoints", y="Mean score", title = "Social functioning")+
  theme(legend.position = c(.95, .95),         
        legend.justification = c("right", "top"),         
        legend.box.just = "right",         
        legend.margin = margin(6, 6, 6, 6))

sf

# GHS/QoL
GHS <- ggplot(ByRecurrence, aes(x=Time, y=QoL, shape=Subgroups, color=Subgroups))+
  geom_point()+
  geom_line()+
  scale_x_continuous(limits = c(0,11),
                     expand = c(0,0),
                     breaks = seq(0,11,1))+
  scale_color_manual(values=c("plum", "gold", "aquamarine4"))+
  ylim(50,100)+
  labs(x="Timepoints", y="Mean score", title = "GHS/QoL")+
  theme(legend.position = c(.95, .95),         
        legend.justification = c("right", "top"),         
        legend.box.just = "right",         
        legend.margin = margin(6, 6, 6, 6))

GHS

## Symptom scale
# dyspnea

dys <- ggplot(ByRecurrence, aes(x=Time, y=Dys, shape=Subgroups, color=Subgroups))+
  geom_point()+
  geom_line()+
  scale_x_continuous(limits = c(0,11),
                     expand = c(0,0),
                     breaks = seq(0,11,1))+
  scale_color_manual(values=c("plum", "gold", "aquamarine4"))+
  ylim(0,55)+
  labs(x="Timepoints", y="Mean score", title = "Dyspnea")+
  theme(legend.position = c(.95, .95),         
        legend.justification = c("right", "top"),         
        legend.box.just = "right",         
        legend.margin = margin(6, 6, 6, 6))

dys

# pain
pain <- ggplot(ByRecurrence, aes(x=Time, y=Pain, shape=Subgroups, color=Subgroups))+
  geom_point()+
  geom_line()+
  scale_x_continuous(limits = c(0,11),
                     expand = c(0,0),
                     breaks = seq(0,11,1))+
  scale_color_manual(values=c("plum", "gold", "aquamarine4"))+
  ylim(0,55)+
  labs(x="Timepoints", y="Mean score", title = "Pain")+
  theme(legend.position = c(.95, .95),         
        legend.justification = c("right", "top"),         
        legend.box.just = "right",         
        legend.margin = margin(6, 6, 6, 6))

pain

# fatigue
fatigue <- ggplot(ByRecurrence, aes(x=Time, y=Fati, shape=Subgroups, color=Subgroups))+
  geom_point()+
  geom_line()+
  scale_x_continuous(limits = c(0,11),
                     expand = c(0,0),
                     breaks = seq(0,11,1))+
  scale_color_manual(values=c("plum", "gold", "aquamarine4"))+
  ylim(0,55)+
  labs(x="Timepoints", y="Mean score", title = "Fatigue")+
  theme(legend.position = c(.95, .95),         
        legend.justification = c("right", "top"),         
        legend.box.just = "right",         
        legend.margin = margin(6, 6, 6, 6))

fatigue

# insomnia
ins <- ggplot(ByRecurrence, aes(x=Time, y=Inso, shape=Subgroups, color=Subgroups))+
  geom_point()+
  geom_line()+
  scale_x_continuous(limits = c(0,11),
                     expand = c(0,0),
                     breaks = seq(0,11,1))+
  scale_color_manual(values=c("plum", "gold", "aquamarine4"))+
  labs(x="Timepoints", y="Mean score", title = "Insomnia")+
  ylim(0,55)+
  theme(legend.position = c(.95, .30),         
        legend.justification = c("right", "top"),         
        legend.box.just = "right",         
        legend.margin = margin(6, 6, 6, 6))

ins

# appetitie loss
al <- ggplot(ByRecurrence, aes(x=Time, y=AL, shape=Subgroups, color=Subgroups))+
  geom_point()+
  geom_line()+
  scale_x_continuous(limits = c(0,11),
                     expand = c(0,0),
                     breaks = seq(0,11,1))+
  scale_color_manual(values=c("plum", "gold", "aquamarine4"))+
  labs(x="Timepoints", y="Mean score", title = "Appetite loss")+
  ylim(0,55)+
  theme(legend.position = c(.95, .95),         
        legend.justification = c("right", "top"),         
        legend.box.just = "right",         
        legend.margin = margin(6, 6, 6, 6))

al

# nausea/vomiting
nv <- ggplot(ByRecurrence, aes(x=Time, y=NV, shape=Subgroups, color=Subgroups))+
  geom_point()+
  geom_line()+
  scale_x_continuous(limits = c(0,11),
                     expand = c(0,0),
                     breaks = seq(0,11,1))+
  scale_color_manual(values=c("plum", "gold", "aquamarine4"))+
  labs(x="Timepoints", y="Mean score", title = "Nausea/vomiting")+
  ylim(0,55)+
  theme(legend.position = c(.95, .95),         
        legend.justification = c("right", "top"),         
        legend.box.just = "right",         
        legend.margin = margin(6, 6, 6, 6))

nv

# constipation
cons <- ggplot(ByRecurrence, aes(x=Time, y=Cons, shape=Subgroups, color=Subgroups))+
  geom_point()+
  geom_line()+
  scale_x_continuous(limits = c(0,11),
                     expand = c(0,0),
                     breaks = seq(0,11,1))+
  scale_color_manual(values=c("plum", "gold", "aquamarine4"))+
  labs(x="Timepoints", y="Mean score", title = "Constipation")+
  ylim(0,55)+
  theme(legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6))

cons

# diarrhea
diar <- ggplot(ByRecurrence, aes(x=Time, y=Diar, shape=Subgroups, color=Subgroups))+
  geom_point()+
  geom_line()+
  scale_x_continuous(limits = c(0,11),
                     expand = c(0,0),
                     breaks = seq(0,11,1))+
  scale_color_manual(values=c("plum", "gold", "aquamarine4"))+
  ylim(0,55)+
  labs(x="Timepoints", y="Mean score", title = "Diarrhea")+
  theme(legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6))

diar

# fin.diff.
fd <- ggplot(ByRecurrence, aes(x=Time, y=Fin, shape=Subgroups, color=Subgroups))+
  geom_point()+
  geom_line()+
  scale_x_continuous(limits = c(0,11),
                     expand = c(0,0),
                     breaks = seq(0,11,1))+
  scale_color_manual(values=c("plum", "gold", "aquamarine4"))+
  ylim(0,55)+
  labs(x="Timepoints", y="Mean score", title = "Financial difficulty")+
  theme(legend.position = c(.95, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6))

fd

### wilcoxon test
View(ISE1_ISE3_1)
ISE1_ISE3_1$RezTuMe <- as.factor(RezTuMe)

## tests 
# at tp1
wilcox.test(QoLGHS_1~RezTuMe)

wilcox.test(QoLPhysF_1~RezTuMe) 
wilcox.test(QoLRoleF_1~RezTuMe) 
wilcox.test(QoLCogF_1~RezTuMe) 
wilcox.test(QoLEmoF_1~RezTuMe)
wilcox.test(QoLSocF_1~RezTuMe)

wilcox.test(QoLDysp_1~RezTuMe)
wilcox.test(QoLPain_1~RezTuMe) 
wilcox.test(QoLFat_1~RezTuMe) 
wilcox.test(QoLInso_1~RezTuMe) 
wilcox.test(QoLAppL_1~RezTuMe)
wilcox.test(QoLNV_1~RezTuMe)
wilcox.test(QoLCons_1~RezTuMe) 
wilcox.test(QoLDiar_1~RezTuMe) 
wilcox.test(QoLFin_1~RezTuMe)

# at tp2
wilcox.test(QoLGHS_2~RezTuMe)

wilcox.test(QoLPhysF_2~RezTuMe) 
wilcox.test(QoLRoleF_2~RezTuMe) 
wilcox.test(QoLCogF_2~RezTuMe) 
wilcox.test(QoLEmoF_2~RezTuMe)
wilcox.test(QoLSocF_2~RezTuMe)

wilcox.test(QoLDysp_2~RezTuMe)
wilcox.test(QoLPain_2~RezTuMe) 
wilcox.test(QoLFat_2~RezTuMe) 
wilcox.test(QoLInso_2~RezTuMe) 
wilcox.test(QoLAppL_2~RezTuMe)
wilcox.test(QoLNV_2~RezTuMe)
wilcox.test(QoLCons_2~RezTuMe) 
wilcox.test(QoLDiar_2~RezTuMe) 
wilcox.test(QoLFin_2~RezTuMe)

# at tp3
wilcox.test(QoLGHS_3~RezTuMe)

wilcox.test(QoLPhysF_3~RezTuMe) 
wilcox.test(QoLRoleF_3~RezTuMe) 
wilcox.test(QoLCogF_3~RezTuMe) 
wilcox.test(QoLEmoF_3~RezTuMe)
wilcox.test(QoLSocF_3~RezTuMe)

wilcox.test(QoLDysp_3~RezTuMe)
wilcox.test(QoLPain_3~RezTuMe) 
wilcox.test(QoLFat_3~RezTuMe) 
wilcox.test(QoLInso_3~RezTuMe) 
wilcox.test(QoLAppL_3~RezTuMe)
wilcox.test(QoLNV_3~RezTuMe)
wilcox.test(QoLCons_3~RezTuMe) 
wilcox.test(QoLDiar_3~RezTuMe) 
wilcox.test(QoLFin_3~RezTuMe)

# at tp4
wilcox.test(QoLGHS_4~RezTuMe)

wilcox.test(QoLPhysF_4~RezTuMe) 
wilcox.test(QoLRoleF_4~RezTuMe) 
wilcox.test(QoLCogF_4~RezTuMe) 
wilcox.test(QoLEmoF_4~RezTuMe)
wilcox.test(QoLSocF_4~RezTuMe)

wilcox.test(QoLDysp_4~RezTuMe)
wilcox.test(QoLPain_4~RezTuMe) 
wilcox.test(QoLFat_4~RezTuMe) 
wilcox.test(QoLInso_4~RezTuMe) 
wilcox.test(QoLAppL_4~RezTuMe)
wilcox.test(QoLNV_4~RezTuMe)
wilcox.test(QoLCons_4~RezTuMe) 
wilcox.test(QoLDiar_4~RezTuMe) 
wilcox.test(QoLFin_4~RezTuMe)

# at tp5
wilcox.test(QoLGHS_5~RezTuMe)

wilcox.test(QoLPhysF_5~RezTuMe) 
wilcox.test(QoLRoleF_5~RezTuMe) 
wilcox.test(QoLCogF_5~RezTuMe) 
wilcox.test(QoLEmoF_5~RezTuMe)
wilcox.test(QoLSocF_5~RezTuMe)

wilcox.test(QoLDysp_5~RezTuMe)
wilcox.test(QoLPain_5~RezTuMe) 
wilcox.test(QoLFat_5~RezTuMe) 
wilcox.test(QoLInso_5~RezTuMe) 
wilcox.test(QoLAppL_5~RezTuMe)
wilcox.test(QoLNV_5~RezTuMe)
wilcox.test(QoLCons_5~RezTuMe) 
wilcox.test(QoLDiar_5~RezTuMe) 
wilcox.test(QoLFin_5~RezTuMe)

# at tp10
wilcox.test(N_qol~RezTuMe)

wilcox.test(N_pf~RezTuMe) 
wilcox.test(N_rf~RezTuMe) 
wilcox.test(N_cf~RezTuMe) 
wilcox.test(N_ef~RezTuMe)
wilcox.test(N_sf~RezTuMe)

wilcox.test(N_dys~RezTuMe)
wilcox.test(N_pain~RezTuMe) 
wilcox.test(N_fatigue~RezTuMe) 
wilcox.test(N_ins~RezTuMe) 
wilcox.test(N_al~RezTuMe)
wilcox.test(N_nau_vomit~RezTuMe)
wilcox.test(N_cons~RezTuMe) 
wilcox.test(N_dia~RezTuMe) 
wilcox.test(N_fd~RezTuMe)

