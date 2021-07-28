##### Sensitivity Analysis - 3 lines in 1 figure: 
### patients who attened the FU (ISE-3), patients who attended baseline, end of RT and FU (239), full responders 6 timepoints (189)

load("U:/Studien/Empfind/Projekte/Quality_of_life_2020/yifeng/R data/PatientsWhoAttendedBaseline&endRT&ISE3.Rdata")
load("U:/Studien/Empfind/Projekte/Quality_of_life_2020/yifeng/R data/fullresponder.Rdata")
load("U:/Studien/Empfind/Projekte/Quality_of_life_2020/yifeng/R data/ISE1&3_merged.Rdata")

dten <- baseline_ise3_endRT
dtfull <- fullresponder
dtise3 <- ISE1_ISE3

attach(dten)
attach(dtfull)
attach(dtise3)

### prepare a new dataset

Groups = c("Patients who had attended baseline, end of RT and ISE-3 (n=239)", "Patients who had attended baseline, end of RT and ISE-3 (n=239)", "Patients who had attended baseline, end of RT and ISE-3 (n=239)", "Patients who had attended baseline, end of RT and ISE-3 (n=239)", "Patients who had attended baseline, end of RT and ISE-3 (n=239)", "Patients who had attended baseline, end of RT and ISE-3 (n=239)", "Patients who have attended all 6 timepoints, full respndors (n=189)", "Patients who have attended all 6 timepoints, full respndors (n=189)", "Patients who have attended all 6 timepoints, full respndors (n=189)", "Patients who have attended all 6 timepoints, full respndors (n=189)", "Patients who have attended all 6 timepoints, full respndors (n=189)", "Patients who have attended all 6 timepoints, full respndors (n=189)", "Patients who have attended the 10-year follow-up, the ISE-3 cohort (n=292)", "Patients who have attended the 10-year follow-up, the ISE-3 cohort (n=292)", "Patients who have attended the 10-year follow-up, the ISE-3 cohort (n=292)", "Patients who have attended the 10-year follow-up, the ISE-3 cohort (n=292)", "Patients who have attended the 10-year follow-up, the ISE-3 cohort (n=292)", "Patients who have attended the 10-year follow-up, the ISE-3 cohort (n=292)")

Time =  c("1", "2", "3", "4", "5", "10", "1", "2", "3", "4", "5", "10", "1", "2", "3", "4", "5", "10")
Time <- as.numeric(Time)

Mean_pf = c(mean(dten$QoLPhysF_1, na.rm = T), mean(dten$QoLPhysF_2, na.rm = T), mean(dten$QoLPhysF_3, na.rm = T), mean(dten$QoLPhysF_4, na.rm = T), mean(dten$QoLPhysF_5, na.rm = T), mean(dten$N_pf, na.rm = T),
            mean(dtfull$QoLPhysF_1, na.rm = T), mean(dtfull$QoLPhysF_2, na.rm = T), mean(dtfull$QoLPhysF_3, na.rm = T), mean(dtfull$QoLPhysF_4, na.rm = T), mean(dtfull$QoLPhysF_5, na.rm = T), mean(dtfull$N_pf, na.rm = T),
            mean(dtise3$QoLPhysF_1, na.rm = T), mean(dtise3$QoLPhysF_2, na.rm = T), mean(dtise3$QoLPhysF_3, na.rm = T), mean(dtise3$QoLPhysF_4, na.rm = T), mean(dtise3$QoLPhysF_5, na.rm = T), mean(dtise3$N_pf, na.rm = T))

Mean_rf = c(mean(dten$QoLRoleF_1, na.rm = T), mean(dten$QoLRoleF_2, na.rm = T), mean(dten$QoLRoleF_3, na.rm = T), mean(dten$QoLRoleF_4, na.rm = T), mean(dten$QoLRoleF_5, na.rm = T), mean(dten$N_rf, na.rm = T),
            mean(dtfull$QoLRoleF_1, na.rm = T), mean(dtfull$QoLRoleF_2, na.rm = T), mean(dtfull$QoLRoleF_3, na.rm = T), mean(dtfull$QoLRoleF_4, na.rm = T), mean(dtfull$QoLRoleF_5, na.rm = T), mean(dtfull$N_rf, na.rm = T),
            mean(dtise3$QoLRoleF_1, na.rm = T), mean(dtise3$QoLRoleF_2, na.rm = T), mean(dtise3$QoLRoleF_3, na.rm = T), mean(dtise3$QoLRoleF_4, na.rm = T), mean(dtise3$QoLRoleF_5, na.rm = T), mean(dtise3$N_rf, na.rm = T))

Mean_cf = c(mean(dten$QoLCogF_1, na.rm = T), mean(dten$QoLCogF_2, na.rm = T), mean(dten$QoLCogF_3, na.rm = T), mean(dten$QoLCogF_4, na.rm = T), mean(dten$QoLCogF_5, na.rm = T), mean(dten$N_cf, na.rm = T),
            mean(dtfull$QoLCogF_1, na.rm = T), mean(dtfull$QoLCogF_2, na.rm = T), mean(dtfull$QoLCogF_3, na.rm = T), mean(dtfull$QoLCogF_4, na.rm = T), mean(dtfull$QoLCogF_5, na.rm = T), mean(dtfull$N_cf, na.rm = T),
            mean(dtise3$QoLCogF_1, na.rm = T), mean(dtise3$QoLCogF_2, na.rm = T), mean(dtise3$QoLCogF_3, na.rm = T), mean(dtise3$QoLCogF_4, na.rm = T), mean(dtise3$QoLCogF_5, na.rm = T), mean(dtise3$N_cf, na.rm = T))

Mean_ef = c(mean(dten$QoLEmoF_1, na.rm = T), mean(dten$QoLEmoF_2, na.rm = T), mean(dten$QoLEmoF_3, na.rm = T), mean(dten$QoLEmoF_4, na.rm = T), mean(dten$QoLEmoF_5, na.rm = T), mean(dten$N_ef, na.rm = T),
            mean(dtfull$QoLEmoF_1, na.rm = T), mean(dtfull$QoLEmoF_2, na.rm = T), mean(dtfull$QoLEmoF_3, na.rm = T), mean(dtfull$QoLEmoF_4, na.rm = T), mean(dtfull$QoLEmoF_5, na.rm = T), mean(dtfull$N_ef, na.rm = T),
            mean(dtise3$QoLEmoF_1, na.rm = T), mean(dtise3$QoLEmoF_2, na.rm = T), mean(dtise3$QoLEmoF_3, na.rm = T), mean(dtise3$QoLEmoF_4, na.rm = T), mean(dtise3$QoLEmoF_5, na.rm = T), mean(dtise3$N_ef, na.rm = T))

Mean_sf = c(mean(dten$QoLSocF_1, na.rm = T), mean(dten$QoLSocF_2, na.rm = T), mean(dten$QoLSocF_3, na.rm = T), mean(dten$QoLSocF_4, na.rm = T), mean(dten$QoLSocF_5, na.rm = T), mean(dten$N_sf, na.rm = T),
            mean(dtfull$QoLSocF_1, na.rm = T), mean(dtfull$QoLSocF_2, na.rm = T), mean(dtfull$QoLSocF_3, na.rm = T), mean(dtfull$QoLSocF_4, na.rm = T), mean(dtfull$QoLSocF_5, na.rm = T), mean(dtfull$N_sf, na.rm = T),
            mean(dtise3$QoLSocF_1, na.rm = T), mean(dtise3$QoLSocF_2, na.rm = T), mean(dtise3$QoLSocF_3, na.rm = T), mean(dtise3$QoLSocF_4, na.rm = T), mean(dtise3$QoLSocF_5, na.rm = T), mean(dtise3$N_sf, na.rm = T))

Mean_qol = c(mean(dten$QoLGHS_1, na.rm = T), mean(dten$QoLGHS_2, na.rm = T), mean(dten$QoLGHS_3, na.rm = T), mean(dten$QoLGHS_4, na.rm = T), mean(dten$QoLGHS_5, na.rm = T), mean(dten$N_qol, na.rm = T),
             mean(dtfull$QoLGHS_1, na.rm = T), mean(dtfull$QoLGHS_2, na.rm = T), mean(dtfull$QoLGHS_3, na.rm = T), mean(dtfull$QoLGHS_4, na.rm = T), mean(dtfull$QoLGHS_5, na.rm = T), mean(dtfull$N_qol, na.rm = T),
             mean(dtise3$QoLGHS_1, na.rm = T), mean(dtise3$QoLGHS_2, na.rm = T), mean(dtise3$QoLGHS_3, na.rm = T), mean(dtise3$QoLGHS_4, na.rm = T), mean(dtise3$QoLGHS_5, na.rm = T), mean(dtise3$N_qol, na.rm = T)) 

Mean_dys = c(mean(dten$QoLDysp_1, na.rm = T), mean(dten$QoLDysp_2, na.rm = T), mean(dten$QoLDysp_3, na.rm = T), mean(dten$QoLDysp_4, na.rm = T), mean(dten$QoLDysp_5, na.rm = T), mean(dten$N_dys, na.rm = T),
             mean(dtfull$QoLDysp_1, na.rm = T), mean(dtfull$QoLDysp_2, na.rm = T), mean(dtfull$QoLDysp_3, na.rm = T), mean(dtfull$QoLDysp_4, na.rm = T), mean(dtfull$QoLDysp_5, na.rm = T), mean(dtfull$N_dys, na.rm = T),
             mean(dtise3$QoLDysp_1, na.rm = T), mean(dtise3$QoLDysp_2, na.rm = T), mean(dtise3$QoLDysp_3, na.rm = T), mean(dtise3$QoLDysp_4, na.rm = T), mean(dtise3$QoLDysp_5, na.rm = T), mean(dtise3$N_dys, na.rm = T))

Mean_pain = c(mean(dten$QoLPain_1, na.rm = T), mean(dten$QoLPain_2, na.rm = T), mean(dten$QoLPain_3, na.rm = T), mean(dten$QoLPain_4, na.rm = T), mean(dten$QoLPain_5, na.rm = T), mean(dten$N_pain, na.rm = T),
              mean(dtfull$QoLPain_1, na.rm = T), mean(dtfull$QoLPain_2, na.rm = T), mean(dtfull$QoLPain_3, na.rm = T), mean(dtfull$QoLPain_4, na.rm = T), mean(dtfull$QoLPain_5, na.rm = T), mean(dtfull$N_pain, na.rm = T),
              mean(dtise3$QoLPain_1, na.rm = T), mean(dtise3$QoLPain_2, na.rm = T), mean(dtise3$QoLPain_3, na.rm = T), mean(dtise3$QoLPain_4, na.rm = T), mean(dtise3$QoLPain_5, na.rm = T), mean(dtise3$N_pain, na.rm = T))

Mean_fati = c(mean(dten$QoLFat_1, na.rm = T), mean(dten$QoLFat_2, na.rm = T), mean(dten$QoLFat_3, na.rm = T), mean(dten$QoLFat_4, na.rm = T), mean(dten$QoLFat_5, na.rm = T), mean(dten$N_fatigue, na.rm = T),
              mean(dtfull$QoLFat_1, na.rm = T), mean(dtfull$QoLFat_2, na.rm = T), mean(dtfull$QoLFat_3, na.rm = T), mean(dtfull$QoLFat_4, na.rm = T), mean(dtfull$QoLFat_5, na.rm = T), mean(dtfull$N_fatigue, na.rm = T),
              mean(dtise3$QoLFat_1, na.rm = T), mean(dtise3$QoLFat_2, na.rm = T), mean(dtise3$QoLFat_3, na.rm = T), mean(dtise3$QoLFat_4, na.rm = T), mean(dtise3$QoLFat_5, na.rm = T), mean(dtise3$N_fatigue, na.rm = T))

Mean_ins = c(mean(dten$QoLInso_1, na.rm = T), mean(dten$QoLInso_2, na.rm = T), mean(dten$QoLInso_3, na.rm = T), mean(dten$QoLInso_4, na.rm = T), mean(dten$QoLInso_5, na.rm = T), mean(dten$N_ins, na.rm = T),
             mean(dtfull$QoLInso_1, na.rm = T), mean(dtfull$QoLInso_2, na.rm = T), mean(dtfull$QoLInso_3, na.rm = T), mean(dtfull$QoLInso_4, na.rm = T), mean(dtfull$QoLInso_5, na.rm = T), mean(dtfull$N_ins, na.rm = T),
             mean(dtise3$QoLInso_1, na.rm = T), mean(dtise3$QoLInso_2, na.rm = T), mean(dtise3$QoLInso_3, na.rm = T), mean(dtise3$QoLInso_4, na.rm = T), mean(dtise3$QoLInso_5, na.rm = T), mean(dtise3$N_ins, na.rm = T))

Mean_al = c(mean(dten$QoLAppL_1, na.rm = T), mean(dten$QoLAppL_2, na.rm = T), mean(dten$QoLAppL_3, na.rm = T), mean(dten$QoLAppL_4, na.rm = T), mean(dten$QoLAppL_5, na.rm = T), mean(dten$N_al, na.rm = T),
            mean(dtfull$QoLAppL_1, na.rm = T), mean(dtfull$QoLAppL_2, na.rm = T), mean(dtfull$QoLAppL_3, na.rm = T), mean(dtfull$QoLAppL_4, na.rm = T), mean(dtfull$QoLAppL_5, na.rm = T), mean(dtfull$N_al, na.rm = T),
            mean(dtise3$QoLAppL_1, na.rm = T), mean(dtise3$QoLAppL_2, na.rm = T), mean(dtise3$QoLAppL_3, na.rm = T), mean(dtise3$QoLAppL_4, na.rm = T), mean(dtise3$QoLAppL_5, na.rm = T), mean(dtise3$N_al, na.rm = T))

Mean_nv = c(mean(dten$QoLNV_1, na.rm = T), mean(dten$QoLNV_2, na.rm = T), mean(dten$QoLNV_3, na.rm = T), mean(dten$QoLNV_4, na.rm = T), mean(dten$QoLNV_5, na.rm = T), mean(dten$N_nau_vomit, na.rm = T),
            mean(dtfull$QoLNV_1, na.rm = T), mean(dtfull$QoLNV_2, na.rm = T), mean(dtfull$QoLNV_3, na.rm = T), mean(dtfull$QoLNV_4, na.rm = T), mean(dtfull$QoLNV_5, na.rm = T), mean(dtfull$N_nau_vomit, na.rm = T),
            mean(dtise3$QoLNV_1, na.rm = T), mean(dtise3$QoLNV_2, na.rm = T), mean(dtise3$QoLNV_3, na.rm = T), mean(dtise3$QoLNV_4, na.rm = T), mean(dtise3$QoLNV_5, na.rm = T), mean(dtise3$N_nau_vomit, na.rm = T))

Mean_con = c(mean(dten$QoLCons_1, na.rm = T), mean(dten$QoLCons_2, na.rm = T), mean(dten$QoLCons_3, na.rm = T), mean(dten$QoLCons_4, na.rm = T), mean(dten$QoLCons_5, na.rm = T), mean(dten$N_cons, na.rm = T),
             mean(dtfull$QoLCons_1, na.rm = T), mean(dtfull$QoLCons_2, na.rm = T), mean(dtfull$QoLCons_3, na.rm = T), mean(dtfull$QoLCons_4, na.rm = T), mean(dtfull$QoLCons_5, na.rm = T), mean(dtfull$N_cons, na.rm = T),
             mean(dtise3$QoLCons_1, na.rm = T), mean(dtise3$QoLCons_2, na.rm = T), mean(dtise3$QoLCons_3, na.rm = T), mean(dtise3$QoLCons_4, na.rm = T), mean(dtise3$QoLCons_5, na.rm = T), mean(dtise3$N_cons, na.rm = T))

Mean_dia = c(mean(dten$QoLDiar_1, na.rm = T), mean(dten$QoLDiar_2, na.rm = T), mean(dten$QoLDiar_3, na.rm = T), mean(dten$QoLDiar_4, na.rm = T), mean(dten$QoLDiar_5, na.rm = T), mean(dten$N_dia, na.rm = T),
             mean(dtfull$QoLDiar_1, na.rm = T), mean(dtfull$QoLDiar_2, na.rm = T), mean(dtfull$QoLDiar_3, na.rm = T), mean(dtfull$QoLDiar_4, na.rm = T), mean(dtfull$QoLDiar_5, na.rm = T), mean(dtfull$N_dia, na.rm = T),
             mean(dtise3$QoLDiar_1, na.rm = T), mean(dtise3$QoLDiar_2, na.rm = T), mean(dtise3$QoLDiar_3, na.rm = T), mean(dtise3$QoLDiar_4, na.rm = T), mean(dtise3$QoLDiar_5, na.rm = T), mean(dtise3$N_dia, na.rm = T))

Mean_fd = c(mean(dten$QoLFin_1, na.rm = T), mean(dten$QoLFin_2, na.rm = T), mean(dten$QoLFin_3, na.rm = T), mean(dten$QoLFin_4, na.rm = T), mean(dten$QoLFin_5, na.rm = T), mean(dten$N_fd, na.rm = T),
            mean(dtfull$QoLFin_1, na.rm = T), mean(dtfull$QoLFin_2, na.rm = T), mean(dtfull$QoLFin_3, na.rm = T), mean(dtfull$QoLFin_4, na.rm = T), mean(dtfull$QoLFin_5, na.rm = T), mean(dtfull$N_fd, na.rm = T),
            mean(dtise3$QoLFin_1, na.rm = T), mean(dtise3$QoLFin_2, na.rm = T), mean(dtise3$QoLFin_3, na.rm = T), mean(dtise3$QoLFin_4, na.rm = T), mean(dtise3$QoLFin_5, na.rm = T), mean(dtise3$N_fd, na.rm = T))


dt3in1 <- data.frame(Groups, Time, Mean_pf, Mean_rf, Mean_cf, Mean_ef, Mean_sf, Mean_qol, Mean_dys, Mean_pain, Mean_fati, Mean_ins, Mean_al, Mean_nv, Mean_con, Mean_dia, Mean_fd)
head(dt3in1)

### Plotting

## Functioning scale & GHS/QoL scale
# physical functioning
library(ggplot2)
library(cowplot)
pf <- ggplot(dt3in1, aes(x=Time, y=Mean_pf, shape=Groups, color=Groups))+
  geom_point()+
  geom_line()+
  scale_x_continuous(limits = c(0,11),
                     expand = c(0,0),
                     breaks = seq(0,11,1))+
  scale_color_manual(values=c("gold", "aquamarine4", "plum"))+
  ylim(50,100)+
  labs(x="Timepoints", y="Mean score", title = "Physical functioning")+
  theme(legend.position = c(.98, .30),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6))

pf

# role functioning
rf <- ggplot(dt3in1, aes(x=Time, y=Mean_rf, shape=Groups, color=Groups))+
  geom_point()+
  geom_line()+
  scale_x_continuous(limits = c(0,11),
                     expand = c(0,0),
                     breaks = seq(0,11,1))+
  scale_color_manual(values=c("gold", "aquamarine4", "plum"))+
  ylim(50,100)+
  labs(x="Timepoints", y="Mean score", title = "Role functioning")+
  theme(legend.position = c(.98, .98),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6))

rf

# cognitive functioning
cf <- ggplot(dt3in1, aes(x=Time, y=Mean_cf, shape=Groups, color=Groups))+
  geom_point()+
  geom_line()+
  scale_x_continuous(limits = c(0,11),
                     expand = c(0,0),
                     breaks = seq(0,11,1))+
  scale_color_manual(values=c("gold", "aquamarine4", "plum"))+
  ylim(50,100)+
  labs(x="Timepoints", y="Mean score", title = "Cognitive functioning")+
  theme(legend.position = c(.98, .98),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6))

cf

# emotional functioning
ef <- ggplot(dt3in1, aes(x=Time, y=Mean_ef, shape=Groups, color=Groups))+
  geom_point()+
  geom_line()+
  scale_x_continuous(limits = c(0,11),
                     expand = c(0,0),
                     breaks = seq(0,11,1))+
  scale_color_manual(values=c("gold", "aquamarine4", "plum"))+
  ylim(50,100)+
  labs(x="Timepoints", y="Mean score", title = "Emotional functioning")+
  theme(legend.position = c(.98, .98),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6))

ef

# social functioning
sf <- ggplot(dt3in1, aes(x=Time, y=Mean_sf, shape=Groups, color=Groups))+
  geom_point()+
  geom_line()+
  scale_x_continuous(limits = c(0,11),
                     expand = c(0,0),
                     breaks = seq(0,11,1))+
  scale_color_manual(values=c("gold", "aquamarine4", "plum"))+
  ylim(50,100)+
  labs(x="Timepoints", y="Mean score", title = "Social functioning")+
  theme(legend.position = c(.98, .98),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6))

sf

# GHS
qol <- ggplot(dt3in1, aes(x=Time, y=Mean_qol, shape=Groups, color=Groups))+
  geom_point()+
  geom_line()+
  scale_x_continuous(limits = c(0,11),
                     expand = c(0,0),
                     breaks = seq(0,11,1))+
  scale_color_manual(values=c("gold", "aquamarine4", "plum"))+
  ylim(50,100)+
  labs(x="Timepoints", y="Mean score", title = "GHS/QoL")+
  theme(legend.position = c(.98, .98),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6))

qol

## Symptom scale
# dyspnea

dys <- ggplot(dt3in1, aes(x=Time, y=Mean_dys, shape=Groups, color=Groups))+
  geom_point()+
  geom_line()+
  scale_x_continuous(limits = c(0,11),
                     expand = c(0,0),
                     breaks = seq(0,11,1))+
  scale_color_manual(values=c("gold", "aquamarine4", "plum"))+
  ylim(0,50)+
  labs(x="Timepoints", y="Mean score", title = "Dyspnea")+
  theme(legend.position = c(.98, .98),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6))

dys

# pain
pain <- ggplot(dt3in1, aes(x=Time, y=Mean_pain, shape=Groups, color=Groups))+
  geom_point()+
  geom_line()+
  scale_x_continuous(limits = c(0,11),
                     expand = c(0,0),
                     breaks = seq(0,11,1))+
  scale_color_manual(values=c("gold", "aquamarine4", "plum"))+
  ylim(0,50)+
  labs(x="Timepoints", y="Mean score", title = "Pain")+
  theme(legend.position = c(.98, .98),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6))

pain

# fatigue
fatigue <- ggplot(dt3in1, aes(x=Time, y=Mean_fati, shape=Groups, color=Groups))+
  geom_point()+
  geom_line()+
  scale_x_continuous(limits = c(0,11),
                     expand = c(0,0),
                     breaks = seq(0,11,1))+
  scale_color_manual(values=c("gold", "aquamarine4", "plum"))+
  ylim(0,50)+
  labs(x="Timepoints", y="Mean score", title = "Fatigue")+
  theme(legend.position = c(.98, .98),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6))

fatigue

# insomnia
inso <- ggplot(dt3in1, aes(x=Time, y=Mean_ins, shape=Groups, color=Groups))+
  geom_point()+
  geom_line()+
  scale_x_continuous(limits = c(0,11),
                     expand = c(0,0),
                     breaks = seq(0,11,1))+
  scale_color_manual(values=c("gold", "aquamarine4", "plum"))+
  ylim(0,50)+
  labs(x="Timepoints", y="Mean score", title = "Insomnia")+
  theme(legend.position = c(.98, .30),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6))

inso

# appetitie loss
al <- ggplot(dt3in1, aes(x=Time, y=Mean_al, shape=Groups, color=Groups))+
  geom_point()+
  geom_line()+
  scale_x_continuous(limits = c(0,11),
                     expand = c(0,0),
                     breaks = seq(0,11,1))+
  scale_color_manual(values=c("gold", "aquamarine4", "plum"))+
  ylim(0,50)+
  labs(x="Timepoints", y="Mean score", title = "Appetite loss")+
  theme(legend.position = c(.98, .98),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6))

al

# nausea/vomiting
nv <- ggplot(dt3in1, aes(x=Time, y=Mean_nv, shape=Groups, color=Groups))+
  geom_point()+
  geom_line()+
  scale_x_continuous(limits = c(0,11),
                     expand = c(0,0),
                     breaks = seq(0,11,1))+
  scale_color_manual(values=c("gold", "aquamarine4", "plum"))+
  ylim(0,50)+
  labs(x="Timepoints", y="Mean score", title = "Nausea/vomiting")+
  theme(legend.position = c(.98, .98),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6))

nv

# constipation
cons <- ggplot(dt3in1, aes(x=Time, y=Mean_con, shape=Groups, color=Groups))+
  geom_point()+
  geom_line()+
  scale_x_continuous(limits = c(0,11),
                     expand = c(0,0),
                     breaks = seq(0,11,1))+
  scale_color_manual(values=c("gold", "aquamarine4", "plum"))+
  ylim(0,50)+
  labs(x="Timepoints", y="Mean score", title = "Constipation")+
  theme(legend.position = c(.98, .98),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6))

cons

# diarrhea
diar <- ggplot(dt3in1, aes(x=Time, y=Mean_dia, shape=Groups, color=Groups))+
  geom_point()+
  geom_line()+
  scale_x_continuous(limits = c(0,11),
                     expand = c(0,0),
                     breaks = seq(0,11,1))+
  scale_color_manual(values=c("gold", "aquamarine4", "plum"))+
  ylim(0,50)+
  labs(x="Timepoints", y="Mean score", title = "Diarrhea")+
  theme(legend.position = c(.98, .98),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6))

diar

# fin.diff.
fd <- ggplot(dt3in1, aes(x=Time, y=Mean_fd, shape=Groups, color=Groups))+
  geom_point()+
  geom_line()+
  scale_x_continuous(limits = c(0,11),
                     expand = c(0,0),
                     breaks = seq(0,11,1))+
  scale_color_manual(values=c("gold", "aquamarine4", "plum"))+
  ylim(0,50)+
  labs(x="Timepoints", y="Mean score", title = "Financial difficulty")+
  theme(legend.position = c(.98, .98),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6))

fd





