load("U:/Studien/Empfind/Projekte/Quality_of_life_2020/yifeng/R data/ISE1&3_merged.Rdata")

dtall <- ISE1_ISE3
attach(dtall)

### prepare a new dataset

time =  c("1", "2", "3", "4", "5", "10")
time <- as.numeric(time)

Cohort = c("All ISE-3 patients (N=292)", "All ISE-3 patients (N=292)", "All ISE-3 patients (N=292)", "All ISE-3 patients (N=292)", "All ISE-3 patients (N=292)", "All ISE-3 patients (N=292)")

mean_pf = c(mean(dtall$QoLPhysF_1, na.rm = T), 
            mean(dtall$QoLPhysF_2, na.rm = T), 
            mean(dtall$QoLPhysF_3, na.rm = T), 
            mean(dtall$QoLPhysF_4, na.rm = T), 
            mean(dtall$QoLPhysF_5, na.rm = T), 
            mean(dtall$N_pf, na.rm = T))

mean_rf = c(mean(dtall$QoLRoleF_1, na.rm = T), 
            mean(dtall$QoLRoleF_2, na.rm = T), 
            mean(dtall$QoLRoleF_3, na.rm = T), 
            mean(dtall$QoLRoleF_4, na.rm = T), 
            mean(dtall$QoLRoleF_5, na.rm = T), 
            mean(dtall$N_rf, na.rm = T))

mean_cf = c(mean(dtall$QoLCogF_1, na.rm = T), 
            mean(dtall$QoLCogF_2, na.rm = T), 
            mean(dtall$QoLCogF_3, na.rm = T), 
            mean(dtall$QoLCogF_4, na.rm = T), 
            mean(dtall$QoLCogF_5, na.rm = T), 
            mean(dtall$N_cf, na.rm = T))

mean_ef = c(mean(dtall$QoLEmoF_1, na.rm = T), 
            mean(dtall$QoLEmoF_2, na.rm = T), 
            mean(dtall$QoLEmoF_3, na.rm = T), 
            mean(dtall$QoLEmoF_4, na.rm = T), 
            mean(dtall$QoLEmoF_5, na.rm = T), 
            mean(dtall$N_ef, na.rm = T))

mean_sf = c(mean(dtall$QoLSocF_1, na.rm = T), 
            mean(dtall$QoLSocF_2, na.rm = T), 
            mean(dtall$QoLSocF_3, na.rm = T), 
            mean(dtall$QoLSocF_4, na.rm = T), 
            mean(dtall$QoLSocF_5, na.rm = T), 
            mean(dtall$N_sf, na.rm = T))

mean_qol = c(mean(dtall$QoLGHS_1, na.rm = T), 
             mean(dtall$QoLGHS_2, na.rm = T), 
             mean(dtall$QoLGHS_3, na.rm = T), 
             mean(dtall$QoLGHS_4, na.rm = T), 
             mean(dtall$QoLGHS_5, na.rm = T), 
             mean(dtall$N_qol, na.rm = T)) 

mean_dys = c(mean(dtall$QoLDysp_1, na.rm = T), 
             mean(dtall$QoLDysp_2, na.rm = T), 
             mean(dtall$QoLDysp_3, na.rm = T), 
             mean(dtall$QoLDysp_4, na.rm = T), 
             mean(dtall$QoLDysp_5, na.rm = T), 
             mean(dtall$N_dys, na.rm = T)) 

mean_pain = c(mean(dtall$QoLPain_1, na.rm = T), 
              mean(dtall$QoLPain_2, na.rm = T), 
              mean(dtall$QoLPain_3, na.rm = T), 
              mean(dtall$QoLPain_4, na.rm = T), 
              mean(dtall$QoLPain_5, na.rm = T), 
              mean(dtall$N_pain, na.rm = T))

mean_fati = c(mean(dtall$QoLFat_1, na.rm = T), 
              mean(dtall$QoLFat_2, na.rm = T), 
              mean(dtall$QoLFat_3, na.rm = T), 
              mean(dtall$QoLFat_4, na.rm = T), 
              mean(dtall$QoLFat_5, na.rm = T), 
              mean(dtall$N_fatigue, na.rm = T)) 

mean_ins = c(mean(dtall$QoLInso_1, na.rm = T), 
             mean(dtall$QoLInso_2, na.rm = T), 
             mean(dtall$QoLInso_3, na.rm = T), 
             mean(dtall$QoLInso_4, na.rm = T), 
             mean(dtall$QoLInso_5, na.rm = T), 
             mean(dtall$N_ins, na.rm = T))

mean_al = c(mean(dtall$QoLAppL_1, na.rm = T), 
            mean(dtall$QoLAppL_2, na.rm = T), 
            mean(dtall$QoLAppL_3, na.rm = T), 
            mean(dtall$QoLAppL_4, na.rm = T), 
            mean(dtall$QoLAppL_5, na.rm = T), 
            mean(dtall$N_al, na.rm = T)) 


mean_nv = c(mean(dtall$QoLNV_1, na.rm = T), 
            mean(dtall$QoLNV_2, na.rm = T), 
            mean(dtall$QoLNV_3, na.rm = T), 
            mean(dtall$QoLNV_4, na.rm = T), 
            mean(dtall$QoLNV_5, na.rm = T), 
            mean(dtall$N_nau_vomit, na.rm = T))


mean_con = c(mean(dtall$QoLCons_1, na.rm = T), 
             mean(dtall$QoLCons_2, na.rm = T), 
             mean(dtall$QoLCons_3, na.rm = T), 
             mean(dtall$QoLCons_4, na.rm = T), 
             mean(dtall$QoLCons_5, na.rm = T), 
             mean(dtall$N_cons, na.rm = T))

mean_dia = c(mean(dtall$QoLDiar_1, na.rm = T), 
             mean(dtall$QoLDiar_2, na.rm = T), 
             mean(dtall$QoLDiar_3, na.rm = T), 
             mean(dtall$QoLDiar_4, na.rm = T), 
             mean(dtall$QoLDiar_5, na.rm = T), 
             mean(dtall$N_dia, na.rm = T))

mean_fd = c(mean(dtall$QoLFin_1, na.rm = T), 
            mean(dtall$QoLFin_2, na.rm = T), 
            mean(dtall$QoLFin_3, na.rm = T), 
            mean(dtall$QoLFin_4, na.rm = T), 
            mean(dtall$QoLFin_5, na.rm = T), 
            mean(dtall$N_fd, na.rm = T))

## new dataframe
dt_allise3 <- data.frame(Cohort, time, mean_pf, mean_rf, mean_cf, mean_ef, mean_sf, mean_qol, 
                         mean_dys, mean_pain, mean_fati, mean_ins, mean_al, mean_nv, mean_con, mean_dia, mean_fd)


### Plotting
library(ggplot2)
dev.off()
## functioning scale & GHS/QoL scale
# Physical functiong
Pf <- ggplot(dt_allise3, aes(x=time, y=mean_pf, shape=Cohort))+
  geom_point()+
  geom_line()+
  scale_x_continuous(limits = c(0,11),
                     expand = c(0,0),
                     breaks = seq(0,11,1))+
  ylim(50, 100)+
  labs(x="Timepoints", y="Mean score", title = "Physical functioning")+
  theme(legend.position = c(.98, .98),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6))

Pf

# role functioning
Rf <- ggplot(dt_allise3, aes(x=time, y=mean_rf, shape=Cohort))+
  geom_point()+
  geom_line()+
  scale_x_continuous(limits = c(0,11),
                     expand = c(0,0),
                     breaks = seq(0,11,1))+
  ylim(50, 100)+
  labs(x="Timepoints", y="Mean score", title = "Role functioning")+
  theme(legend.position = c(.98, .98),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6))

Rf


# cognitive functioning
Cf <- ggplot(dt_allise3, aes(x=time, y=mean_cf, shape=Cohort))+
  geom_point()+
  geom_line()+
  scale_x_continuous(limits = c(0,11),
                     expand = c(0,0),
                     breaks = seq(0,11,1))+
  ylim(50, 100)+
  labs(x="Timepoints", y="Mean score", title = "Cognitive functioning")+
  theme(legend.position = c(.98, .98),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6))

Cf

# emotional functioning
Ef <- ggplot(dt_allise3, aes(x=time, y=mean_ef, shape=Cohort))+
  geom_point()+
  geom_line()+
  scale_x_continuous(limits = c(0,11),
                     expand = c(0,0),
                     breaks = seq(0,11,1))+
  ylim(50, 100)+
  labs(x="Timepoints", y="Mean score", title = "Emotional functioning")+
  theme(legend.position = c(.98, .98),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6))

Ef

# social functiong
Sf <- ggplot(dt_allise3, aes(x=time, y=mean_sf, shape=Cohort))+
  geom_point()+
  geom_line()+
  scale_x_continuous(limits = c(0,11),
                     expand = c(0,0),
                     breaks = seq(0,11,1))+
  ylim(50, 100)+
  labs(x="Timepoints", y="Mean score", title = "Social functioning")+
  theme(legend.position = c(.98, .98),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6))

Sf

# GHS/QoL
ghs <- ggplot(dt_allise3, aes(x=time, y=mean_qol, shape=Cohort))+
  geom_point()+
  geom_line()+
  scale_x_continuous(limits = c(0,11),
                     expand = c(0,0),
                     breaks = seq(0,11,1))+
  ylim(50, 100)+
  labs(x="Timepoints", y="Mean score", title = "GHS/QoL")+
  theme(legend.position = c(.98, .98),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6))

ghs

## Symptom scale
# dyspnea

Dys <- ggplot(dt_allise3, aes(x=time, y=mean_dys, shape=Cohort))+
  geom_point()+
  geom_line()+
  scale_x_continuous(limits = c(0,11),
                     expand = c(0,0),
                     breaks = seq(0,11,1))+
  ylim(0,50)+
  labs(x="Timepoints", y="Mean score", title = "Dyspnea")+
  theme(legend.position = c(.98, .98),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6))

Dys

# pain
Pain <- ggplot(dt_allise3, aes(x=time, y=mean_pain, shape=Cohort))+
  geom_point()+
  geom_line()+
  scale_x_continuous(limits = c(0,11),
                     expand = c(0,0),
                     breaks = seq(0,11,1))+
  ylim(0,50)+
  labs(x="Timepoints", y="Mean score", title = "Pain")+
  theme(legend.position = c(.98, .98),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6))

Pain

# fatigue
Fatigue <- ggplot(dt_allise3, aes(x=time, y=mean_fati, shape=Cohort))+
  geom_point()+
  geom_line()+
  scale_x_continuous(limits = c(0,11),
                     expand = c(0,0),
                     breaks = seq(0,11,1))+
  ylim(0,50)+
  labs(x="Timepoints", y="Mean score", title = "Fatigue")+
  theme(legend.position = c(.98, .98),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6))

Fatigue

# insomnia
Inso <- ggplot(dt_allise3, aes(x=time, y=mean_ins, shape=Cohort))+
  geom_point()+
  geom_line()+
  scale_x_continuous(limits = c(0,11),
                     expand = c(0,0),
                     breaks = seq(0,11,1))+
  ylim(0,50)+
  labs(x="Timepoints", y="Mean score", title = "Insomnia")+
  theme(legend.position = c(.98, .15),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6))

Inso

# appetitie loss
Al <- ggplot(dt_allise3, aes(x=time, y=mean_al, shape=Cohort))+
  geom_point()+
  geom_line()+
  scale_x_continuous(limits = c(0,11),
                     expand = c(0,0),
                     breaks = seq(0,11,1))+
  ylim(0,50)+
  labs(x="Timepoints", y="Mean score", title = "Appetite loss")+
  theme(legend.position = c(.98, .98),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6))

Al

# nausea/vomiting
NV <- ggplot(dt_allise3, aes(x=time, y=mean_nv, shape=Cohort))+
  geom_point()+
  geom_line()+
  scale_x_continuous(limits = c(0,11),
                     expand = c(0,0),
                     breaks = seq(0,11,1))+
  ylim(0,50)+
  labs(x="Timepoints", y="Mean score", title = "Nausea/vomiting")+
  theme(legend.position = c(.98, .98),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6))

NV

# constipation
Cons <- ggplot(dt_allise3, aes(x=time, y=mean_con, shape=Cohort))+
  geom_point()+
  geom_line()+
  scale_x_continuous(limits = c(0,11),
                     expand = c(0,0),
                     breaks = seq(0,11,1))+
  ylim(0,50)+
  labs(x="Timepoints", y="Mean score", title = "Constipation")+
  theme(legend.position = c(.98, .98),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6))

Cons

# diarrhea
Diar <- ggplot(dt_allise3, aes(x=time, y=mean_dia, shape=Cohort))+
  geom_point()+
  geom_line()+
  scale_x_continuous(limits = c(0,11),
                     expand = c(0,0),
                     breaks = seq(0,11,1))+
  ylim(0,50)+
  labs(x="Timepoints", y="Mean score", title = "Diarrhea")+
  theme(legend.position = c(.98, .98),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6))

Diar

# fin.diff.
Fd <- ggplot(dt_allise3, aes(x=time, y=mean_fd, shape=Cohort))+
  geom_point()+
  geom_line()+
  scale_x_continuous(limits = c(0,11),
                     expand = c(0,0),
                     breaks = seq(0,11,1))+
  ylim(0,50)+
  labs(x="Timepoints", y="Mean score", title = "Financial difficulty")+
  theme(legend.position = c(.98, .98),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6))

Fd


### try to put all lines in 2 graphs
GHS_functionings = c("physical functioning", "role functioning", "cognitive functioning", "emotional functioning", "social functioning", "QoL/GHS", 
                     "physical functioning", "role functioning", "cognitive functioning", "emotional functioning", "social functioning", "QoL/GHS", 
                     "physical functioning", "role functioning", "cognitive functioning", "emotional functioning", "social functioning", "QoL/GHS", 
                     "physical functioning", "role functioning", "cognitive functioning", "emotional functioning", "social functioning", "QoL/GHS", 
                     "physical functioning", "role functioning", "cognitive functioning", "emotional functioning", "social functioning", "QoL/GHS", 
                     "physical functioning", "role functioning", "cognitive functioning", "emotional functioning", "social functioning", "QoL/GHS")

symptoms = c("dyspnea", "pain", "fatigue", "insomnia", "appetite loss", "nausea/vomiting", "constipation", "diarrhea", "financial difficulty", 
             "dyspnea", "pain", "fatigue", "insomnia", "appetite loss", "nausea/vomiting", "constipation", "diarrhea", "financial difficulty",
             "dyspnea", "pain", "fatigue", "insomnia", "appetite loss", "nausea/vomiting", "constipation", "diarrhea", "financial difficulty", 
             "dyspnea", "pain", "fatigue", "insomnia", "appetite loss", "nausea/vomiting", "constipation", "diarrhea", "financial difficulty", 
             "dyspnea", "pain", "fatigue", "insomnia", "appetite loss", "nausea/vomiting", "constipation", "diarrhea", "financial difficulty", 
             "dyspnea", "pain", "fatigue", "insomnia", "appetite loss", "nausea/vomiting", "constipation", "diarrhea", "financial difficulty")

time_fun = c("1", "1", "1","1", "1", "1", "2", "2", "2", "2", "2", "2", "3", "3", "3", "3", "3", "3", "4", "4", "4", "4", "4", "4", "5", "5", "5", "5", "5", "5", "10", "10", "10", "10", "10", "10")

time_sym = c("1", "1", "1", "1", "1", "1", "1", "1", "1", "2", "2", "2", "2", "2", "2", "2", "2", "2", "3", "3", "3", "3", "3", "3", "3", "3", "3", "4", "4", "4", "4", "4", "4", "4", "4", "4", "5", "5", "5", "5", "5", "5", "5", "5", "5", "10", "10", "10", "10", "10", "10", "10", "10", "10")

time_fun <- as.numeric(time_fun)
time_sym <- as.numeric(time_sym)

value_fun = c(mean(dtall$QoLPhysF_1, na.rm = T),
              mean(dtall$QoLRoleF_1, na.rm = T),
              mean(dtall$QoLCogF_1, na.rm = T),
              mean(dtall$QoLEmoF_1, na.rm = T),
              mean(dtall$QoLSocF_1, na.rm = T),
              mean(dtall$QoLGHS_1, na.rm = T),
              mean(dtall$QoLPhysF_2, na.rm = T),
              mean(dtall$QoLRoleF_2, na.rm = T),
              mean(dtall$QoLCogF_2, na.rm = T),
              mean(dtall$QoLEmoF_2, na.rm = T),
              mean(dtall$QoLSocF_2, na.rm = T),
              mean(dtall$QoLGHS_2, na.rm = T),
              mean(dtall$QoLPhysF_3, na.rm = T),
              mean(dtall$QoLRoleF_3, na.rm = T),
              mean(dtall$QoLCogF_3, na.rm = T),
              mean(dtall$QoLEmoF_3, na.rm = T),
              mean(dtall$QoLSocF_3, na.rm = T),
              mean(dtall$QoLGHS_3, na.rm = T),
              mean(dtall$QoLPhysF_4, na.rm = T),
              mean(dtall$QoLRoleF_4, na.rm = T),
              mean(dtall$QoLCogF_4, na.rm = T),
              mean(dtall$QoLEmoF_4, na.rm = T),
              mean(dtall$QoLSocF_4, na.rm = T),
              mean(dtall$QoLGHS_4, na.rm = T),
              mean(dtall$QoLPhysF_5, na.rm = T),
              mean(dtall$QoLRoleF_5, na.rm = T),
              mean(dtall$QoLCogF_5, na.rm = T),
              mean(dtall$QoLEmoF_5, na.rm = T),
              mean(dtall$QoLSocF_5, na.rm = T),
              mean(dtall$QoLGHS_5, na.rm = T),
              mean(dtall$N_pf, na.rm = T),
              mean(dtall$N_rf, na.rm = T),
              mean(dtall$N_cf, na.rm = T),
              mean(dtall$N_ef, na.rm = T),
              mean(dtall$N_sf, na.rm = T),
              mean(dtall$N_qol, na.rm = T))

value_sym = c(mean(dtall$QoLDysp_1, na.rm = T),
              mean(dtall$QoLPain_1, na.rm = T),
              mean(dtall$QoLFat_1, na.rm = T),
              mean(dtall$QoLInso_1, na.rm = T),
              mean(dtall$QoLAppL_1, na.rm = T),
              mean(dtall$QoLNV_1, na.rm = T),
              mean(dtall$QoLCons_1, na.rm = T),
              mean(dtall$QoLDiar_1, na.rm = T),
              mean(dtall$QoLFin_1, na.rm = T),
              mean(dtall$QoLDysp_2, na.rm = T),
              mean(dtall$QoLPain_2, na.rm = T),
              mean(dtall$QoLFat_2, na.rm = T),
              mean(dtall$QoLInso_2, na.rm = T),
              mean(dtall$QoLAppL_2, na.rm = T),
              mean(dtall$QoLNV_2, na.rm = T),
              mean(dtall$QoLCons_2, na.rm = T),
              mean(dtall$QoLDiar_2, na.rm = T),
              mean(dtall$QoLFin_2, na.rm = T),
              mean(dtall$QoLDysp_3, na.rm = T),
              mean(dtall$QoLPain_3, na.rm = T),
              mean(dtall$QoLFat_3, na.rm = T),
              mean(dtall$QoLInso_3, na.rm = T),
              mean(dtall$QoLAppL_3, na.rm = T),
              mean(dtall$QoLNV_3, na.rm = T),
              mean(dtall$QoLCons_3, na.rm = T),
              mean(dtall$QoLDiar_3, na.rm = T),
              mean(dtall$QoLFin_3, na.rm = T),
              mean(dtall$QoLDysp_4, na.rm = T),
              mean(dtall$QoLPain_4, na.rm = T),
              mean(dtall$QoLFat_4, na.rm = T),
              mean(dtall$QoLInso_4, na.rm = T),
              mean(dtall$QoLAppL_4, na.rm = T),
              mean(dtall$QoLNV_4, na.rm = T),
              mean(dtall$QoLCons_4, na.rm = T),
              mean(dtall$QoLDiar_4, na.rm = T),
              mean(dtall$QoLFin_4, na.rm = T),
              mean(dtall$QoLDysp_5, na.rm = T),
              mean(dtall$QoLPain_5, na.rm = T),
              mean(dtall$QoLFat_5, na.rm = T),
              mean(dtall$QoLInso_5, na.rm = T),
              mean(dtall$QoLAppL_5, na.rm = T),
              mean(dtall$QoLNV_5, na.rm = T),
              mean(dtall$QoLCons_5, na.rm = T),
              mean(dtall$QoLDiar_5, na.rm = T),
              mean(dtall$QoLFin_5, na.rm = T),
              mean(dtall$N_dys, na.rm = T),
              mean(dtall$N_pain, na.rm = T),
              mean(dtall$N_fatigue, na.rm = T),
              mean(dtall$N_ins, na.rm = T),
              mean(dtall$N_al, na.rm = T),
              mean(dtall$N_nau_vomit, na.rm = T),
              mean(dtall$N_cons, na.rm = T),
              mean(dtall$N_dia, na.rm = T),
              mean(dtall$N_fd, na.rm = T))

qol_fun <- data.frame(GHS_functionings, time_fun, value_fun)
symptoms_df <- data.frame(symptoms, time_sym, value_sym)

#plotting
library(cowplot)
QoL_func <- ggplot(qol_fun, aes(x=time_fun, y=value_fun, shape=GHS_functionings, color=GHS_functionings))+
  geom_point()+
  geom_line()+
  scale_x_continuous(limits = c(0,11),
                     expand = c(0,0),
                     breaks = seq(0,11,1))+
  scale_color_manual(values=c("plum", "gold", "aquamarine4", "blue", "dark grey", "green"))+
  ylim(50,100)+
  labs(x="Timepoints", y="QLQ-C30 scores", title = "(a)QoL/GHS & Functional scales", colour = "categories", shape = "categories")+
  theme(legend.position = "bottom")

QoL_func




Symp <- ggplot(symptoms_df, aes(x=time_sym, y=value_sym, shape=symptoms, color=symptoms))+
  geom_point()+
  geom_line()+
  scale_x_continuous(limits = c(0,11),
                     expand = c(0,0),
                     breaks = seq(0,11,1))+
  scale_color_manual(values=c("plum", "gold", "aquamarine4", "blue", "dark grey", "green", "black", "red", "purple"))+
  ylim(0,55)+
  labs(x="Timepoints", y="QLQ-C30 scores", title = "(b)Symptom scales", colour = "categories", shape = "categories")+
  theme(legend.position = "bottom")

Symp



