#### Friedman tests and post-hoc tests for all 6 time points (only applys for full responder sample)####
load("U:/Studien/Empfind/Projekte/Quality_of_life_2020/yifeng/R data/fullresponder.Rdata")

dtfull <- fullresponder

###### Friedman Test for full-responders (a non-parametric alternative to one-way repeated measures ANOVA test) ######

library(tidyverse)
library(ggpubr)
library(rstatix)

##### For GHS scale ##### *
## data preparation ##
head(dtfull, 3)
# gather columns qol1 to qol6 into long format

qolfull <- data.frame(id = dtfull$ID_ISE, t1 = dtfull$QoLGHS_1, t2 = dtfull$QoLGHS_2, t3 = dtfull$QoLGHS_3, t4 = dtfull$QoLGHS_4, t5 = dtfull$QoLGHS_5, t6 = dtfull$N_qol)
qolfull <- qolfull[which(complete.cases(qolfull)), ]

dtfull_qol <- qolfull %>%
  gather(key = "time", value = "QoL_score", t1, t2, t3, t4, t5, t6) %>%
  convert_as_factor(id, time)

head(dtfull_qol, 3)


## summary statistics ##
dtfull_qol %>%
  group_by(time) %>%
  get_summary_stats(QoL_score, type="common") 

## visualization
bxp_qol <- ggboxplot(dtfull_qol, x="time", y="QoL_score", add = "jitter")
bxp_qol


## computation
table(dtfull_qol$time, dtfull_qol$id)

res.fried_qol <- dtfull_qol %>% friedman_test(QoL_score ~ time |id)
res.fried_qol # The qol score was statistically significantly different at the different time points, X2(5) = 60.1, p < 0.0001

## effect size
dtfull_qol %>% friedman_effsize(QoL_score ~ time |id)

## Post-hoc tests
# multiple pairwise-comparisons (wilcoxon signed rank test): From the output of the Friedman test, we know that there is a significant difference between groups, but we don't know which pairs of groups are different. P-values are adjusted using the Bonferroni multiple testing correction method.
pwc_qol <- dtfull_qol %>%
  wilcox_test(QoL_score ~ time, paired = TRUE, p.adjust.method = "bonferroni")

pwc_qol  # ......

## Report
pwc_qol <- pwc_qol %>% add_xy_position(x = "time")
ggboxplot(dtfull_qol, x="time", y="QoL_score", add = "point") +
  stat_pvalue_manual(pwc_qol, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.fried_qol, detailed = TRUE),
    caption = get_pwc_label(pwc_qol)
  )

##### For PF scale ##### *
pffull <- data.frame(id = dtfull$ID_ISE, t1 = dtfull$QoLPhysF_1, t2 = dtfull$QoLPhysF_2, t3 = dtfull$QoLPhysF_3, t4 = dtfull$QoLPhysF_4, t5 = dtfull$QoLPhysF_5, t6 = dtfull$N_pf)
pffull <- pffull[which(complete.cases(pffull)), ]

dtfull_pf <- pffull %>%
  gather(key = "time", value = "physical_functioning", t1, t2, t3, t4, t5, t6) %>%
  convert_as_factor(id, time)

head(dtfull_pf, 3)
#
dtfull_pf %>%
  group_by(time) %>%
  get_summary_stats(physical_functioning, type="common") 

#
bxp_pf <- ggboxplot(dtfull_pf, x="time", y="physical_functioning", add = "jitter")
bxp_pf

#
table(dtfull_pf$time, dtfull_pf$id)

res.fried_pf <- dtfull_pf %>% friedman_test(physical_functioning ~ time |id)
res.fried_pf # The PF score was statistically significantly different at the different time points, X2(5) = 43.4, p < 0.0001

#
pwc_pf <- dtfull_pf %>%
  wilcox_test(physical_functioning ~ time, paired = TRUE, p.adjust.method = "bonferroni")

pwc_pf

#
pwc_pf <- pwc_pf %>% add_xy_position(x = "time1")
ggboxplot(dtfull_pf, x="time", y="physical_functioning", add = "point") +
  stat_pvalue_manual(pwc_pf, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.fried_pf, detailed = TRUE),
    caption = get_pwc_label(pwc_pf)
  )

##### For RF scale ##### *
rffull <- data.frame(id = dtfull$ID_ISE, t1 = dtfull$QoLRoleF_1, t2 = dtfull$QoLRoleF_2, t3 = dtfull$QoLRoleF_3, t4 = dtfull$QoLRoleF_4, t5 = dtfull$QoLRoleF_5, t6 = dtfull$N_rf)
rffull <- rffull[which(complete.cases(rffull)), ]

dtfull_rf <- rffull %>%
  gather(key = "time", value = "role_functioning", t1, t2, t3, t4, t5, t6) %>%
  convert_as_factor(id, time)

head(dtfull_rf, 3)
#
dtfull_rf %>%
  group_by(time) %>%
  get_summary_stats(role_functioning, type="common") 

#
bxp_rf <- ggboxplot(dtfull_rf, x="time", y="role_functioning", add = "jitter")
bxp_rf

#
table(dtfull_rf$time, dtfull_rf$id)

res.fried_rf <- dtfull_rf %>% friedman_test(role_functioning ~ time |id)
res.fried_rf # The RF score was statistically significantly different at the different time points, X2(5) = 43.4, p < 0.0001

#
pwc_rf <- dtfull_rf %>%
  wilcox_test(role_functioning ~ time, paired = TRUE, p.adjust.method = "bonferroni")

pwc_rf

#
pwc_rf <- pwc_rf %>% add_xy_position(x = "time1")
ggboxplot(dtfull_rf, x="time", y="role_functioning", add = "point") +
  stat_pvalue_manual(pwc_rf, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.fried_rf, detailed = TRUE),
    caption = get_pwc_label(pwc_rf)
  )

##### For CF scale ##### 
cffull <- data.frame(id = dtfull$ID_ISE, t1 = dtfull$QoLCogF_1, t2 = dtfull$QoLCogF_2, t3 = dtfull$QoLCogF_3, t4 = dtfull$QoLCogF_4, t5 = dtfull$QoLCogF_5, t6 = dtfull$N_cf)
cffull <- cffull[which(complete.cases(cffull)), ]

dtfull_cf <- cffull %>%
  gather(key = "time", value = "cognitive_functioning", t1, t2, t3, t4, t5, t6) %>%
  convert_as_factor(id, time)

head(dtfull_cf, 3)
#
dtfull_cf %>%
  group_by(time) %>%
  get_summary_stats(cognitive_functioning, type="common") 

#
bxp_cf <- ggboxplot(dtfull_cf, x="time", y="cognitive_functioning", add = "jitter")
bxp_cf

#
table(dtfull_cf$time, dtfull_cf$id)

res.fried_cf <- dtfull_cf %>% friedman_test(cognitive_functioning ~ time |id)
res.fried_cf # The CF score was not statistically significantly different at any time points,


##### For EF scale ##### *
effull <- data.frame(id = dtfull$ID_ISE, t1 = dtfull$QoLEmoF_1, t2 = dtfull$QoLEmoF_2, t3 = dtfull$QoLEmoF_3, t4 = dtfull$QoLEmoF_4, t5 = dtfull$QoLEmoF_5, t6 = dtfull$N_ef)
effull <- effull[which(complete.cases(effull)), ]

dtfull_ef <- effull %>%
  gather(key = "time", value = "emotional_functioning", t1, t2, t3, t4, t5, t6) %>%
  convert_as_factor(id, time)

head(dtfull_ef, 3)
#
dtfull_ef %>%
  group_by(time) %>%
  get_summary_stats(emotional_functioning, type="common") 

#
bxp_ef <- ggboxplot(dtfull_ef, x="time", y="emotional_functioning", add = "jitter")
bxp_ef

#
table(dtfull_ef$time, dtfull_ef$id)

res.fried_ef <- dtfull_ef %>% friedman_test(emotional_functioning ~ time |id)
res.fried_ef # The EF score was statistically significantly different at the different time points, X2(5) = 43.4, p < 0.0001

#
pwc_ef <- dtfull_ef %>%
  wilcox_test(emotional_functioning ~ time, paired = TRUE, p.adjust.method = "bonferroni")

pwc_ef

#
pwc_ef <- pwc_ef %>% add_xy_position(x = "time1")
ggboxplot(dtfull_ef, x="time", y="emotional_functioning", add = "point") +
  stat_pvalue_manual(pwc_ef, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.fried_ef, detailed = TRUE),
    caption = get_pwc_label(pwc_ef)
  )

##### For SF scale ##### *
sffull <- data.frame(id = dtfull$ID_ISE, t1 = dtfull$QoLSocF_1, t2 = dtfull$QoLSocF_2, t3 = dtfull$QoLSocF_3, t4 = dtfull$QoLSocF_4, t5 = dtfull$QoLSocF_5, t6 = dtfull$N_sf)
sffull <- sffull[which(complete.cases(sffull)), ]

dtfull_sf <- sffull %>%
  gather(key = "time", value = "social_functioning", t1, t2, t3, t4, t5, t6) %>%
  convert_as_factor(id, time)

head(dtfull_sf, 3)
#
dtfull_sf %>%
  group_by(time) %>%
  get_summary_stats(social_functioning, type="common") 

#
bxp_sf <- ggboxplot(dtfull_sf, x="time", y="social_functioning", add = "jitter")
bxp_sf

#
table(dtfull_sf$time, dtfull_sf$id)

res.fried_sf <- dtfull_sf %>% friedman_test(social_functioning ~ time |id)
res.fried_sf # The SF score was statistically significantly different at the different time points, X2(5) = 43.4, p < 0.0001

#
pwc_sf <- dtfull_sf %>%
  wilcox_test(social_functioning ~ time, paired = TRUE, p.adjust.method = "bonferroni")

pwc_sf

#
pwc_sf <- pwc_sf %>% add_xy_position(x = "time1")
ggboxplot(dtfull_sf, x="time", y="social_functioning", add = "point") +
  stat_pvalue_manual(pwc_sf, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.fried_sf, detailed = TRUE),
    caption = get_pwc_label(pwc_sf)
  )

##### For dys scale ##### *
dysfull <- data.frame(id = dtfull$ID_ISE, t1 = dtfull$QoLDysp_1, t2 = dtfull$QoLDysp_2, t3 = dtfull$QoLDysp_3, t4 = dtfull$QoLDysp_4, t5 = dtfull$QoLDysp_5, t6 = dtfull$N_dys)
dysfull <- dysfull[which(complete.cases(dysfull)), ]

dtfull_dys <- dysfull %>%
  gather(key = "time", value = "dyspnea", t1, t2, t3, t4, t5, t6) %>%
  convert_as_factor(id, time)

head(dtfull_dys, 3)
#
dtfull_dys %>%
  group_by(time) %>%
  get_summary_stats(dyspnea, type="common") 

#
bxp_dys <- ggboxplot(dtfull_dys, x="time", y="dyspnea", add = "jitter")
bxp_dys

#
table(dtfull_dys$time, dtfull_dys$id)

res.fried_dys <- dtfull_dys %>% friedman_test(dyspnea ~ time |id)
res.fried_dys # The dys scale score was statistically significantly different at the different time points, X2(5) = 147, p < 0.0001


#
pwc_dys <- dtfull_dys %>%
  wilcox_test(dyspnea ~ time, paired = TRUE, p.adjust.method = "bonferroni")

pwc_dys

#
pwc_dys <- pwc_dys %>% add_xy_position(x = "time")
ggboxplot(dtfull_dys, x="time", y="dyspnea", add = "point") +
  stat_pvalue_manual(pwc_dys, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.fried_dys, detailed = TRUE),
    caption = get_pwc_label(pwc_dys)
  )

# sd, se, 95% CI for t5 and t6
n <- 175

SEt5_dys <- qnorm(0.975)*sd(dtfull$QoLDysp_5, na.rm = T)/sqrt(n)
SEt6_dys <- qnorm(0.975)*sd(dtfull$N_dys, na.rm = T)/sqrt(n)

SEt5_dys
SEt6_dys

##### For pain scale ##### *
painfull <- data.frame(id = dtfull$ID_ISE, t1 = dtfull$QoLPain_1, t2 = dtfull$QoLPain_2, t3 = dtfull$QoLPain_3, t4 = dtfull$QoLPain_4, t5 = dtfull$QoLPain_5, t6 = dtfull$N_pain)
painfull <- painfull[which(complete.cases(painfull)), ]

dtfull_pain <- painfull %>%
  gather(key = "time", value = "pain", t1, t2, t3, t4, t5, t6) %>%
  convert_as_factor(id, time)

head(dtfull_pain, 3)
#
dtfull_pain %>%
  group_by(time) %>%
  get_summary_stats(pain, type="common") 

#
bxp_pain <- ggboxplot(dtfull_pain, x="time", y="pain", add = "jitter")
bxp_pain

#
table(dtfull_pain$time, dtfull_pain$id)

res.fried_pain <- dtfull_pain %>% friedman_test(pain ~ time |id)
res.fried_pain # The pain scale score was statistically significantly different at the different time points, X2(5) = 85.5, p < 0.0001

#
pwc_pain <- dtfull_pain %>%
  wilcox_test(pain ~ time, paired = TRUE, p.adjust.method = "bonferroni")

pwc_pain

#
pwc_pain <- pwc_pain %>% add_xy_position(x = "time")
ggboxplot(dtfull_pain, x="time", y="pain", add = "point") +
  stat_pvalue_manual(pwc_pain, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.fried_pain, detailed = TRUE),
    caption = get_pwc_label(pwc_pain)
  )


##### For fat scale ##### *
fatfull <- data.frame(id = dtfull$ID_ISE, t1 = dtfull$QoLFat_1, t2 = dtfull$QoLFat_2, t3 = dtfull$QoLFat_3, t4 = dtfull$QoLFat_4, t5 = dtfull$QoLFat_5, t6 = dtfull$N_fatigue)
fatfull <- fatfull[which(complete.cases(fatfull)), ]

dtfull_fat <- fatfull %>%
  gather(key = "time", value = "fatigue", t1, t2, t3, t4, t5, t6) %>%
  convert_as_factor(id, time)

head(dtfull_fat, 3)
#
dtfull_fat %>%
  group_by(time) %>%
  get_summary_stats(fatigue, type="common") 

#
bxp_fat <- ggboxplot(dtfull_fat, x="time", y="fatigue", add = "jitter")
bxp_fat

#
table(dtfull_fat$time, dtfull_fat$id)

res.fried_fat <- dtfull_fat %>% friedman_test(fatigue ~ time |id)
res.fried_fat # The fatigue scale score was statistically significantly different at the different time points, X2(5) = 147, p < 0.0001

#
pwc_fat <- dtfull_fat %>%
  wilcox_test(fatigue ~ time, paired = TRUE, p.adjust.method = "bonferroni")

pwc_fat

#
pwc_fat <- pwc_fat %>% add_xy_position(x = "time")
ggboxplot(dtfull_fat, x="time", y="fatigue", add = "point") +
  stat_pvalue_manual(pwc_fat, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.fried_fat, detailed = TRUE),
    caption = get_pwc_label(pwc_fat)
  )



##### For insomnia scale ##### * the Y is ordinary (only 4 levels) - maybe should be treated as count data? *
insfull <- data.frame(id = dtfull$ID_ISE, t1 = dtfull$QoLInso_1, t2 = dtfull$QoLInso_2, t3 = dtfull$QoLInso_3, t4 = dtfull$QoLInso_4, t5 = dtfull$QoLInso_5, t6 = dtfull$N_ins)
insfull <- insfull[which(complete.cases(insfull)), ]

dtfull_ins <- insfull %>%
  gather(key = "time", value = "ins_score", t1, t2, t3, t4, t5, t6) %>%
  convert_as_factor(id, time)

head(dtfull_ins, 3)
#
dtfull_ins %>%
  group_by(time) %>%
  get_summary_stats(ins_score, type="common") 

#
bxp_ins <- ggboxplot(dtfull_ins, x="time", y="ins_score", add = "jitter")
bxp_ins

#
table(dtfull_ins$time, dtfull_ins$id)

res.fried_ins <- dtfull_ins %>% friedman_test(ins_score ~ time |id)
res.fried_ins # The insomnia score was not statistically significantly different at the different time points, X2(5) = 5.44, p = 0.365


##### For al scale #####
alfull <- data.frame(id = dtfull$ID_ISE, t1 = dtfull$QoLAppL_1, t2 = dtfull$QoLAppL_2, t3 = dtfull$QoLAppL_3, t4 = dtfull$QoLAppL_4, t5 = dtfull$QoLAppL_5, t6 = dtfull$N_al)
alfull <- alfull[which(complete.cases(alfull)), ]

dtfull_al <- alfull %>%
  gather(key = "time", value = "al_score", t1, t2, t3, t4, t5, t6) %>%
  convert_as_factor(id, time)

head(dtfull_al, 3)
#
dtfull_al %>%
  group_by(time) %>%
  get_summary_stats(al_score, type="common") 

#
bxp_al <- ggboxplot(dtfull_al, x="time", y="al_score", add = "jitter")
bxp_al

#
table(dtfull_al$time, dtfull_al$id)

res.fried_al <- dtfull_al %>% friedman_test(al_score ~ time |id)
res.fried_al # The al scale score was not statistically significantly different at the any time points

##### For nv scale ##### *
nvfull <- data.frame(id = dtfull$ID_ISE, t1 = dtfull$QoLNV_1, t2 = dtfull$QoLNV_2, t3 = dtfull$QoLNV_3, t4 = dtfull$QoLNV_4, t5 = dtfull$QoLNV_5, t6 = dtfull$N_nau_vomit)
nvfull <- nvfull[which(complete.cases(nvfull)), ]

dtfull_nv <- nvfull %>%
  gather(key = "time", value = "nausea_vomiting", t1, t2, t3, t4, t5, t6) %>%
  convert_as_factor(id, time)

head(dtfull_nv, 3)
#
dtfull_nv %>%
  group_by(time) %>%
  get_summary_stats(nausea_vomiting, type="common") 

#
bxp_nv <- ggboxplot(dtfull_nv, x="time", y="nausea_vomiting", add = "jitter")
bxp_nv

#
table(dtfull_nv$time, dtfull_nv$id)

res.fried_nv <- dtfull_nv %>% friedman_test(nausea_vomiting ~ time |id)
res.fried_nv # The nv scale score was statistically significantly different at the different time points, X2(5) = 147, p < 0.0001

#
pwc_nv <- dtfull_nv %>%
  wilcox_test(nausea_vomiting ~ time, paired = TRUE, p.adjust.method = "bonferroni")

pwc_nv

#
pwc_nv <- pwc_nv %>% add_xy_position(x = "time")
ggboxplot(dtfull_nv, x="time", y="nausea_vomiting", add = "point") +
  stat_pvalue_manual(pwc_nv, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.fried_nv, detailed = TRUE),
    caption = get_pwc_label(pwc_nv)
  )

##### For cons scale #####
consfull <- data.frame(id = dtfull$ID_ISE, t1 = dtfull$QoLCons_1, t2 = dtfull$QoLCons_2, t3 = dtfull$QoLCons_3, t4 = dtfull$QoLCons_4, t5 = dtfull$QoLCons_5, t6 = dtfull$N_cons)
consfull <- consfull[which(complete.cases(consfull)), ]

dtfull_cons <- consfull %>%
  gather(key = "time", value = "cons_score", t1, t2, t3, t4, t5, t6) %>%
  convert_as_factor(id, time)

head(dtfull_cons, 3)
#
dtfull_cons %>%
  group_by(time) %>%
  get_summary_stats(cons_score, type="common") 

#
bxp_cons <- ggboxplot(dtfull_cons, x="time", y="cons_score", add = "jitter")
bxp_cons

#
table(dtfull_cons$time, dtfull_cons$id)

res.fried_cons <- dtfull_cons %>% friedman_test(cons_score ~ time |id)
res.fried_cons # The cons scale score was not statistically significantly different at the different time points, X2(5) = 147, p < 0.0001

##### For diar scale ##### *
diarfull <- data.frame(id = dtfull$ID_ISE, t1 = dtfull$QoLDiar_1, t2 = dtfull$QoLDiar_2, t3 = dtfull$QoLDiar_3, t4 = dtfull$QoLDiar_4, t5 = dtfull$QoLDiar_5, t6 = dtfull$N_dia)
diarfull <- diarfull[which(complete.cases(diarfull)), ]

dtfull_diar <- diarfull %>%
  gather(key = "time", value = "diarrhea", t1, t2, t3, t4, t5, t6) %>%
  convert_as_factor(id, time)

head(dtfull_diar, 3)
#
dtfull_diar %>%
  group_by(time) %>%
  get_summary_stats(diarrhea, type="common") 

#
bxp_diar <- ggboxplot(dtfull_diar, x="time", y="diarrhea", add = "jitter")
bxp_diar

#
table(dtfull_diar$time, dtfull_diar$id)

res.fried_diar <- dtfull_diar %>% friedman_test(diarrhea ~ time |id)
res.fried_nv # The diar scale score was statistically significantly different at the different time points, X2(5) = 147, p < 0.0001

#
pwc_diar <- dtfull_diar %>%
  wilcox_test(diarrhea ~ time, paired = TRUE, p.adjust.method = "bonferroni")

pwc_diar

#
pwc_diar <- pwc_diar %>% add_xy_position(x = "time")
ggboxplot(dtfull_diar, x="time", y="diarrhea", add = "point") +
  stat_pvalue_manual(pwc_diar, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.fried_diar, detailed = TRUE),
    caption = get_pwc_label(pwc_diar)
  )

##### For fd scale #####
fdfull <- data.frame(id = dtfull$ID_ISE, t1 = dtfull$QoLFin_1, t2 = dtfull$QoLFin_2, t3 = dtfull$QoLFin_3, t4 = dtfull$QoLFin_4, t5 = dtfull$QoLFin_5, t6 = dtfull$N_fd)
fdfull <- fdfull[which(complete.cases(fdfull)), ]

dtfull_fd <- fdfull %>%
  gather(key = "time", value = "fd_score", t1, t2, t3, t4, t5, t6) %>%
  convert_as_factor(id, time)

head(dtfull_fd, 3)
#
dtfull_fd %>%
  group_by(time) %>%
  get_summary_stats(fd_score, type="common") 

#
bxp_fd <- ggboxplot(dtfull_fd, x="time", y="fd_score", add = "jitter")
bxp_fd

#
table(dtfull_fd$time, dtfull_fd$id)

res.fried_fd <- dtfull_fd %>% friedman_test(fd_score ~ time |id)
res.fried_fd # The fatigue scale score was not statistically significantly different at any different time points

