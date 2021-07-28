load("U:/Studien/Empfind/Projekte/Quality_of_life_2020/yifeng/R data/PatientsWhoAttendedBaseline&AfterRT&ISE3.Rdata")

dtafterise <- baseline_ise3_afterRT
attach(dtafterise)

###### Friedman Test with post-hoc analysis ######

library(tidyverse)
library(ggpubr)
library(rstatix)

##### GHS/QoL ##### 
qolafterise <- data.frame(id = dtafterise$ID_ISE, t1 = dtafterise$QoLGHS_1, t2 = dtafterise$QoLGHS_5, t3 = dtafterise$N_qol)
qolafterise <- qolafterise[which(complete.cases(qolafterise)), ]

dtafterise_qol <- qolafterise %>%
  gather(key = "time", value = "QoL_score", t1, t2, t3) %>%
  convert_as_factor(id, time)

head(dtafterise_qol, 3)

dtafterise_qol %>%
  group_by(time) %>%
  get_summary_stats(QoL_score, type="common") 

bxp_qol <- ggboxplot(dtafterise_qol, x="time", y="QoL_score", add = "jitter")
bxp_qol

table(dtafterise_qol$time, dtafterise_qol$id)

res.fried_qol <- dtafterise_qol %>% friedman_test(QoL_score ~ time |id)
res.fried_qol 

dtafterise_qol %>% friedman_effsize(QoL_score ~ time |id)

## Post-hoc tests
pwc_qol <- dtafterise_qol %>%
  wilcox_test(QoL_score ~ time, paired = TRUE, p.adjust.method = "bonferroni")

pwc_qol 

pwc_qol <- pwc_qol %>% add_xy_position(x = "time")
ggboxplot(dtafterise_qol, x="time", y="QoL_score", add = "point") +
  stat_pvalue_manual(pwc_qol, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.fried_qol, detailed = TRUE),
    caption = get_pwc_label(pwc_qol)
  )

##### physical functioning ##### 
pfafterise <- data.frame(id = dtafterise$ID_ISE, t1 = dtafterise$QoLPhysF_1, t2 = dtafterise$QoLPhysF_5, t3 = dtafterise$N_pf)
pfafterise <- pfafterise[which(complete.cases(pfafterise)), ]

dtafterise_pf <- pfafterise %>%
  gather(key = "time", value = "physical_functioning", t1, t2, t3) %>%
  convert_as_factor(id, time)

head(dtafterise_pf, 3)

dtafterise_pf %>%
  group_by(time) %>%
  get_summary_stats(physical_functioning, type="common") 

bxp_pf <- ggboxplot(dtafterise_pf, x="time", y="physical_functioning", add = "jitter")
bxp_pf

table(dtafterise_pf$time, dtafterise_pf$id)

res.fried_pf <- dtafterise_pf %>% friedman_test(physical_functioning ~ time |id)
res.fried_pf

## Post-hoc tests
pwc_pf <- dtafterise_pf %>%
  wilcox_test(physical_functioning ~ time, paired = TRUE, p.adjust.method = "bonferroni")

pwc_pf <- pwc_pf %>% add_xy_position(x = "time")
ggboxplot(dtafterise_pf, x="time", y="physical_functioning", add = "point") +
  stat_pvalue_manual(pwc_pf, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.fried_pf, detailed = TRUE),
    caption = get_pwc_label(pwc_pf)
  )

##### role functioning ##### 
rfafterise <- data.frame(id = dtafterise$ID_ISE, t1 = dtafterise$QoLRoleF_1, t2 = dtafterise$QoLRoleF_5, t3 = dtafterise$N_rf)
rfafterise <- rfafterise[which(complete.cases(rfafterise)), ]

dtafterise_rf <- rfafterise %>%
  gather(key = "time", value = "role_functioning", t1, t2, t3) %>%
  convert_as_factor(id, time)

head(dtafterise_rf, 3)

dtafterise_rf %>%
  group_by(time) %>%
  get_summary_stats(role_functioning, type="common") 

bxp_rf <- ggboxplot(dtafterise_rf, x="time", y="role_functioning", add = "jitter")
bxp_rf

table(dtafterise_rf$time, dtafterise_rf$id)

res.fried_rf <- dtafterise_rf %>% friedman_test(role_functioning ~ time |id)
res.fried_rf 

## Post-hoc tests
pwc_rf <- dtafterise_rf %>%
  wilcox_test(role_functioning ~ time, paired = TRUE, p.adjust.method = "bonferroni")

pwc_rf <- pwc_rf %>% add_xy_position(x = "time")
ggboxplot(dtafterise_rf, x="time", y="role_functioning", add = "point") +
  stat_pvalue_manual(pwc_rf, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.fried_rf, detailed = TRUE),
    caption = get_pwc_label(pwc_rf)
  )

##### cognitive functioning ##### 
cfafterise <- data.frame(id = dtafterise$ID_ISE, t1 = dtafterise$QoLCogF_1, t2 = dtafterise$QoLCogF_5, t3 = dtafterise$N_cf)
cfafterise <- cfafterise[which(complete.cases(cfafterise)), ]

dtafterise_cf <- cfafterise %>%
  gather(key = "time", value = "cognitive_functioning", t1, t2, t3) %>%
  convert_as_factor(id, time)

head(dtafterise_cf, 3)

dtafterise_cf %>%
  group_by(time) %>%
  get_summary_stats(cognitive_functioning, type="common") 

bxp_cf <- ggboxplot(dtafterise_cf, x="time", y="cognitive_functioning", add = "jitter")
bxp_cf

table(dtafterise_cf$time, dtafterise_cf$id)

res.fried_cf <- dtafterise_cf %>% friedman_test(cognitive_functioning ~ time |id)
res.fried_cf 

## Post-hoc tests
pwc_cf <- dtafterise_cf %>%
  wilcox_test(cognitive_functioning ~ time, paired = TRUE, p.adjust.method = "bonferroni")

pwc_cf <- pwc_cf %>% add_xy_position(x = "time")
ggboxplot(dtafterise_cf, x="time", y="cognitive_functioning", add = "point") +
  stat_pvalue_manual(pwc_cf, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.fried_cf, detailed = TRUE),
    caption = get_pwc_label(pwc_cf)
  )

##### emotional functioning ##### 
efafterise <- data.frame(id = dtafterise$ID_ISE,t1 = dtafterise$QoLEmoF_1, t2 = dtafterise$QoLEmoF_5, t3 = dtafterise$N_ef)
efafterise <- efafterise[which(complete.cases(efafterise)), ]

dtafterise_ef <- efafterise %>%
  gather(key = "time", value = "emotional_functioning", t1, t2, t3) %>%
  convert_as_factor(id, time)

head(dtafterise_ef, 3)

dtafterise_ef %>%
  group_by(time) %>%
  get_summary_stats(emotional_functioning, type="common") 

bxp_ef <- ggboxplot(dtafterise_ef, x="time", y="emotional_functioning", add = "jitter")
bxp_ef

table(dtafterise_ef$time, dtafterise_ef$id)

res.fried_ef <- dtafterise_ef %>% friedman_test(emotional_functioning ~ time |id)
res.fried_ef 

## Post-hoc tests
pwc_ef <- dtafterise_ef %>%
  wilcox_test(emotional_functioning ~ time, paired = TRUE, p.adjust.method = "bonferroni")

pwc_ef <- pwc_ef %>% add_xy_position(x = "time")
ggboxplot(dtafterise_ef, x="time", y="emotional_functioning", add = "point") +
  stat_pvalue_manual(pwc_ef, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.fried_ef, detailed = TRUE),
    caption = get_pwc_label(pwc_ef)
  )


##### social functioning ##### 
sfafterise <- data.frame(id = dtafterise$ID_ISE, t1 = dtafterise$QoLSocF_1, t2 = dtafterise$QoLSocF_5, t3 = dtafterise$N_sf)
sfafterise <- sfafterise[which(complete.cases(sfafterise)), ]

dtafterise_sf <- sfafterise %>%
  gather(key = "time", value = "social_functioning", t1, t2, t3) %>%
  convert_as_factor(id, time)

head(dtafterise_sf, 3)

dtafterise_sf %>%
  group_by(time) %>%
  get_summary_stats(social_functioning, type="common") 

bxp_sf <- ggboxplot(dtafterise_sf, x="time", y="social_functioning", add = "jitter")
bxp_sf

table(dtafterise_sf$time, dtafterise_sf$id)

res.fried_sf <- dtafterise_sf %>% friedman_test(social_functioning ~ time |id)
res.fried_sf 

## Post-hoc tests
pwc_sf <- dtafterise_sf %>%
  wilcox_test(social_functioning ~ time, paired = TRUE, p.adjust.method = "bonferroni")

pwc_sf <- pwc_sf %>% add_xy_position(x = "time")
ggboxplot(dtafterise_sf, x="time", y="social_functioning", add = "point") +
  stat_pvalue_manual(pwc_sf, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.fried_sf, detailed = TRUE),
    caption = get_pwc_label(pwc_sf)
  )

##### dyspnea ##### 
dysafterise <- data.frame(id = dtafterise$ID_ISE, t1 = dtafterise$QoLDysp_1, t2 = dtafterise$QoLDysp_5, t3 = dtafterise$N_dys)
dysafterise <- dysafterise[which(complete.cases(dysafterise)), ]

dtafterise_dys <- dysafterise %>%
  gather(key = "time", value = "dyspnea", t1, t2, t3) %>%
  convert_as_factor(id, time)

head(dtafterise_dys, 3)

dtafterise_dys %>%
  group_by(time) %>%
  get_summary_stats(dyspnea, type="common") 

bxp_dys <- ggboxplot(dtafterise_dys, x="time", y="dyspnea", add = "jitter")
bxp_dys

table(dtafterise_dys$time, dtafterise_dys$id)

res.fried_dys <- dtafterise_dys %>% friedman_test(dyspnea ~ time |id)
res.fried_dys 

## Post-hoc tests
pwc_dys <- dtafterise_dys %>%
  wilcox_test(dyspnea ~ time, paired = TRUE, p.adjust.method = "bonferroni")

pwc_dys <- pwc_dys %>% add_xy_position(x = "time")
ggboxplot(dtafterise_dys, x="time", y="dyspnea", add = "point") +
  stat_pvalue_manual(pwc_dys, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.fried_dys, detailed = TRUE),
    caption = get_pwc_label(pwc_dys)
  )


##### pain ##### 
painafterise <- data.frame(id = dtafterise$ID_ISE, t1 = dtafterise$QoLPain_1, t2 = dtafterise$QoLPain_5, t3 = dtafterise$N_pain)
painafterise <- painafterise[which(complete.cases(painafterise)), ]

dtafterise_pain <- painafterise %>%
  gather(key = "time", value = "pain", t1, t2, t3) %>%
  convert_as_factor(id, time)

head(dtafterise_pain, 3)

dtafterise_pain %>%
  group_by(time) %>%
  get_summary_stats(pain, type="common") 

bxp_pain <- ggboxplot(dtafterise_pain, x="time", y="pain", add = "jitter")
bxp_pain

table(dtafterise_pain$time, dtafterise_pain$id)

res.fried_pain <- dtafterise_pain %>% friedman_test(pain ~ time |id)
res.fried_pain 

## Post-hoc tests
pwc_pain <- dtafterise_pain %>%
  wilcox_test(pain ~ time, paired = TRUE, p.adjust.method = "bonferroni")

pwc_pain <- pwc_pain %>% add_xy_position(x = "time")
ggboxplot(dtafterise_pain, x="time", y="pain", add = "point") +
  stat_pvalue_manual(pwc_pain, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.fried_pain, detailed = TRUE),
    caption = get_pwc_label(pwc_pain)
  )


##### fatigue ##### 
fatafterise <- data.frame(id = dtafterise$ID_ISE, t1 = dtafterise$QoLFat_1, t2 = dtafterise$QoLFat_5, t3 = dtafterise$N_fatigue)
fatafterise <- fatafterise[which(complete.cases(fatafterise)), ]

dtafterise_fat <- fatafterise %>%
  gather(key = "time", value = "fatigue", t1, t2, t3) %>%
  convert_as_factor(id, time)

head(dtafterise_fat, 3)

dtafterise_fat %>%
  group_by(time) %>%
  get_summary_stats(fatigue, type="common") 

bxp_fat <- ggboxplot(dtafterise_fat, x="time", y="fatigue", add = "jitter")
bxp_fat

table(dtafterise_fat$time, dtafterise_fat$id)

res.fried_fat <- dtafterise_fat %>% friedman_test(fatigue ~ time |id)
res.fried_fat 

## Post-hoc tests
pwc_fat <- dtafterise_fat %>%
  wilcox_test(fatigue ~ time, paired = TRUE, p.adjust.method = "bonferroni")

pwc_fat <- pwc_fat %>% add_xy_position(x = "time")
ggboxplot(dtafterise_fat, x="time", y="fatigue", add = "point") +
  stat_pvalue_manual(pwc_fat, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.fried_fat, detailed = TRUE),
    caption = get_pwc_label(pwc_fat)
  )


##### insomnia ##### 
insafterise <- data.frame(id = dtafterise$ID_ISE, t1 = dtafterise$QoLInso_1, t2 = dtafterise$QoLInso_5, t3 = dtafterise$N_ins)
insafterise <- insafterise[which(complete.cases(insafterise)), ]

dtafterise_ins <- insafterise %>%
  gather(key = "time", value = "insomnia", t1, t2, t3) %>%
  convert_as_factor(id, time)

head(dtafterise_ins, 3)

dtafterise_ins %>%
  group_by(time) %>%
  get_summary_stats(insomnia, type="common") 

bxp_ins <- ggboxplot(dtafterise_ins, x="time", y="insomnia", add = "jitter")
bxp_ins

table(dtafterise_ins$time, dtafterise_ins$id)

res.fried_ins <- dtafterise_ins %>% friedman_test(insomnia ~ time |id)
res.fried_ins 

## Post-hoc tests
pwc_ins <- dtafterise_ins %>%
  wilcox_test(insomnia ~ time, paired = TRUE, p.adjust.method = "bonferroni")

pwc_ins <- pwc_ins %>% add_xy_position(x = "time")
ggboxplot(dtafterise_ins, x="time", y="insomnia", add = "point") +
  stat_pvalue_manual(pwc_ins, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.fried_ins, detailed = TRUE),
    caption = get_pwc_label(pwc_ins)
  )

##### appetite loss #####
alafterise <- data.frame(id = dtafterise$ID_ISE, t1 = dtafterise$QoLAppL_1, t2 = dtafterise$QoLAppL_5, t3 = dtafterise$N_al)
alafterise <- alafterise[which(complete.cases(alafterise)), ]

dtafterise_al <- alafterise %>%
  gather(key = "time", value = "appetite_loss", t1, t2, t3) %>%
  convert_as_factor(id, time)

head(dtafterise_al, 3)

dtafterise_al %>%
  group_by(time) %>%
  get_summary_stats(appetite_loss, type="common") 

bxp_al <- ggboxplot(dtafterise_al, x="time", y="appetite_loss", add = "jitter")
bxp_al

table(dtafterise_al$time, dtafterise_al$id)

res.fried_al <- dtafterise_al %>% friedman_test(appetite_loss ~ time |id)
res.fried_al 

## Post-hoc tests
pwc_al <- dtafterise_al %>%
  wilcox_test(appetite_loss ~ time, paired = TRUE, p.adjust.method = "bonferroni")

pwc_al <- pwc_al %>% add_xy_position(x = "time")
ggboxplot(dtafterise_al, x="time", y="appetite_loss", add = "point") +
  stat_pvalue_manual(pwc_al, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.fried_al, detailed = TRUE),
    caption = get_pwc_label(pwc_al)
  )

##### nausea/vomiting ##### 
nvafterise <- data.frame(id = dtafterise$ID_ISE, t1 = dtafterise$QoLNV_1, t2 = dtafterise$QoLNV_5, t3 = dtafterise$N_nau_vomit)
nvafterise <- nvafterise[which(complete.cases(nvafterise)), ]

dtafterise_nv <- nvafterise %>%
  gather(key = "time", value = "nausea_vomiting", t1, t2, t3) %>%
  convert_as_factor(id, time)

head(dtafterise_nv, 3)

dtafterise_nv %>%
  group_by(time) %>%
  get_summary_stats(nausea_vomiting, type="common") 

bxp_nv <- ggboxplot(dtafterise_nv, x="time", y="nausea_vomiting", add = "jitter")
bxp_nv

table(dtafterise_nv$time, dtafterise_nv$id)

res.fried_nv <- dtafterise_nv %>% friedman_test(nausea_vomiting ~ time |id)
res.fried_nv 

## Post-hoc tests
pwc_nv <- dtafterise_nv %>%
  wilcox_test(nausea_vomiting ~ time, paired = TRUE, p.adjust.method = "bonferroni")

pwc_nv <- pwc_nv %>% add_xy_position(x = "time")
ggboxplot(dtafterise_nv, x="time", y="nausea_vomiting", add = "point") +
  stat_pvalue_manual(pwc_nv, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.fried_nv, detailed = TRUE),
    caption = get_pwc_label(pwc_nv)
  )

##### constipation #####
consafterise <- data.frame(id = dtafterise$ID_ISE, t1 = dtafterise$QoLCons_1, t2 = dtafterise$QoLCons_5, t3 = dtafterise$N_cons)
consafterise <- consafterise[which(complete.cases(consafterise)), ]

dtafterise_cons <- consafterise %>%
  gather(key = "time", value = "constipation", t1, t2, t3) %>%
  convert_as_factor(id, time)

head(dtafterise_cons, 3)

dtafterise_cons %>%
  group_by(time) %>%
  get_summary_stats(constipation, type="common") 

bxp_cons <- ggboxplot(dtafterise_cons, x="time", y="constipation", add = "jitter")
bxp_cons

table(dtafterise_cons$time, dtafterise_cons$id)

res.fried_cons <- dtafterise_cons %>% friedman_test(constipation ~ time |id)
res.fried_cons 

## Post-hoc tests
pwc_cons <- dtafterise_cons %>%
  wilcox_test(constipation ~ time, paired = TRUE, p.adjust.method = "bonferroni")

pwc_cons <- pwc_cons %>% add_xy_position(x = "time")
ggboxplot(dtafterise_cons, x="time", y="constipation", add = "point") +
  stat_pvalue_manual(pwc_cons, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.fried_cons, detailed = TRUE),
    caption = get_pwc_label(pwc_cons)
  )


##### diarrhea ##### 
diarafterise <- data.frame(id = dtafterise$ID_ISE, t1 = dtafterise$QoLDiar_1, t2 = dtafterise$QoLDiar_5, t3 = dtafterise$N_dia)
diarafterise <- diarafterise[which(complete.cases(diarafterise)), ]

dtafterise_diar <- diarafterise %>%
  gather(key = "time", value = "diarrhea", t1, t2, t3) %>%
  convert_as_factor(id, time)

head(dtafterise_diar, 3)

dtafterise_diar %>%
  group_by(time) %>%
  get_summary_stats(diarrhea, type="common") 

bxp_diar <- ggboxplot(dtafterise_diar, x="time", y="diarrhea", add = "jitter")
bxp_diar

table(dtafterise_diar$time, dtafterise_diar$id)

res.fried_diar <- dtafterise_diar %>% friedman_test(diarrhea ~ time |id)
res.fried_diar # The diar scale score was statistically significantly different at the different time points, X2(5) = 147, p < 0.0001

## Post-hoc tests
pwc_diar <- dtafterise_diar %>%
  wilcox_test(diarrhea ~ time, paired = TRUE, p.adjust.method = "bonferroni")

pwc_diar <- pwc_diar %>% add_xy_position(x = "time")
ggboxplot(dtafterise_diar, x="time", y="diarrhea", add = "point") +
  stat_pvalue_manual(pwc_diar, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.fried_diar, detailed = TRUE),
    caption = get_pwc_label(pwc_diar)
  )

##### financial difficulty #####
fdafterise <- data.frame(id = dtafterise$ID_ISE, t1 = dtafterise$QoLFin_1, t2 = dtafterise$QoLFin_5, t3 = dtafterise$N_fd)
fdafterise <- fdafterise[which(complete.cases(fdafterise)), ]

dtafterise_fd <- fdafterise %>%
  gather(key = "time", value = "financial_difficulty", t1, t2, t3) %>%
  convert_as_factor(id, time)

head(dtafterise_fd, 3)

dtafterise_fd %>%
  group_by(time) %>%
  get_summary_stats(financial_difficulty, type="common") 

bxp_fd <- ggboxplot(dtafterise_fd, x="time", y="financial_difficulty", add = "jitter")
bxp_fd

table(dtafterise_fd$time, dtafterise_fd$id)

res.fried_fd <- dtafterise_fd %>% friedman_test(financial_difficulty ~ time |id)
res.fried_fd 

## Post-hoc tests
pwc_fd <- dtafterise_fd %>%
  wilcox_test(financial_difficulty ~ time, paired = TRUE, p.adjust.method = "bonferroni")

pwc_fd <- pwc_fd %>% add_xy_position(x = "time")
ggboxplot(dtafterise_fd, x="time", y="financial_difficulty", add = "point") +
  stat_pvalue_manual(pwc_fd, hide.ns = TRUE) +
  labs(
    subtitle = get_test_label(res.fried_fd, detailed = TRUE),
    caption = get_pwc_label(pwc_fd)
  )
