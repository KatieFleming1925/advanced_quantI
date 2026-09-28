###################################
###################################
#####  REPLICATION FILE FOR: ######
###  MY HISTORY OR OUR HISTORY? ###
###################################
###################################

# ========================================================================= #
# - Script: Analysis for Online Appendix (and Dataverse Supplementary Tables)
# - Author: Nicholas Haas (nick.haas@ps.au.dk) 
#           Emmy Lindstam (emmy.lindstam@ie.edu)
# ========================================================================= #

# Clear environment
rm(list = ls())

# Load packages and data for analysis:
source(here::here("00 - PACKAGES.R"))
load(here("data/DataforAnalysis.Rdata"))

# =============================================================#

# Overview of files where the different appendices are reproduced:

# APPENDIX A --> See APPENDIX.do
# APPENDIX B --> See ANALYSIS.R
# APPENDIX C --> This script (see below) + APPENDIX.do
# APPENDIX D --> This script

#==============================================================#

# Appendix C - Additional Results and Robustness               #

#==============================================================#

# In this part you can find code to replicate the following tables and figures:

# - Table C9
# - Figure C1
# - Figure C2
# - Figure C3
# - Figure C4
# - Table C10
# - Figure C5
# - Table C11
# - Table C16
# - Figure C8
# - Figure C9
# - Table C17
# - Table C18
# - Table C19

# Results from Appendix C that are not reproduced in this script are reproduced in APPENDIX.do

#================================================================

# TABLE C9: Effects of Historical Representations on Willingness to Lead (interaction) # --------

# Check that the interaction is significant 
mainI <- lm(WillingR~Treat*Religion, data=subset(dataS))
summary(mainI)

# Check with pre-treatment controls 
mainIC <- lm(WillingR~Treat*Religion+Gender+Age+EducationHigh+State+GroupCon+Round+as.factor(Comp), data=subset(dataS))
summary(mainIC)

# EXTRA --> Also confirm that inclusive is statistically different from exclusive (it is)
mainII <- lm(WillingR~relevel(Treat, ref = "2") + Gender+Age+EducationHigh+State+GroupCon+Round+as.factor(Comp), data=subset(dataS, Religion==2))
summary(mainII)

# Make table C9 for appendix # -------------------
stargazer(mainI, mainIC,
          dep.var.labels = "Willingness to Lead (1-4)" ,
          title = "\\textbf{Effect of Historical Representations on Willingness to Lead}",
          omit = "Constant",
          covariate.labels = c("Exclusive", "Inclusive", "Muslim", "Female", "Age", "Education High", "Uttar Pradesh", "Gujarat", "Maharashtra", "Group Consciousness", "Survey Round", "Hindu Majority", "Muslim Majority", "Exclusive*Muslim", "Inclusive*Muslim"),
          keep.stat = c("n", "adj.rsq"),
          out = here("Output/Appendix/TableC9.tex"), label = "t1")

#================================================================

# FIGURE C1: Treatment effects of historical representations on perceptions of Hindu and Muslim MLAs among Muslims # -----------

# Make a list including all models then plot:
list(
  follow1_ord1 = lm(MLA_Follow_1~Treat, data=subset(dataS, MLAOrd==1&Religion==2)),
  follow2_ord1 = lm(MLA_Follow_2~Treat, data=subset(dataS, MLAOrd==1&Religion==2)),
  follow3_ord1 = lm(MLA_Follow_3~Treat, data=subset(dataS, MLAOrd==1&Religion==2)),
  follow4_ord1 = lm(MLA_Follow_4~Treat, data=subset(dataS, MLAOrd==1&Religion==2)),
  follow5_ord1 = lm(MLA_Follow_5~Treat, data=subset(dataS, MLAOrd==1&Religion==2)),
  follow6_ord1 = lm(MLA_Follow_6~Treat, data=subset(dataS, MLAOrd==1&Religion==2)),
  follow7_ord1 = lm(MLA_Follow_7~Treat, data=subset(dataS, MLAOrd==1&Religion==2)),
  follow1_ord0 = lm(MLA_Follow_1~Treat, data=subset(dataS, MLAOrd==0&Religion==2)),
  follow2_ord0 = lm(MLA_Follow_2~Treat, data=subset(dataS, MLAOrd==0&Religion==2)),
  follow3_ord0 = lm(MLA_Follow_3~Treat, data=subset(dataS, MLAOrd==0&Religion==2)),
  follow4_ord0 = lm(MLA_Follow_4~Treat, data=subset(dataS, MLAOrd==0&Religion==2)),
  follow5_ord0 = lm(MLA_Follow_5~Treat, data=subset(dataS, MLAOrd==0&Religion==2)),
  follow6_ord0 = lm(MLA_Follow_6~Treat, data=subset(dataS, MLAOrd==0&Religion==2)),
  follow7_ord0 = lm(MLA_Follow_7~Treat, data=subset(dataS, MLAOrd==0&Religion==2))
) %>%
  map_dfr(tidy, conf.int = T, conf.level = .9, .id = "model") %>% 
  separate(model, c("DV","MLA"),"_") %>% 
  filter(term %in% c("Treat2","Treat3")) %>%
  mutate(MLA = recode_factor(MLA,
                             ord1 = "Hindu MLA",
                             ord0 = "Muslim MLA"),
         DV = recode_factor(DV,
                            follow7 = "Prototypical",
                            follow6 = "Comfortable\nbeing represented",
                            follow5 = "Popular",
                            follow4 = "Represents\nIndia",
                            follow3 = "Represents\nConstituents",
                            follow2 = "Deserving\nof office",
                            follow1 = "Qualified\nfor office"),
         Treatment = recode_factor(term,
                                   Treat2 = "Exclusive",
                                   Treat3 = "Inclusive")) %>% 
  ggplot(aes(x = estimate, xmin = conf.low, xmax = conf.high, y = DV, 
             col = Treatment, shape = Treatment)) +
  geom_vline(xintercept = 0, col = "grey") +
  geom_point(position = position_dodge(-0.3), size=2) +
  geom_errorbarh(height=0, position = position_dodge(-0.3)) +
  facet_wrap(~MLA) +
  custom_theme() +
  labs(y = "Dependent Variable", x = "Coefficient") + 
  scale_colour_manual(values=c("orange","darkgreen"))

# Save Figure C1:
ggsave(path="Output/Appendix", filename="FigureC1.pdf", height=5, width=8)

# Make table for dataverse doc:
follow1_ord1 <- lm(MLA_Follow_1~Treat, data=subset(dataS, MLAOrd==1&Religion==2))
follow2_ord1 <- lm(MLA_Follow_2~Treat, data=subset(dataS, MLAOrd==1&Religion==2))
follow3_ord1 <- lm(MLA_Follow_3~Treat, data=subset(dataS, MLAOrd==1&Religion==2))
follow4_ord1 <- lm(MLA_Follow_4~Treat, data=subset(dataS, MLAOrd==1&Religion==2))
follow5_ord1 <- lm(MLA_Follow_5~Treat, data=subset(dataS, MLAOrd==1&Religion==2))
follow6_ord1 <- lm(MLA_Follow_6~Treat, data=subset(dataS, MLAOrd==1&Religion==2))
follow7_ord1 <- lm(MLA_Follow_7~Treat, data=subset(dataS, MLAOrd==1&Religion==2))
follow1_ord0 <- lm(MLA_Follow_1~Treat, data=subset(dataS, MLAOrd==0&Religion==2))
follow2_ord0 <- lm(MLA_Follow_2~Treat, data=subset(dataS, MLAOrd==0&Religion==2))
follow3_ord0 <- lm(MLA_Follow_3~Treat, data=subset(dataS, MLAOrd==0&Religion==2))
follow4_ord0 <- lm(MLA_Follow_4~Treat, data=subset(dataS, MLAOrd==0&Religion==2))
follow5_ord0 <- lm(MLA_Follow_5~Treat, data=subset(dataS, MLAOrd==0&Religion==2))
follow6_ord0 <- lm(MLA_Follow_6~Treat, data=subset(dataS, MLAOrd==0&Religion==2))
follow7_ord0 <- lm(MLA_Follow_7~Treat, data=subset(dataS, MLAOrd==0&Religion==2))

# Tables 2 and 3 of the Extra Supplementary Materials: # ---------------------
# Table 2
stargazer(follow1_ord1, follow2_ord1, follow3_ord1, follow4_ord1, follow5_ord1, follow6_ord1, follow7_ord1,
          dep.var.labels = c("Qualified", "Deserving", "Repr. Constituents", "Repr. India", "Popular", "Comfortable", "Prototypical"),
          title = "\\textbf{Treatment Effects of Historical Representations on Perceptions of Hindu MLAs among Muslims}",
          omit = "Constant",
          covariate.labels = c("Exclusive", "Inclusive"),
          keep.stat = c("n", "adj.rsq"),
          out = here("Output/Appendix/Table2_SuppAppendix.tex"))

# Table 3
stargazer(follow1_ord0, follow2_ord0, follow3_ord0, follow4_ord0, follow5_ord0, follow6_ord0, follow7_ord0,
          dep.var.labels = c("Qualified", "Deserving", "Repr. Constituents", "Repr. India", "Popular", "Comfortable", "Prototypical"),
          title = "\\textbf{Treatment Effects of Historical Representations on Perceptions of Muslim MLAs among Muslims}",
          omit = "Constant",
          covariate.labels = c("Exclusive", "Inclusive"),
          keep.stat = c("n", "adj.rsq"),
          out = here("Output/Appendix/Table3_SuppAppendix.tex"))

#================================================================

# FIGURE C2: Treatment effects on the perceived entitlement of Hindu MLA # ----------------------

# Hindu MLA Qualified -------------------------------- #
Over_QualPol <- lm(as.numeric(MLA_Follow_1)~Treat, data=subset(dataS, MLAOrd==1))
M_QualPol <- lm(as.numeric(MLA_Follow_1)~Treat, data=subset(dataS, Religion==2&MLAOrd==1))
H_QualPol <- lm(as.numeric(MLA_Follow_1)~Treat, data=subset(dataS, Religion==1&MLAOrd==1))

# Hindu MLA Deserving ----------------------------------#
Over_DesPol <- lm(as.numeric(MLA_Follow_2)~Treat, data=subset(dataS, MLAOrd==1))
M_DesPol <- lm(as.numeric(MLA_Follow_2)~Treat, data=subset(dataS, Religion==2&MLAOrd==1))
H_DesPol <- lm(as.numeric(MLA_Follow_2)~Treat, data=subset(dataS, Religion==1&MLAOrd==1))

# Save models  
models <- list(
  model1 = Over_QualPol,
  model2 = M_QualPol,
  model3 = H_QualPol,
  model4 = Over_DesPol,
  model5 = M_DesPol,
  model6 = H_DesPol)

# Make plot for Figure C2
models %>% 
  map_dfr(tidy, conf.int = T, conf.level = .95, .id="model") %>%
  rename(conf.low_95 = conf.low, conf.high_95 = conf.high) %>% 
  left_join(map_dfr(models, tidy, conf.int=T, .id = "model", conf.level = .90)) %>% 
  rename(conf.low_90 = conf.low, conf.high_90 = conf.high) %>% 
  rename(Treatment = term,
         Coefficient = estimate,
         SE = std.error) %>%
  filter(Treatment %in% c("Treat2","Treat3")) %>%
  mutate(Treatment = recode_factor(Treatment,
                                   Treat2 = "Exclusive",
                                   Treat3 = "Inclusive"),
         model = recode_factor(model, 
                               model1 = "Hindu MLA: \nQualified",
                               model2 = "Hindu MLA: \nQualified",
                               model3 =  "Hindu MLA: \nQualified",
                               model4 = "Hindu MLA: \nDeserving",
                               model5 = "Hindu MLA: \nDeserving",
                               model6 =  "Hindu MLA: \nDeserving"),
         Respondent = c(rep("Overall", 2), rep("Muslims", 2), rep("Hindus", 2), rep("Overall", 2), rep("Muslims", 2), rep("Hindus", 2)), 
         Respondent = fct_relevel(Respondent, "Overall", "Hindus", "Muslims"),
         Centrality = c(rep("Muslim MLA",6), rep("Self-Assessment",6)),
         y_min = c(rep(-0.8, 6), rep(-0.3, 6)),
         y_max = c(rep(0.8, 12))) %>%
  ggplot(aes(x = Treatment, col = Respondent, y = Coefficient, shape = Respondent)) + 
  geom_hline(yintercept = 0, colour = gray(1/2), lty=1) +
  geom_point(size=2, position = position_dodge(0.3)) +
  geom_linerange(aes(ymin = conf.low_90, ymax = conf.high_90),
                 lwd = 1, position = position_dodge(0.3)) + 
  geom_linerange(aes(ymin = conf.low_95, ymax = conf.high_95),
                 lwd = 1/2, position = position_dodge(0.3)) +
  facet_wrap(~model, ncol=2) +
  scale_y_continuous(breaks = seq(-0.3, .3, by = 0.3)) +
  theme(legend.position = "bottom") +
  custom_theme() +
  scale_colour_manual(values=c("darkgrey", "orange", "darkgreen"))

# Save Figure C2:
ggsave(path="Output/Appendix", filename="FigureC2.pdf", height=4, width=8)

# Table 1 for Supplementary Materials on Dataverse # ----------------------------
stargazer(Over_QualPol, M_QualPol, H_QualPol, Over_DesPol, M_DesPol,H_DesPol,
          dep.var.labels = c("Hindu MLA: Qualified", "Hindu MLA: Deserving"),
          title = "\\textbf{Treatment Effects on the Perceived Entitlement of Hindu MLA}",
          column.labels = rep(c("\\textit{Overall}", "\\textit{Muslims}", "\\textit{Hindus}"),2),
          omit = "Constant",
          covariate.labels = c("Exclusive", "Inclusive"),
          keep.stat = c("n", "adj.rsq"),
          out = here("Output/Appendix/Table1_SuppAppendix.tex")) 

#================================================================

# FIGURE C3: Demand for Muslim Leaders among Hindu Respondents with High and Low Levels of Group consciousness # -----------------

summary(dataS$GroupCon) # Median value is 0.73

# Make variable Hindu - Muslim partner
dataS$Demandif <- dataS$DemandHH - dataS$DemandHM
table(dataS$Demandif)

# Among those with low group consciousness
diff2 <- lm(as.numeric(Demandif)~Treat, data=subset(dataS, GroupCon<0.73)) # under median
summary(diff2)

# Among those with high group consciousness
diff3 <- lm(as.numeric(Demandif)~Treat, data=subset(dataS, GroupCon>0.73)) # above median
summary(diff3)

# Use ggmeans for CIs
pred <- ggemmeans(diff2, terms=c("Treat"))

# Make plot showing expected averages + confidence intervals 
pred <- pred %>% 
  mutate(Treatment = recode_treatment(x))

dif2 <- pred %>% 
  ggplot(aes(x= Treatment, y = predicted, group=1)) + 
  geom_point(col="darkorange", size=2)+
  geom_line(col="darkorange", lty="dashed", alpha=0.7) +
  geom_text_repel(aes(label=round(predicted,2)), vjust=0.5, nudge_x=.15, size=3.5, min.segment.length = Inf) +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high, alpha=0.7), col="darkorange") +
  labs(y="Hindu Bias (-2 to 2)") +
  ylim(-0.6,0.7) +
  ggtitle("Low Group Consciousness") +
  scale_alpha(guide = 'none') +
  custom_theme()

dif2

# Use ggmeans for CIs
pred <- ggemmeans(diff3, terms=c("Treat"))

# Make plot showing expected averages + confidence intervals 
pred <- pred %>% 
  mutate(Treatment = recode_treatment(x))

dif3 <- pred %>% 
  ggplot(aes(x= Treatment, y = predicted, group=1)) + 
  geom_point(col="darkorange", size=2)+
  geom_line(col="darkorange", lty="dashed", alpha=0.7) +
  geom_text_repel(aes(label=round(predicted,2)), vjust=0.5, nudge_x=.15, size=3.5, min.segment.length = Inf) +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high, alpha=0.7), col="darkorange") +
  labs(y="Hindu Bias (-2 to 2)") +
  ylim(-0.6,0.7) +
  ggtitle("High Group Consciousness") +
  scale_alpha(guide = 'none') +
  custom_theme()

dif3

# Produce Figure C3
q <- arrangeGrob(dif2, dif3, ncol=2, nrow=1) 
ggsave(path="Output/Appendix", filename="FigureC3.pdf", q, height=4, width=10)

# Produce Table 4 for the Supplementary Materials on Dataverse # -----------------
stargazer(diff2, diff3,
          dep.var.labels = "Pro-Hindu Bias",
          title = "\\textbf{Demand for Muslim Leaders among Hindu Respondents with High and Low Levels of Group Consciousness}",
          column.labels = rep(c("\\textit{Low GP}", "\\textit{High GP}", "\\textit{Hindus}"),2),
          omit = "Constant",
          covariate.labels = c("Exclusive", "Inclusive"),
          keep.stat = c("n", "adj.rsq"),
          out = here("Output/Appendix/Table4_SuppAppendix.tex")) 

#================================================================

# FIGURE C4: Treatment Effects (with Distributions) # ----------------------

# Figure A #

# Effect of treatments on willingness to lead among Muslims
main <- lm(WillingR~Treat, data=subset(dataS, Religion==2))
summary(main)

# Use "ggmeans" to obtain mean values with confidence intervals 
pred <- ggemmeans(main, terms=c("Treat"))

# Make plot showing expected means with confidence intervals ------------------------------------
pred <- pred %>% 
  mutate(Treatment = recode_treatment(x))

datM <- main$model %>% 
  mutate(predicted = WillingR,
         Treatment = recode_treatment(Treat))

# Make figure C4_A
FigureC4_A <- pred %>% 
  ggplot(aes(x= Treatment, y = predicted)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0) +
  geom_point(data = datM,
             position = position_jitter(width = 0.2, height = 0.1), 
             alpha = 0.2, stroke = 0) +
  coord_cartesian(ylim = c(1, 4)) +
  theme_bw() +
  labs(title="", subtitle = "Panel A (Muslim participants)") +
  ylab("Willingness to Lead (1-4)")  +
  custom_theme()

# Figure B #

# Effect of treatments on willingness to lead among Hindus
mainH <- lm(WillingR~Treat, data=subset(dataS, Religion==1))
summary(mainH)

# Use "ggmeans" to obtain mean values with confidence intervals 
pred2 <- ggemmeans(mainH, terms=c("Treat"))

# Make plot showing expected means with confidence intervals ------------------------------------
pred2 <- pred2 %>% 
  mutate(Treatment = recode_treatment(x))

datH <- mainH$model %>% 
  mutate(predicted = WillingR,
         Treatment = recode_treatment(Treat))

# Make figure C4_B
FigureC4_B <- pred2 %>% 
  ggplot(aes(x= Treatment, y = predicted)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0) +
  geom_point(data = datM,
             position = position_jitter(width = 0.2, height = 0.1), 
             alpha = 0.2, stroke = 0) +
  coord_cartesian(ylim = c(1, 4)) +
  theme_bw() +
  labs(title="", subtitle = "Panel B (Hindu participants)") +
  ylab("Willingness to Lead (1-4)")  +
  custom_theme()

# Figure C #

# Effect of treatments on pro-Hindu bias among Hindus
dataS$Demandif <- dataS$DemandHH - dataS$DemandHM
hindubias <- lm(Demandif~Treat, data=dataS)
summary(hindubias)

# Use "ggmeans" to obtain mean values with confidence intervals 
pred3 <- ggemmeans(hindubias, terms=c("Treat"))

# Make plot showing expected means with confidence intervals ------------------------------------
pred3 <- pred3 %>% 
  mutate(Treatment = recode_treatment(x))

datH2 <- hindubias$model %>% 
  mutate(predicted = Demandif,
         Treatment = recode_treatment(Treat))

# Make figure C4_C
FigureC4_C <- pred3 %>% 
  ggplot(aes(x= Treatment, y = predicted)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0) +
  geom_point(data = datH2,
             position = position_jitter(width = 0.2, height = 0.1), 
             alpha = 0.2, stroke = 0) +
  coord_cartesian(ylim = c(-2, 2)) +
  theme_bw() +
  labs(title="", subtitle = "Panel C (Hindu participants)") +
  ylab("Pro-Hindu Bias (-2 to 2)")  +
  custom_theme()

# Figure D #

# Effect of treatments on Hindus perception of MLA
mla <- lm(MLA_Follow_1~Treat, data=subset(dataS, Religion==1&MLAOrd==0))
summary(mla)

# Use "ggmeans" to obtain mean values with confidence intervals 
pred4 <- ggemmeans(mla, terms=c("Treat"))

# Make plot showing expected means with confidence intervals ------------------------------------
pred4 <- pred4 %>% 
  mutate(Treatment = recode_treatment(x))

datH3 <- mla$model %>% 
  mutate(predicted = MLA_Follow_1,
         Treatment = recode_treatment(Treat))

# Make figure C4_B
FigureC4_D <- pred4 %>% 
  ggplot(aes(x= Treatment, y = predicted)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0) +
  geom_point(data = datH3,
             position = position_jitter(width = 0.2, height = 0.1), 
             alpha = 0.2, stroke = 0) +
  coord_cartesian(ylim = c(1, 5)) +
  theme_bw() +
  labs(title="", subtitle = "Panel D (Hindu participants)") +
  ylab("Muslim MLA Qualified (1-5)")  +
  custom_theme()

# Save the plots together 
l <- arrangeGrob(FigureC4_A, FigureC4_B, FigureC4_C, FigureC4_D, ncol=2, nrow=2) 
ggsave(path="Output/Appendix", filename="FigureC4.pdf", l, height=10, width=8) 

#================================================================

# TABLE C10: Effects of historical representations on behavioral measure of willingness to seek out information on citizenship participation # -------------

# Treatment effects on propensity to visit website among Muslims
Mvisit <- lm(Visited~Treat, data=subset(dataS, Religion==2))
summary(Mvisit)

# Treatment effects on propensity to visit website among Hindus
Hvisit <- lm(Visited~Treat, data=subset(dataS, Religion==1))
summary(Hvisit)

# Table C10:
stargazer(Mvisit, Hvisit,
          dep.var.labels = "Visited Website",
          title = "\\textbf{Effects of Historical Representations on Behavioral Measure of Willingness to Seek Out Information on Citizenship Participation}",
          omit = "Constant",
          covariate.labels = c("Exclusive", "Inclusive"),
          keep.stat = c("n", "adj.rsq"),
          out = here("Output/Appendix/TableC10.tex")) 

#================================================================

# FIGURE C5: 20 most frequently mentioned words # --------------------

# Paste "WhyWant" and "WhyNot"
dataS$Why <- paste(ifelse(!is.na(dataS$WhyWant), dataS$WhyWant, ""), 
                   ifelse(!is.na(dataS$WhyNot), dataS$WhyNot, ""))

# Produce Figure C5
dataS %>% 
  unnest_tokens(word, Why) %>% 
  anti_join(get_stopwords()) %>% 
  count(word, sort = T) %>% 
  mutate(word = fct_reorder(word, n)) %>%
  slice_head(n = 20) %>% 
  ggplot(aes(x = n, y = word, reo)) +
  geom_col(fill="darkgreen", alpha=0.6) +
  custom_theme()

# Save figure
ggsave(path="Output/Appendix", filename="FigureC5.pdf", height=5, width=5)


#================================================================

# TABLE C11: Percentage of respondents who mentioned words related to leadership, suitability and alternative perceptions of the role # ------------

mean(grepl("represent", dataS$Why))
mean(grepl("lead", dataS$Why))
mean(grepl("good", dataS$Why))
mean(grepl("quali", dataS$Why))
mean(grepl("sociology", dataS$Why))
mean(grepl("geography", dataS$Why))
mean(grepl("earn", dataS$Why))

#================================================================

# TABLE C16: The effect of historical representations on reported engagement in the study and score in the history exercises # ------------

# Treatment effects on perceived engagement among Muslims
EngageM <- lm(Pers_Engagement~Treat, data=subset(dataS, Religion==2))
summary(EngageM)

# Treatment effects on perceived engagement among Hindus
EngageH <- lm(Pers_Engagement~Treat, data=subset(dataS, Religion==1))
summary(EngageH)

# Treatment effects on history score among Muslims
ScoreM <- lm(Score~Treat, data=subset(dataS, Religion==2))
summary(ScoreM)

# Treatment effects on history score among Hindus
ScoreH <- lm(Score~Treat, data=subset(dataS, Religion==1))
summary(ScoreH)

# Table C16:
stargazer(EngageM, EngageH, ScoreM, ScoreH,
          dep.var.labels = c("Engagement", "Score"),
          title = "\\textbf{The Effect of Historical Representations on Reported Engagement in the Study and Score in the History Exercises}",
          omit = "Constant",
          column.labels = c("\\textit{Muslims}", "\\textit{Hindus}", "\\textit{Muslims}", "\\textit{Hindus}"),
          covariate.labels = c("Exclusive", "Inclusive"),
          keep.stat = c("n", "adj.rsq"),
          out = here("Output/Appendix/TableC16.tex"))

#================================================================

# Figure C8: Conditional effects of the inclusive history treatment on Muslim Respondents' willingness to lead given comprehension scores # -----------

# Code score as factor
dataS$Score <- as.factor(dataS$Score)

# Run interaction model
score <- lm(WillingR~Score*Treat, data=subset(dataS, Religion==2))
summary(score)

# Plot marginal effects given score using "marginaleffects" package:
plot_slopes(score, variables="Treat", condition="Score", 
            draw=FALSE) %>% 
  filter(contrast != "2 - 1") %>% 
  ggplot(aes(x = Score, y = estimate)) +
  geom_hline(yintercept = 0, col = "grey") +
  labs(y = "Marginal Effects", x = "Score") +
  geom_point(col="darkgreen") +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high),
                 col="darkgreen") +
  custom_theme()

# Save figure
ggsave(path="Output/Appendix", filename="FigureC8.pdf", height=4, width=5)

# Make Table 5 for Extra Supplementary Materials for Dataverse: # ---------------------
stargazer(score,
          dep.var.labels = "Willingness to Lead (1-4)",
          title = "\\textbf{Conditional Effects of the History Treatment on Muslim Respondents' Willingness to Lead Given Comprehension Scores}",
          omit = c("Treat2", "Score2:Treat2", "Score3:Treat2", "Score4:Treat2", "Constant"),
          covariate.labels = c("Score 2", "Score 3", "Score 4", "Inclusive", "Score 2*Inclusive", "Score 3*Inclusive", "Score 4*Inclusive"),
          keep.stat = c("n", "adj.rsq"),
          out = here("Output/Appendix/Supp5.tex")) 

#================================================================

# FIGURE C9: Treatment effects on alternative measures of perceived contributions # -----------------

table(dataS$words)

# Make a function to count the number of Hindu and Muslim words respondents picked from the pool of words.
countWords <- function(stringvar, grouping) {
  str_split(stringvar, ",", simplify = T) %>% 
    as_tibble() %>%
    mutate_all(match, table = grouping, nomatch = 0) %>%
    apply(1, function(x) sum(x > 0))
}

# Count number of Hindu words not included in the exclusive text
dataS$hinducontrEX3 <- countWords(dataS$words, as.character(c(4,17,18)))

# Count number of non-Muslim words included in the inclusive text
dataS$muslimcontrEX3 <- countWords(dataS$words, as.character(c(2,3)))

# Overview
table(dataS$hinducontrEX3)
table(dataS$muslimcontrEX3)

# Make dummy indicating if someone picked Hindu word not in exclusive treatment
dataS$dum <- ifelse((dataS$hinducontrEX3!=0),1,0)
table(dataS$dum)
# Make dummy indicating if someone picked non-Muslim words included in inclusive treatment
dataS$dum2 <- ifelse((dataS$muslimcontrEX3!=0),1,0)
table(dataS$dum2)

# Treatment effect on propensity to pick Hindu words not in text in text among Hindus
test1 <- lm(dum~Treat, data=subset(dataS, Religion==1))
summary(test1)
# Treatment effect on propensity to pick Hindu words not in text in text among Muslims
test2 <- lm(dum~Treat, data=subset(dataS, Religion==2))
summary(test2)
# Treatment effect on propensity to pick non-Muslims words included in text among Hindus
test3 <- lm(dum2~Treat, data=subset(dataS, Religion==1))
summary(test3)
# Treatment effect on propensity to pick non-Muslim words included in text among Muslims
test4 <- lm(dum2~Treat, data=subset(dataS, Religion==2))
summary(test4)

# Save models 
models <- list(
  model1 = test1,
  model2 = test2,
  model3 = test3,
  model4 = test4)

# Make plot (showing results separately for Hindus and Muslims)
models %>% 
  map_dfr(tidy, conf.int = T, conf.level = .95, .id="model") %>%
  rename(conf.low_95 = conf.low, conf.high_95 = conf.high) %>% 
  left_join(map_dfr(models, tidy, conf.int=T, .id = "model", conf.level = .90)) %>% 
  rename(conf.low_90 = conf.low, conf.high_90 = conf.high) %>% 
  rename(Treatment = term,
         Coefficient = estimate,
         SE = std.error) %>%
  filter(Treatment %in% c("Treat2","Treat3")) %>%
  mutate(Treatment = recode_factor(Treatment,
                                   Treat2 = "Exclusive",
                                   Treat3 = "Inclusive"),
         model = recode_factor(model, 
                               model1 = "Hindu Words \nNot in Excl Treatment",
                               model2 = "Hindu Words \nNot in Excl Treatment",
                               model3 = "Non-Muslim Words \nin Incl Treatment",
                               model4 = "Non-Muslim Words \nin Incl Treatment"),
         Religion = c(rep("Hindus", 2), rep("Muslims", 2), rep("Hindus", 2), rep("Muslims", 2)),
         Religion = fct_relevel(Religion, "Hindus", "Muslims"))  %>% 
  ggplot(aes(x = Treatment, col = Religion, y = Coefficient, shape = Religion)) + 
  geom_hline(yintercept = 0, colour = gray(1/2), lty=1) +
  geom_point(size=2, position = position_dodge(0.3)) +
  geom_linerange(aes(ymin = conf.low_90, ymax = conf.high_90),
                 lwd = 1, position = position_dodge(0.3)) + 
  geom_linerange(aes(ymin = conf.low_95, ymax = conf.high_95),
                 lwd = 1/2, position = position_dodge(0.3)) +
  facet_wrap(~model) +
  ylim(-0.3, 0.3) +
  scale_colour_manual(values=c("orange", "darkgreen")) +
  custom_theme()

# Save figure C9
ggsave(path="Output/Appendix", filename="FigureC9.pdf", height=4, width=6)

# Make Table 6 for Supplementary Materials for Dataverse:
stargazer(test1,test2,test3,test4,
          dep.var.labels = c("Hindu Words Not in in Treatment", "Non-Muslim Words in Treatment"),
          title = "\\textbf{Treatment Effects on Alternative Measures of Perceived Contributions}",
          omit = "Constant",
          column.labels = c("\\textit{Hindus}", "\\textit{Muslims}", "\\textit{Hindus}", "\\textit{Muslims}"),
          covariate.labels = c("Exclusive", "Inclusive"),
          keep.stat = c("n", "adj.rsq"),
          out = here("Output/Appendix/Table6_SuppAppendix.tex"))

#================================================================

# TABLE C17: Heterogeneous effects given gender and caste identity # --------------------

# Heterogeneous effects given gender (Muslims)
HGenderM <- lm(WillingR~Treat*Gender, data=subset(dataS, Religion==2))
summary(HGenderM)

# Heterogeneous effects given gender (Hindus)
HGenderH <- lm(WillingR~Treat*Gender, data=subset(dataS, Religion==1))
summary(HGenderH)

# Heterogeneous effects given caste (Muslims)
HCasteM <- lm(WillingR~Treat*Caste, data=subset(dataS, Religion==2))
summary(HCasteM)

# Heterogeneous effects given caste (Hindus)
HCasteH <- lm(WillingR~Treat*Caste, data=subset(dataS, Religion==1))
summary(HCasteH)

# Produce Table C17: # -------------------------------
stargazer(HGenderM, HGenderH, HCasteM, HCasteH,
          dep.var.labels = "Willingness to Lead",
          title = "\\textbf{Heterogeneous Effects Given Gender and Caste Identity}",
          omit = "Constant",
          column.labels = c("\\textit{Muslims}", "\\textit{Hindus}", "\\textit{Muslims}", "\\textit{Hindus}"),
          covariate.labels = c("Exclusive", "Inclusive", "Woman", "Exclusive*Woman", "Inclusive*Woman", "Low Caste", "Exclusive*Low Caste", "Inclusive*Low Caste"),
          keep.stat = c("n", "adj.rsq"),
          out = here("Output/Appendix/TableC17.tex"))

#================================================================

# TABLE C18: Heterogeneous effects given BJP support and levels of group consciousness # -------------------

# Heterogeneous effects given BJP vote (Muslims)
HBJPM <- lm(WillingR~Treat*BJPvote, data=subset(dataS, Religion==2))
summary(HBJPM)

# Heterogeneous effects given BJP vote (Hindus)
HBJPH <- lm(WillingR~Treat*BJPvote, data=subset(dataS, Religion==1))
summary(HBJPH)

# Heterogeneous effects given group consciousness (Muslims)
HGPM <- lm(WillingR~Treat*GroupCon, data=subset(dataS, Religion==2))
summary(HGPM)

# Heterogeneous effects given group consciousness (Hindus)
HGPH <- lm(WillingR~Treat*GroupCon, data=subset(dataS, Religion==1))
summary(HGPH)

# Produce Table C18:
stargazer(HBJPM, HBJPH, HGPM, HGPH,
          dep.var.labels = "Willingness to Lead",
          title = "\\textbf{Heterogeneous Effects Given BJP Support and Levels of Group Consciousness}",
          omit = "Constant",
          column.labels = c("\\textit{Muslims}", "\\textit{Hindus}", "\\textit{Muslims}", "\\textit{Hindus}"),
          covariate.labels = c("Exclusive", "Inclusive", "BJP Vote", "Exclusive*BJP Vote", "Inclusive*BJP Vote", "Group Consciousness", "Exclusive*Group Consciousness", "Inclusive*Group Consciousness"),
          keep.stat = c("n", "adj.rsq"),
          out = here("Output/Appendix/TableC18.tex"))  

#================================================================

# TABLE C19: The conditional effect of the treatments given group consciousness on feelings of being qualified and worthy to lead # -----------------

# Qualified (Muslims)
mainGCM1 <- lm(WorthQual_1~Treat*GroupCon, data=subset(dataS, Religion==2))
summary(mainGCM1)

# Qualified (Hindus)
mainGCH1 <- lm(WorthQual_1~Treat*GroupCon, data=subset(dataS, Religion==1))
summary(mainGCH1)

# Worthy (Muslims)
mainGCM <- lm(WorthQual_2~Treat*GroupCon, data=subset(dataS, Religion==2))
summary(mainGCM)

# Worthy (Hindus)
mainGCH <- lm(WorthQual_2~Treat*GroupCon, data=subset(dataS, Religion==1))
summary(mainGCH)

# Table C19 -------------------------------------- 
stargazer(mainGCM1, mainGCH1, mainGCM, mainGCH,
          dep.var.labels = c("Qualified", "Worthy"),
          title = "\\textbf{The Conditional Effect of the Treatments Given Group Consciousness on Feelings of Being Qualified and Deserving to Lead}",
          omit = "Constant",
          column.labels = c("\\textit{Muslims}", "\\textit{Hindus}", "\\textit{Muslims}", "\\textit{Hindus}"),
          covariate.labels = c("Exclusive", "Inclusive", "Group Consciousness", "Exclusive*Group Consciousness", "Inclusive*Group Consciousness"),
          keep.stat = c("n", "adj.rsq"),
          out = here("Output/Appendix/TableC19.tex")) 

#=======================================================================#


#==============================================================#

# Appendix D - Pre-test and Pilot                              #

#==============================================================#

# Load Pilot Data
load(here("data/DataforPilot.Rdata"))

# Make FIGURE D12 # ------------------------

t1 <- dataPre %>%
  ggplot(aes(x= Excl_Fact)) + 
  geom_bar() +
  ylab("N") + xlab("") +
  ggtitle("Do you think the content in this text is factually correct?", subtitle = "Exclusive Treatment") +
  scale_x_discrete(labels=c("Completely correct", "Mostly Correct", "Completely incorrect")) +
  custom_theme()

t2 <- dataPre %>%
  ggplot(aes(x= Incl_Fact)) + 
  geom_bar() +
  ylab("N") + xlab("") +
  ggtitle("Do you think the content in this text is factually correct?", subtitle = "Inclusive Treatment") +
  scale_x_discrete(labels=c("Completely correct", "Mostly Correct", "Completely incorrect")) +
  custom_theme()

t4 <- dataPre %>%
  count(Incl_comp = as.character(Incl_comp)) %>%
  add_row(Incl_comp = "5", n = 0) %>%
  ggplot(aes(x = Incl_comp, y = n)) + 
  geom_col() +
  ylab("N") +
  xlab("") +
  ggtitle("How difficult was the text to understand?", subtitle = "Inclusive Treatment") +
  scale_x_discrete(labels = c("(1)\nVery easy", "(2)", "(3)", "(4)", "(5)\nVery difficult")) +
  custom_theme()  

all_values <- tibble(Excl_Comp = as.character(c("1", "2", "3", "4", "5")))

t3 <- dataPre %>%
  count(Excl_Comp = as.character(Excl_Comp)) %>%
  right_join(all_values, by = "Excl_Comp") %>%  # Join with all_values to ensure all values are present
  mutate(n = coalesce(n, 0)) %>%  # Replace missing counts with 0
  ggplot(aes(x = Excl_Comp, y = n)) + 
  geom_col() +
  ylab("N") +
  xlab("") +
  ggtitle("How difficult was the text to understand?", subtitle = "Exclusive Treatment") +
  scale_x_discrete(labels = c("(1)\nVery easy", "(2)", "(3)", "(4)", "(5)\nVery difficult")) +
  custom_theme()  # Replace with your custom theme function

# Make figure D12 # -------------------------------

f <- arrangeGrob(t1, t2, t3, t4, ncol=2, nrow=2) 
ggsave(path="Output/Appendix", filename="FigureD12.pdf", f, height=8, width=10) 

# FIGURE D13 # --------------------------------

# Check proportions who guessed that avatars were Hindu (1) or Muslim (2) among pre-test respondents (among those who did not answer "don't know").

# Hindu men
prop.table(table(dataPre$Avatar_Review.1_1[dataPre$Avatar_Review.1_1!=10&dataPre$Avatar_Review.1_1!=11]))
prop.table(table(dataPre$Avatar_Review.1_3[dataPre$Avatar_Review.1_3!=10&dataPre$Avatar_Review.1_3!=11]))
prop.table(table(dataPre$Avatar_Review.1_5[dataPre$Avatar_Review.1_5!=10&dataPre$Avatar_Review.1_5!=11]))

# Hindu women
prop.table(table(dataPre$Avatar_Review.1_7[dataPre$Avatar_Review.1_7!=10&dataPre$Avatar_Review.1_7!=11]))
prop.table(table(dataPre$Avatar_Review.1_8[dataPre$Avatar_Review.1_8!=10&dataPre$Avatar_Review.1_8!=11]))
prop.table(table(dataPre$Avatar_Review.1_9[dataPre$Avatar_Review.1_9!=10&dataPre$Avatar_Review.1_9!=11]))

# Muslim men
prop.table(table(dataPre$Avatar_Review.1_13[dataPre$Avatar_Review.1_13!=10&dataPre$Avatar_Review.1_13!=11]))
prop.table(table(dataPre$Avatar_Review.1_14[dataPre$Avatar_Review.1_14!=10&dataPre$Avatar_Review.1_14!=11]))
prop.table(table(dataPre$Avatar_Review.1_17[dataPre$Avatar_Review.1_17!=10&dataPre$Avatar_Review.1_17!=11]))

# Muslim women
prop.table(table(dataPre$Avatar_Review.1_19[dataPre$Avatar_Review.1_19!=10&dataPre$Avatar_Review.1_19!=11]))

# FIGURE D14  # -------------------------------------

# Effect of treatments on willingness to lead among Muslims
main <- lm(WillingR~Treat, data=subset(dataPil, Religion==2))
summary(main)

# Effect of treatments on willingness to lead among Hindus
mainH <- lm(WillingR~Treat, data=subset(dataPil, Religion==1))
summary(mainH)

# Use "ggmeans" to obtain mean values with confidence intervals 
pred <- ggemmeans(main, terms=c("Treat"))
pred2 <- ggemmeans(mainH, terms=c("Treat"))

# Make dataframe for plotting mean values
pred <- bind_rows(pred, pred2)
pred$Religion <- c(rep("Muslims",3), rep("Hindus",3))

# Make plot showing expected means with confidence intervals (i.e., H3a & H4a) ------------------------------------
pred <- pred %>% 
  mutate(Treatment = recode_treatment(x))

Willingnessmean <- pred %>% 
  ggplot(aes(x= Treatment, y = predicted, group=Religion)) + 
  geom_point(aes(col=Religion, shape=Religion), size=2)+
  geom_line(aes(col=Religion), lty="dashed", alpha=0.7) +
  geom_text_repel(aes(label=round(predicted,2)), vjust=0.5, nudge_x=.15, size=3.5, min.segment.length = Inf) +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high, col=Religion, alpha=0.05)) +
  labs(y="Average Willingness") +
  labs(title="", subtitle = "(A)") +
  ylim(3.3,3.9)  +
  scale_colour_manual(values=c("orange", "darkgreen")) +
  scale_shape_manual(values = c("triangle", "square")) +
  custom_theme() +
  theme(legend.position = "none") 

Willingnessmean

# Estimate effects of treatments on willingness to lead  ----------------------- #

# Overall
mainov <- lm(WillingR~Treat, data=subset(dataPil))
summary(mainov)

# Overall with pre-treatment controls 
mainov2 <- lm(WillingR~Treat+Age+Gender+EducationHigh+State+as.factor(Comp)+GroupCon, data=subset(dataPil))
summary(mainov2)

# Among Muslims
main <- lm(WillingR~Treat, data=subset(dataPil, Religion==2))
summary(main)

# Among Muslims with pre-treatment controls 
main2 <- lm(WillingR~Treat+Age+Gender+EducationHigh+State+as.factor(Comp)+GroupCon, data=subset(dataPil, Religion==2))
summary(main2)

# Among Hindus
mainH <- lm(WillingR~Treat, data=subset(dataPil, Religion==1))
summary(mainH)

# Among Hindus with pre-treatment controls 
mainH2 <- lm(WillingR~Treat+Age+Gender+EducationHigh+State+as.factor(Comp)+GroupCon, data=subset(dataPil, Religion==1))
summary(mainH2)

# Save models for plotting # -------------------------------
models <- list(
  model1 = mainov,
  model2 = mainov2,
  model3 = main,
  model4 = main2,
  model5 = mainH,
  model6 = mainH2)

# Make plot showing effects on willingness to lead (i.e., H3a & H4a) ------------------------------------
df <- models %>% 
  map_dfr(tidy, conf.int = T, conf.level = .95, .id="model") %>%
  rename(conf.low_95 = conf.low, conf.high_95 = conf.high) %>% 
  left_join(map_dfr(models, tidy, conf.int=T, .id = "model", conf.level = .90)) %>% 
  rename(conf.low_90 = conf.low, conf.high_90 = conf.high) %>% 
  rename(Treatment = term,
         Coefficient = estimate,
         SE = std.error) %>%
  filter(Treatment %in% c("Treat2","Treat3")) %>%
  mutate(Treatment = recode_factor(Treatment,
                                   Treat2 = "Exclusive",
                                   Treat3 = "Inclusive"),
         model = recode_factor(model, 
                               model1="No Controls",
                               model2="Controls",
                               model3 ="No Controls",
                               model4 ="Controls",
                               model5="No Controls",
                               model6="Controls"),
         Respondent = c(rep("Overall", 4), rep("Muslims", 4), rep("Hindus", 4)),
         Respondent = fct_relevel(Respondent, "Overall", "Hindus", "Muslims"))


Willingnesscoef <- df %>% filter(model == "No Controls") %>% 
  ggplot(aes(x = Treatment, col = Respondent, y = Coefficient, shape = Respondent)) + 
  geom_hline(yintercept = 0, colour = gray(1/2), lty=1) +
  geom_point(size=2, position = position_dodge(0.4)) +
  geom_point(size=2, position = position_nudge(x=0.175), alpha=0.3, 
             data=filter(df, model == "Controls" & Respondent=="Muslims")) +
  geom_point(size=2, position = position_nudge(x=.04), alpha=0.3, 
             data=filter(df, model == "Controls" & Respondent=="Hindus")) +
  geom_point(size=2, position = position_nudge(x=-.09), alpha=0.3,
             data=filter(df, model == "Controls" & Respondent=="Overall")) +
  geom_linerange(aes(ymin = conf.low_90, ymax = conf.high_90),
                 lwd = 1, position = position_dodge(0.4)) + 
  geom_linerange(aes(ymin = conf.low_90, ymax = conf.high_90),
                 lwd = 1, position = position_nudge(x=0.175), alpha=0.3, 
                 data=filter(df, model == "Controls" & Respondent=="Muslims")) +
  geom_linerange(aes(ymin = conf.low_95, ymax = conf.high_95),
                 lwd = 1, position = position_nudge(x=.04), alpha=0.3, 
                 data=filter(df, model == "Controls" & Respondent=="Hindus")) +
  geom_linerange(aes(ymin = conf.low_95, ymax = conf.high_95),
                 lwd = 1, position = position_nudge(x=-.09), alpha=0.3, 
                 data=filter(df, model == "Controls" & Respondent=="Overall")) +
  geom_linerange(aes(ymin = conf.low_95, ymax = conf.high_95),
                 lwd = 1/2, position = position_dodge(0.4)) +
  geom_linerange(aes(ymin = conf.low_95, ymax = conf.high_95),
                 lwd = 1/2, position = position_nudge(x=0.175), alpha=0.3, 
                 data=filter(df, model == "Controls" & Respondent=="Muslims")) +
  geom_linerange(aes(ymin = conf.low_95, ymax = conf.high_95),
                 lwd = 1/2, position = position_nudge(x=.04), alpha=0.3, 
                 data=filter(df, model == "Controls" & Respondent=="Hindus")) +
  geom_linerange(aes(ymin = conf.low_95, ymax = conf.high_95),
                 lwd = 1/2, position = position_nudge(x=-.09), alpha=0.3, 
                 data=filter(df, model == "Controls" & Respondent=="Overall")) +
  ylim(-0.4, 0.6) +
  labs(title="", subtitle = "(B)") +
  scale_colour_manual(values=c("darkgrey", "orange", "darkgreen")) +
  custom_theme()

Willingnesscoef

# Make figure D14 including plots for averages and coefficients together  # -------------------------------

t <- arrangeGrob(Willingnessmean, Willingnesscoef, ncol=2, nrow=1) 
ggsave(path="Output/Appendix", filename="FigureD14.pdf", t, height=5, width=10) 

# Make Appendix Table 7 for Extra Supplementary Materials # --------------------------------
stargazer(mainov, mainov2, main, main2, mainH, mainH2,
          dep.var.labels = "Willingness to Lead (1-4)" ,
          title = "\\textbf{Treatment Effects on Supply for Leadership (Pilot Data)}",
          column.labels = c("\\textit{Overall}", "\\textit{Overall}", "\\textit{Muslims}", "\\textit{Muslims}", "\\textit{Hindus}", "\\textit{Hindus}"),
          omit = "Constant",
          covariate.labels = c("Exclusive", "Inclusive", "Age", "Female", "Education High", "Uttar Pradesh", "Gujarat", "Maharashtra", "Hindu Majority", "Muslim Majority", "Group Consciousness", "Survey Round"),
          keep.stat = c("n", "adj.rsq"),
          out = here("Output/Appendix/Table7_SuppAppendix.tex"))

# -------- END OF APPENDIX ANALYSIS ---------------------------#


