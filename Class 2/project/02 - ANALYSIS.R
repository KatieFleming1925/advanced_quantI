
###################################
###################################
#####  REPLICATION FILE FOR: ######
###  MY HISTORY OR OUR HISTORY? ###
###################################
###################################

# ========================================================================================== #
# - Script: Analysis for main paper (testing H1-H5) + full regression output in Appendix B
# - Author: Nicholas Haas (nick.haas@ps.au.dk) 
#           Emmy Lindstam (emmy.lindstam@ie.edu)
# ========================================================================================== #

# Clear environment
rm(list = ls())

# Load packages and data for analysis:
source(here::here("Class 2", "project", "00 - PACKAGES.R"))
# load(here("data/DataforAnalysis.Rdata")) # have to adapt
load(here("Class 2/project/DataForAnalysis.Rdata"))

##################################################
# FIGURE 2 - Perceived Centrality (i.e., H1)     #
##################################################

# Muslim Perceived Contributions # -------------------------

# Overall
table(dataS$muslimcontr) # Number of Muslim words chosen 
MCon_Over <- lm(muslimcontr~Treat, data=dataS) # Effect of treatments on perceptions of Muslim contributions among all respondents
summary(MCon_Over)
# Hindus only
MCon_Hin <- lm(muslimcontr~Treat, data=subset(dataS, Religion==1)) # Effect of treatments on perceptions of Muslim contributions among Hindu respondents
summary(MCon_Hin)
# Muslims only
MCon_Mus <- lm(muslimcontr~Treat, data=subset(dataS, Religion==2)) # Effect of treatments on perceptions of Muslim contributions among Muslim respondents
summary(MCon_Mus)

# Hindus  Perceived Contributions # ------------------------

# Overall
table(dataS$hinducontr)
HCon_Over <- lm(hinducontr~Treat, data=dataS) # Effect of treatments on perceptions of Hindu contributions among all respondents
summary(HCon_Over)
# Hindus only
HCon_Hin <- lm(hinducontr~Treat, data=subset(dataS, Religion==1)) # Effect of treatments on perceptions of Hindu contributions among Hindu respondents
summary(HCon_Hin)
# Muslims only
HCon_Mus <- lm(hinducontr~Treat, data=subset(dataS, Religion==2)) # Effect of treatments on perceptions of Hindu contributions among Muslim respondents
summary(HCon_Mus)

# Difference in Contributions (Muslim-Hindu) # ---------------------

# Calculate the difference between perceived Muslim and Hindu contributions (= nr of words)
dataS$ConDiff <- (dataS$muslimcontr - dataS$hinducontr)

# Overall
Over_Diff <- lm(ConDiff~Treat, data=dataS) # Effect of treatments on perceptions of difference in contributions among all respondents
summary(Over_Diff)
# Muslims only
M_Diff <- lm(ConDiff~Treat, data=subset(dataS, Religion==2)) # Effect of treatments on perceptions of difference in contributions among Muslim respondents
summary(M_Diff)
# Hindus only
H_Diff <- lm(ConDiff~Treat, data=subset(dataS, Religion==1)) # Effect of treatments on perceptions of difference in contributions among Hindu respondents
summary(H_Diff)

# Self-perceived prototypicality # --------------------------

#Take average of the two prototypicality measures
dataS$Prot_avg <- rowMeans(dataS[,c('mech1', 'mech2')], na.rm=TRUE)
table(dataS$Prot_avg)

#Overall
Over_Protag <- lm(Prot_avg~Treat, data=dataS) # Effect of treatments on self-perceptions of prototypicality among all respondents
summary(Over_Protag)
# Muslims only
M_protag <- lm(Prot_avg~Treat, data=subset(dataS, Religion==2)) # Effect of treatments on self-perceptions of prototypicality among Muslim respondents
summary(M_protag)
# Hindus only 
H_protag <- lm(Prot_avg~Treat, data=subset(dataS, Religion==1)) # Effect of treatments on self-perceptions of prototypicality among Hindu respondents
summary(H_protag)

# Save models in order to make the plots:
models <- list(
  model1 = Over_Diff,
  model2 = H_Diff,
  model3 = M_Diff,
  model4 = MCon_Over,
  model5 = MCon_Hin,
  model6 = MCon_Mus,
  model7 = HCon_Over,
  model8 = HCon_Hin,
  model9 = HCon_Mus,
  model10 = Over_Protag,
  model11 = H_protag,
  model12 = M_protag)

# Make plot showing perceived centrality to the nation (i.e., FIGURE 2)  ----------------------------- 

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
                               model1 = "(A) Muslim-Hindu \nContributions",
                               model2 = "(A) Muslim-Hindu \nContributions",
                               model3 = "(A) Muslim-Hindu \nContributions",
                               model4 = "(B) Perceived Muslim \nContributions",
                               model5 = "(B) Perceived Muslim \nContributions",
                               model6 = "(B) Perceived Muslim \nContributions",
                               model7 = "(C) Perceived Hindu \nContributions",
                               model8 = "(C) Perceived Hindu \nContributions",
                               model9 = "(C) Perceived Hindu \nContributions",
                               model10 = "(D) Self-assessed \nPrototypicality",
                               model11 = "(D) Self-assessed \nPrototypicality",
                               model12 = "(D) Self-assessed \nPrototypicality"),
         Respondent = c(rep("Overall", 2), rep("Hindus", 2), rep("Muslims", 2), rep("Overall", 2), rep("Hindus", 2), rep("Muslims", 2), 
                        rep("Overall", 2), rep("Hindus", 2), rep("Muslims", 2), rep("Overall", 2), rep("Hindus", 2), rep("Muslims", 2)),
         Respondent = fct_relevel(Respondent, "Overall", "Hindus", "Muslims"),
         y_min = c(rep(-0.8, 12), rep(-0.3, 12)),
         y_max = c(rep(0.8, 24))) %>%
  ggplot(aes(x = Treatment, col = Respondent, y = Coefficient, shape = Respondent)) + 
  geom_hline(yintercept = 0, colour = gray(1/2), lty=1) +
  geom_point(position = position_dodge(0.3), size=2) + # size of shape symbol
  geom_linerange(aes(ymin = conf.low_90, ymax = conf.high_90),
                 lwd = 1, position = position_dodge(0.3)) + 
  geom_linerange(aes(ymin = conf.low_95, ymax = conf.high_95),
                 lwd = 1/2, position = position_dodge(0.3)) +
  facet_wrap(~model, ncol=2, scales = "free_y") +
  geom_blank(aes(y = y_min)) +
  geom_blank(aes(y = y_max)) +
  scale_y_continuous(breaks = seq(-0.3, .6, by = 0.3)) +
  scale_colour_manual(values=c("darkgrey", "orange", "darkgreen"))  +
  custom_theme() +
  theme(legend.position = "bottom")
  

# Save plot (i.e., FIGURE 2) #  --------------------------------------------------
# ggsave(path= "Output/Main", filename = "Figure2.pdf", height=6, width=8) # original script
ggsave(path = here("Class 2", "project", "Output", "Main"), filename = "Figure2.pdf", height = 6, width = 8)

# Appendix Table B.5 with full regression output  -----------------------
# stargazer(Over_Diff, H_Diff, M_Diff, MCon_Over, MCon_Hin, MCon_Mus, HCon_Over, HCon_Hin, HCon_Mus, Over_Protag, H_protag, M_protag,
          # dep.var.labels = c("Muslim-Hindu Contributions", "Muslim Contributions", "Hindu Contributions", "Self-assessed Prototypicality"),
          # title = "\\textbf{Perceived Centrality} \\\\ (Full Regression Output for Figure 2 of the Main Article)",
         #  column.labels = rep(c("\\textit{Overall}", "\\textit{Hindus}", "\\textit{Muslims}"),4),
          # omit = "Constant",
          # covariate.labels = c("Exclusive", "Inclusive"),
          # keep.stat = c("n", "adj.rsq"),
         #  out = here("Output/Appendix/TableB5.tex"), label = "H1full")

stargazer(Over_Diff, H_Diff, M_Diff, MCon_Over, MCon_Hin, MCon_Mus, HCon_Over, HCon_Hin, HCon_Mus, Over_Protag, H_protag, M_protag, type = "text")

stargazer(Over_Diff, type = "text")

##################################################
# FIGURE 3 - Perceived Entitlement (i.e., H2)    #
##################################################

# Muslim MLA perceived as "qualified" # ------------------------------

#Overall 
Over_QualPol <- lm(as.numeric(MLA_Follow_1)~Treat, data=subset(dataS, MLAOrd==0)) # Effect of treatments on perceptions of Muslim MLA being qualified among all respondents
summary(Over_QualPol)

# Among Muslims
M_QualPol <- lm(as.numeric(MLA_Follow_1)~Treat, data=subset(dataS, Religion==2&MLAOrd==0)) # Effect of treatments on perceptions of Muslim MLA being qualified among Muslims
summary(M_QualPol)

# Among Hindus 
H_QualPol <- lm(as.numeric(MLA_Follow_1)~Treat, data=subset(dataS, Religion==1&MLAOrd==0)) # Effect of treatments on perceptions of Muslim MLA being qualified among Hindus
summary(H_QualPol)

# Muslim MLA perceived as "deserving" # ----------------------------------------

# Overall
Over_DesPol <- lm(as.numeric(MLA_Follow_2)~Treat, data=subset(dataS, MLAOrd==0)) # Effect of treatments on perceptions of Muslim MLA deservingness among all respondents
summary(Over_DesPol)

# Among Muslims
M_DesPol <- lm(as.numeric(MLA_Follow_2)~Treat, data=subset(dataS, Religion==2&MLAOrd==0)) # Effect of treatments on perceptions of Muslim MLA deservingness among Muslims
summary(M_DesPol)

# Among Hindus 
H_DesPol <- lm(as.numeric(MLA_Follow_2)~Treat, data=subset(dataS, Religion==1&MLAOrd==0)) # Effect of treatments on perceptions of Muslim MLA deservingness among Hindus
summary(H_DesPol)

# Respondent self-perceived as "qualified" # ---------------------------------------

# Overall 
Over_Qualown<- lm(as.numeric(WorthQual_1)~Treat, data=dataS) # Effect of treatments on self-perceptions of being qualified among all respondents
summary(Over_Qualown)

# Among Muslims
M_Qualown<- lm(as.numeric(WorthQual_1)~Treat, data=subset(dataS, Religion==2)) # Effect of treatments on self-perceptions of being qualified among Muslims
summary(M_Qualown)

# Among Hindus
H_Qualown<- lm(as.numeric(WorthQual_1)~Treat, data=subset(dataS, Religion==1)) # Effect of treatments on self-perceptions of being qualified among Hindus
summary(H_Qualown)

# Respondent self-perceived as deserving # -------------------------------------------

# Overall 
Over_Qualown2 <- lm(as.numeric(WorthQual_2)~Treat, data=dataS) # Effect of treatments on self-perceptions of being deserving among all respondents
summary(Over_Qualown2)

# Among Muslims
M_Qualown2 <- lm(as.numeric(WorthQual_2)~Treat, data=subset(dataS, Religion==2)) # Effect of treatments on self-perceptions of being deserving among Muslims
summary(M_Qualown2)

# Among Hindus
H_Qualown2 <- lm(as.numeric(WorthQual_2)~Treat, data=subset(dataS, Religion==1)) # Effect of treatments on self-perceptions of being qualified among Hindus
summary(H_Qualown2)

# Save models for plotting  ------------------------------- #
models <- list(
  model1 = Over_QualPol,
  model2 = M_QualPol,
  model3 = H_QualPol,
  model4 = Over_DesPol,
  model5 = M_DesPol,
  model6 = H_DesPol,
  model7 = Over_Qualown,
  model8 = M_Qualown,
  model9 = H_Qualown,
  model10 = Over_Qualown2,
  model11 = M_Qualown2,
  model12 = H_Qualown2)

# Make plot (i.e., FIGURE 3) ------------------------------------ 

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
                               model1 = "(A) Muslim MLA: \nQualified",
                               model2 = "(A) Muslim MLA: \nQualified",
                               model3 =  "(A) Muslim MLA: \nQualified",
                               model4 = "(B) Muslim MLA: \nDeserving",
                               model5 = "(B) Muslim MLA: \nDeserving",
                               model6 =  "(B) Muslim MLA: \nDeserving",
                               model7 =  "(C) Self: \nQualified",
                               model8 = "(C) Self: \nQualified",
                               model9 = "(C) Self: \nQualified",
                               model10 = "(D) Self: \nDeserving",
                               model11 = "(D) Self: \nDeserving",
                               model12 =  "(D) Self: \nDeserving"),
         Respondent = c(rep("Overall", 2), rep("Muslims", 2), rep("Hindus", 2), rep("Overall", 2), rep("Muslims", 2), rep("Hindus", 2), 
                        rep("Overall", 2), rep("Muslims", 2), rep("Hindus", 2), rep("Overall", 2), rep("Muslims", 2), rep("Hindus", 2)),
         Respondent = fct_relevel(Respondent, "Overall", "Hindus", "Muslims"),
         y_min = c(rep(-0.8, 12), rep(-0.3, 12)),
         y_max = c(rep(0.8, 24))) %>%
  ggplot(aes(x = Treatment, col = Respondent, y = Coefficient, shape = Respondent)) + 
  geom_hline(yintercept = 0, colour = gray(1/2), lty=1) +
  geom_point(position = position_dodge(0.3), size=2) +
  geom_linerange(aes(ymin = conf.low_90, ymax = conf.high_90),
                 lwd = 1, position = position_dodge(0.3)) + 
  geom_linerange(aes(ymin = conf.low_95, ymax = conf.high_95),
                 lwd = 1/2, position = position_dodge(0.3)) +
  facet_wrap(~model, ncol=2) +
  scale_y_continuous(breaks = seq(-0.3, .3, by = 0.3)) +
  scale_colour_manual(values=c("darkgrey", "orange", "darkgreen")) +
  custom_theme() +
  theme(legend.position = "bottom")
  
# Save plot (i.e., FIGURE 3) #  --------------------------------------------------
ggsave(path= "Output/Main", filename = "Figure3.pdf", height=6, width=8)


# Appendix Table B.6 with full regression output -----------------------
stargazer(Over_QualPol, M_QualPol, H_QualPol, Over_DesPol, M_DesPol, H_DesPol, Over_Qualown, M_Qualown, H_Qualown, Over_Qualown2, M_Qualown2, H_Qualown2,
          dep.var.labels = c("Muslim MLA: Qualified", "Muslim MLA: Deserving", "Self: Qualified", "Self: Deserving"),
          title = "\\textbf{Perceived Entitlement} \\\\ (Full Regression Output for Figure 3 of the Main Article)",
          column.labels = rep(c("\\textit{Overall}", "\\textit{Hindus}", "\\textit{Muslims}"),4),
          omit = "Constant",
          covariate.labels = c("Exclusive", "Inclusive"),
          keep.stat = c("n", "adj.rsq"),
          out = here("Output/Appendix/TableB6.tex"), label = "H2full")


#########################################################
# FIGURE 4 - Willingness to Lead (i.e., H3a & H4a)      #
#########################################################

# Effect of treatments on willingness to lead among Muslims
main <- lm(WillingR~Treat, data=subset(dataS, Religion==2))
summary(main)

# Effect of treatments on willingness to lead among Hindus
mainH <- lm(WillingR~Treat, data=subset(dataS, Religion==1))
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
  ylim(3.5,3.9)  +
  scale_colour_manual(values=c("orange", "darkgreen")) +
  scale_shape_manual(values = c("triangle", "square")) +
  custom_theme() +
  theme(legend.position = "none") 

Willingnessmean

# Estimate effects of treatments on willingness to lead  ----------------------- 

# Overall
mainov <- lm(WillingR~Treat, data=dataS)
summary(mainov)

# Overall with pre-treatment controls 
mainov2 <- lm(WillingR~Treat+Age+Gender+EducationHigh+State+as.factor(Comp)+GroupCon+Round, data=dataS)
summary(mainov2)

# Among Muslims
main <- lm(WillingR~Treat, data=subset(dataS, Religion==2))
summary(main)

# Among Muslims with pre-treatment controls 
main2 <- lm(WillingR~Treat+Age+Gender+EducationHigh+State+as.factor(Comp)+GroupCon+Round, data=subset(dataS, Religion==2))
summary(main2)

# Among Hindus
mainH <- lm(WillingR~Treat, data=subset(dataS, Religion==1))
summary(mainH)

# Among Hindus with pre-treatment controls 
mainH2 <- lm(WillingR~Treat+Age+Gender+EducationHigh+State+as.factor(Comp)+GroupCon+Round, data=subset(dataS, Religion==1))
summary(mainH2)

# Save models for plotting  ------------------------------- #
models <- list(
  model1 = mainov,
  model2 = mainov2,
  model3 = main,
  model4 = main2,
  model5 = mainH,
  model6 = mainH2)

# Make plot (Figure 4) showing effects on willingness to lead (i.e., H3a & H4a) ------------------------------------ #

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
  geom_point(size=2, position = position_nudge(x=-.09), alpha=0.3, ,
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
  ylim(-0.2, 0.35) +
  labs(title="", subtitle = "(B)") +
  scale_colour_manual(values=c("darkgrey", "orange", "darkgreen")) +
  custom_theme()
 
Willingnesscoef
 
# Make figure 4 including plots for averages and coefficients together (i.e., H3a & H4a) # -------------------------------

e <- arrangeGrob(Willingnessmean, Willingnesscoef, ncol=2, nrow=1) 
ggsave(path="Output/Main", filename="Figure4.pdf", e, height=5, width=10) 

# Make Appendix Table B.2 with full regression output -----------------------
stargazer(mainov, mainov2, main, main2, mainH, mainH2,
          dep.var.labels = "Willingness to Lead (1-4)" ,
          title = "\\textbf{Treatment Effects on Supply for Leadership} \\\\ (Full Regression Output for Figure 4 of the Main Article)",
          column.labels = c("\\textit{Overall}", "\\textit{Overall}", "\\textit{Muslims}", "\\textit{Muslims}", "\\textit{Hindus}", "\\textit{Hindus}"),
          omit = "Constant",
          covariate.labels = c("Exclusive", "Inclusive", "Age", "Female", "Education High", "Uttar Pradesh", "Gujarat", "Maharashtra", "Hindu Majority", "Muslim Majority", "Group Consciousness", "Survey Round"),
          keep.stat = c("n", "adj.rsq"),
          out = here("Output/Appendix/TableB2.tex"), label = "H34afull")

###############################################################
# FIGURE 5 - Demand for Muslim Leadership (i.e., H3B & H4B)   #
###############################################################

# Look at proportion Hindus who ranked Hindu/Muslim partners as first, second & third.
prop.table(table(dataS$DemandHH)) # Ranked Hindu 3rd: 0.37, 2nd: 0.50, 1st: 0.12
prop.table(table(dataS$DemandHM)) # Ranked Muslim 3rd: 0.53, 2nd: 0.35, 1st: 0.11

# Show general difference in demand for Hindu vs Muslim leaders among Hindus in Hindu majority groups. Make dataframe for plot:
df <- data.frame(Rank=factor(rep(c("First", "Second", "Last"), 2), levels = c("First", "Second", "Last")),
                 Proportion=c(0.12, 0.51, 0.37, 0.11, 0.35, 0.53),
                 Religion=rep(c("Hindu Partner", "Muslim Partner"), each = 3))

# Plot this:
rank <- ggplot(data=df, aes(x=Rank, y=Proportion, fill=Religion)) +
  geom_bar(stat="identity", alpha=0.7,position=position_dodge()) +
  geom_text(aes(label=round(Proportion,2)),size=3.5, vjust = -.5, position=position_dodge(width = 1)) +
  scale_fill_manual(values=c("orange","darkgreen")) + 
  ylim(0, 0.6) +
  ylab("Share") +
  labs(title="", subtitle = "(A)") +
  custom_theme() 
  
rank

# Effect of Treatment on demand for Muslim leadership # --------------------

# Make variable for pro-Hindu bias (ranges from -2 to +2)
dataS$Demandif <- dataS$DemandHH - dataS$DemandHM
table(dataS$Demandif)

# Estimate effect of treatments on pro-Hindu bias - positive effect means preference for Hindu over Muslim.
diff <- lm(as.numeric(Demandif)~Treat, data=dataS)
summary(diff)

# Use "ggmeans" to obtain mean values with confidence intervals
pred <- ggemmeans(diff, terms=c("Treat"))

# Make plot showing expected averages + confidence intervals (i.e., testing H3b & H4b)
pred <- pred %>% 
  mutate(Treatment = recode_treatment(x))

dif <- pred %>% 
  ggplot(aes(x= Treatment, y = predicted, group=1)) + 
  geom_point(col="darkorange", size=2) +
  geom_line(col="darkorange", lty="dashed", alpha=0.7) +
  geom_text_repel(aes(label=round(predicted,2)), vjust=0.5, nudge_x=.15, size=3.5, min.segment.length = Inf) +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high, alpha=0.7), col="darkorange") +
  labs(y="Hindu Bias (-2 to 2)") +
  ylim(-0.4,0.55)  +
  labs(title="", subtitle = "(B)") +
  custom_theme() +
  guides(alpha = "none") 

dif

# Make figure 5 including plots for proportions and averages together (i.e., H3b & H4b)  ------------------------------- 

f <- arrangeGrob(rank, dif, ncol=2, nrow=1) 
ggsave(path="Output/Main", filename="Figure5.pdf", f, height=5, width=10) 

# Appendix Table B.3 with full regression output # -------------------
stargazer(diff,
          dep.var.labels = "Pro-Hindu Bias (-2 to 2)" ,
          title = "\\textbf{Pro-Hindu Bias among Hindu Respondents in Hindu Majority Group} \\\\ (Full Regression Output for Figure 5 of the Main Article)",
          omit = "Constant",
          covariate.labels = c("Exclusive", "Inclusive"),
          keep.stat = c("n", "adj.rsq"),
          out = here("Output/Appendix/TableB3.tex"), label = "H34bfull")

############################################################
# FIGURE 6 - Group Composition (i.e., H5)                  #
############################################################

# Estimate the interaction between treatments and group composition among Muslim respondents # ----------------

# Estimating the conditional effects for homogeneous and religiously mixed groups:
mcomp <- lm(WillingR~Treat*as.factor(Comp2), data=subset(dataS, Religion==2))
summary(mcomp)

# Estimating the conditional effects for homogeneous, Hindu majority and Muslim majority groups:
mcomp2 <- lm(WillingR~Treat*as.factor(Comp), data=subset(dataS, Religion==2))
summary(mcomp2)

# Make plot showing conditional treatment effects (i.e., H5) ------------------------------------

# Plot it 
models <- list(
  model1 = mcomp,
  model2 = mcomp2)

# Make the plot 
plotI <- models %>% 
  map_dfr(tidy, conf.int = T, conf.level = .95, .id="model") %>% 
  rename(conf.low_95 = conf.low, conf.high_95 = conf.high) %>% 
  left_join(map_dfr(models, tidy, conf.int=T, .id = "model", conf.level = .90)) %>% 
  rename(conf.low_90 = conf.low, conf.high_90 = conf.high) %>% 
  filter(term !="(Intercept)") %>% 
  mutate(model = recode_factor(model,
                               model1 = "Binary",
                               model2 = "Full Distribution"),
         Variable = recode_factor(term,
                                  `Treat3:as.factor(Comp)2` = "Inclusive*Muslim Majority",
                                  `Treat2:as.factor(Comp)2` = "Exclusive*Muslim Majority",
                                  `Treat3:as.factor(Comp)1` = "Inclusive*Hindu Majority",
                                  `Treat2:as.factor(Comp)1` = "Exclusive*Hindu Majority",
                                  `as.factor(Comp)2` = "Muslim Majority",
                                  `as.factor(Comp)1` = "Hindu Majority",
                                  `Treat3:as.factor(Comp2)1` = "Inclusive*Mixed",
                                  `Treat2:as.factor(Comp2)1` = "Exclusive*Mixed",
                                  `as.factor(Comp2)1` = "Mixed Group",
                                  Treat3 = "Inclusive", 
                                  Treat2 = "Exclusive")) %>% 
  ggplot(aes(x = Variable, y = estimate, shape = model, col="darkseagreen")) +
  geom_hline(yintercept = 0, col = "grey") +
  geom_point(position = position_dodge(0.3)) +
  geom_linerange(aes(ymin = conf.low_90, ymax = conf.high_90),
                 lwd = 1, position = position_dodge(0.3)) + 
  geom_linerange(aes(ymin = conf.low_95, ymax = conf.high_95),
                 lwd = 1/2, position = position_dodge(0.3)) +
  labs(y = "Coefficient", x = NULL) +
  labs(title="", subtitle = "(A)") +
  ylim(-0.80, 0.88) +
  scale_colour_manual(values=c("darkgreen")) +
  coord_flip() +
  facet_wrap(~model) +
  custom_theme() +
  theme(legend.position="none")

plotI

# Make plot showing simulated sampling distributions to visualize effect sizes ------------------------------------

# Extract coefficients
coef <- coef(mcomp)
coef <- as.matrix(coef)
dim(coef)

# Extract variance-covariance matrix
vcov <- vcov(mcomp)
dim(vcov)

# Draw 1000 simulated coefficients
set.seed(7)
S <- mvrnorm(1000, coef, vcov)
dim(S)

# Different scenarios:
scenario1 <- cbind(1,0,0,0,0,0) # Control (homogeneous)
scenario2 <- cbind(1,0,1,0,0,0) # Inclusive (homogeneous)
scenario3 <- cbind(1,0,0,1,0,0) # Control (mixed)
scenario4 <- cbind(1,0,1,1,0,1) # Inclusive (mixed)

# Expected values
props1 <- S%*%t(scenario1)
props2 <- S%*%t(scenario2)
props3 <- S%*%t(scenario3)
props4 <- S%*%t(scenario4)

# Means and confidence intervals 
p.mean1 <- apply(props1,2,mean)
p.mean2 <- apply(props2,2,mean)
p.mean3 <- apply(props3,2,mean)
p.mean4 <- apply(props4,2,mean)

p.qu1 <- t(apply(props1,2,quantile,prob=c(0.025,0.975)))
p.qu2 <- t(apply(props2,2,quantile,prob=c(0.025,0.975)))
p.qu3 <- t(apply(props3,2,quantile,prob=c(0.025,0.975)))
p.qu4 <- t(apply(props4,2,quantile,prob=c(0.025,0.975)))

# Plot expected values
exp.values1 <- c(props1, props2, props3, props4)
df1 <- data.frame(exp.values1)
dim(df1)

# Dataframe for plotting
condition <- rep(1:2, times=1, each=2000)
table(condition)
df1 <- cbind(df1, condition)
head(df1)
df1$condition[df1$condition==1] <- "Homogeneous"
df1$condition[df1$condition==2] <- "Mixed"
df1$id <- c(rep("Control", 1000),  rep("Inclusive", 1000), rep("Control", 1000),  rep("Inclusive", 1000))

df1 <- df1 %>%
  mutate(condition = factor(condition, levels = c("Homogeneous", "Mixed"))) 

# Plot this: 

plotM <- ggplot(df1,aes(x=exp.values1, fill=id)) +
  geom_density(alpha=0.4) +     
  guides(fill=guide_legend(title="")) + scale_fill_manual(values=c("snow2", "darkgreen")) +                                  
  ylab("Density") + 
  xlab("Willingness to Lead (1-4)") + xlim(3.3,4.2) + ylim(0,10) +
  facet_wrap(~condition) +  
  labs(title="", subtitle = "(B)") +
  custom_theme() +
  theme(legend.position = "bottom")

plotM

# Make figure 6 including both plots together (i.e., H5) # -------------------------------
a <- arrangeGrob(plotI, plotM, ncol=2, nrow=1)
ggsave(path="Output/Main", filename="Figure6.pdf", a, height=5, width=10) 

# Appendix Table B.4 with full regression output # ---------------------------
stargazer(mcomp, mcomp2,
          dep.var.labels = "Willingness to Lead (1-4)" ,
          title = "\\textbf{Group Composition and Willingness to Lead among Muslims} \\\\ (Full Regression Output for Figure 6 of the Main Article)",
          omit = "Constant",
          covariate.labels = c("Exclusive", "Inclusive", "Mixed Groups", "Exclusive*Mixed", "Inclusive*Mixed", "Hindu Majority", "Muslim Majority", "Exclusive*Hindu Majority", "Inclusive*Hindu Majority", "Exclusive*Muslim Majority", "Inclusive*Muslim Majority"),
          keep.stat = c("n", "adj.rsq"),
          out = here("Output/Appendix/TableB4.tex"), label = "H5full") 

############################################################
# FIGURE 7 - Seeking Information (i.e., H3A & H4A)         #
############################################################

# Interest in receiving information on how to become involved ----------------------------- 

# Overall 
Overinfo <- lm(Beh_Outcome~Treat, data=dataS)
summary(Overinfo)

# Overall including pre-treatment controls 
Overinfoc <- lm(Beh_Outcome~Treat+Gender+State+Age+EducationHigh+as.factor(Comp), data=dataS)
summary(Overinfoc)

# Among Muslims
minfo <- lm(Beh_Outcome~Treat, data=subset(dataS, Religion==2))
summary(minfo)

# Among Muslims including pre-treatment controls 
minfoc <- lm(Beh_Outcome~Treat+Gender+State+Age+EducationHigh+as.factor(Comp), data=subset(dataS, Religion==2))
summary(minfoc)

# Among Hindus
minfoH <- lm(Beh_Outcome~Treat, data=subset(dataS, Religion==1))
summary(minfoH)

# Among Hindus including pre-treatment controls 
minfoHc <- lm(Beh_Outcome~Treat+Gender+State+Age+EducationHigh+as.factor(Comp), data=subset(dataS, Religion==1))
summary(minfoHc)

# Save models in order to make the plot 
models <- list(
  model1 = Overinfo,
  model2 = Overinfoc,
  model3 = minfo,
  model4 = minfoc,
  model5 = minfoH,
  model6 = minfoHc)

# Make plot showing treatment effects on interest in receiving information -------------------

df3 <- models %>% 
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
                               model1 = "No Controls",
                               model2 =  "Controls",
                               model3 = "No Controls",
                               model4 = "Controls",
                               model5 = "No Controls",
                               model6 = "Controls"),
         Respondent = c(rep("Overall", 4), rep("Muslims", 4), rep("Hindus", 4)),
         Respondent = fct_relevel(Respondent, "Overall", "Hindus", "Muslims"))

polinf <- df3 %>% filter(model == "No Controls") %>% 
  ggplot(aes(x = Treatment, col = Respondent, y = Coefficient, shape = Respondent)) + 
  geom_hline(yintercept = 0, colour = gray(1/2), lty=1) +
  geom_point(size =2, position = position_dodge(0.4)) +
  geom_point(size=2, position = position_nudge(x=0.175), alpha=0.3, 
             data=filter(df3, model == "Controls" & Respondent=="Muslims")) +
  geom_point(size=2, position = position_nudge(x=.04), alpha=0.3, 
             data=filter(df3, model == "Controls" & Respondent=="Hindus")) +
  geom_point(size=2, position = position_nudge(x=-.09), alpha=0.3, 
             data=filter(df3, model == "Controls" & Respondent=="Overall")) +
  geom_linerange(aes(ymin = conf.low_90, ymax = conf.high_90),
                 lwd = 1, position = position_dodge(0.4)) + 
  geom_linerange(aes(ymin = conf.low_90, ymax = conf.high_90),
                 lwd = 1, position = position_nudge(x=0.175), alpha=0.3, 
                 data=filter(df3, model == "Controls" & Respondent=="Muslims")) +
  geom_linerange(aes(ymin = conf.low_95, ymax = conf.high_95),
                 lwd = 1, position = position_nudge(x=.04), alpha=0.3, 
                 data=filter(df3, model == "Controls" & Respondent=="Hindus")) +
  geom_linerange(aes(ymin = conf.low_95, ymax = conf.high_95),
                 lwd = 1, position = position_nudge(x=-.09), alpha=0.3, 
                 data=filter(df3, model == "Controls" & Respondent=="Overall")) +
  geom_linerange(aes(ymin = conf.low_95, ymax = conf.high_95),
                 lwd = 1/2, position = position_dodge(0.4)) +
  geom_linerange(aes(ymin = conf.low_95, ymax = conf.high_95),
                 lwd = 1/2, position = position_nudge(x=0.175), alpha=0.3, 
                 data=filter(df3, model == "Controls" & Respondent=="Muslims")) +
  geom_linerange(aes(ymin = conf.low_95, ymax = conf.high_95),
                 lwd = 1/2, position = position_nudge(x=.04), alpha=0.3, 
                 data=filter(df3, model == "Controls" & Respondent=="Hindus")) +
  geom_linerange(aes(ymin = conf.low_95, ymax = conf.high_95),
                 lwd = 1/2, position = position_nudge(x=-.09), alpha=0.3, 
                 data=filter(df3, model == "Controls" & Respondent=="Overall")) +
  custom_theme() +
  ylim(-0.15, 0.15) +
  labs(title="", subtitle = "(B)") +
  scale_colour_manual(values=c("darkgrey", "orange", "darkgreen"))  

polinf  

# Use "ggmeans" to obtain mean values with confidence intervals  
pred <- ggemmeans(minfo, terms=c("Treat"))
pred2 <- ggemmeans(minfoH, terms=c("Treat"))

# Make dataframe for plotting mean values
pred <- bind_rows(pred, pred2)
pred$Religion <- c(rep("Muslims",3), rep("Hindus",3))

# Make plot showing expected means with confidence intervals  ------------------------------------ 
pred <- pred %>% 
  mutate(Treatment = recode_treatment(x)) 

polinf2 <- pred %>%
  ggplot(aes(x= Treatment, y = predicted, group=Religion)) + 
  geom_point(aes(col=Religion, shape=Religion), size=2)+
  geom_line(aes(col=Religion),lty="dashed", alpha=0.7) +
  geom_text(aes(label=round(predicted,2)), vjust=-0.5, nudge_x=.13, size=3.5) +
  geom_linerange(aes(ymin = conf.low, ymax = conf.high, col=Religion, alpha=0.05)) +
  labs(y="Average Willingness") +
  labs(title="", subtitle = "(A)") +  
  ylim(0.8,1)  +
  scale_colour_manual(values=c("orange", "darkgreen")) +
  scale_shape_manual(values = c("triangle", "square")) +
  custom_theme() +
  theme(legend.position = "none") 

polinf2

# Make figure 7 including plots for averages and coefficients together ------------------------------- #
j <- arrangeGrob(polinf2, polinf, ncol=2, nrow=1) 
ggsave(path="Output/Main", filename="Figure7.pdf", j, height=5, width=10) 

# Make Appendix Table B.7 with full regression output -------------------------
stargazer(Overinfo, Overinfoc, minfo, minfoc, minfoH, minfoHc,
          dep.var.labels = "Willingness to Seek Out Information" ,
          title = "\\textbf{Treatment Effects on Willingness to Seek Out Information} \\\\ (Full Regression Output for Figure 7 of the Main Article)",
          column.labels = c("\\textit{Overall}", "\\textit{Overall}", "\\textit{Muslims}", "\\textit{Muslims}", "\\textit{Hindus}", "\\textit{Hindus}"),
          omit = "Constant",
          covariate.labels = c("Exclusive", "Inclusive", "Female", "Uttar Pradesh", "Gujarat", "Maharashtra", "Age", "Education High", "Hindu Majority", "Muslim Majority"),
          keep.stat = c("n", "adj.rsq"),
          out = here("Output/Appendix/TableB7.tex"), label = "H34Afull2")

############################################################
# FIGURE 8 - Candidate Evaluations n (i.e., H3B & H4B)     #
############################################################

# Treatment effects on candidate evaluations --------------------------------- 

table(dataS$MLAOrd)

# Overall for Muslim candidate
Ocandid <- lm(Candidate~Treat, data=subset(dataS, MLAOrd==0))
summary(Ocandid)

# Overall for Muslim candidate with pre-treatment control,s 
Ocandidc <- lm(Candidate~Treat+Gender+Age+EducationHigh+as.factor(Comp), data=subset(dataS, MLAOrd==0))
summary(Ocandidc)

# Overall for Hindu candidate
Ocandid2 <- lm(Candidate~Treat, data=subset(dataS, MLAOrd==1))
summary(Ocandid2)

# Overall for Hindu candidate with pre-treatment controls
Ocandid2c <- lm(Candidate~Treat+Gender+Age+EducationHigh+as.factor(Comp), data=subset(dataS, MLAOrd==1))
summary(Ocandid2c)

# Muslims for Muslim candidate
mcandid <- lm(Candidate~Treat, data=subset(dataS, Religion==2&MLAOrd==0))
summary(mcandid)

# Muslims for Muslim candidate with pre-treatment controls 
mcandidc <- lm(Candidate~Treat+Gender+Age+EducationHigh+as.factor(Comp), data=subset(dataS, Religion==2&MLAOrd==0))
summary(mcandidc)

# Muslims for Hindu candidate
mcandid2 <- lm(Candidate~Treat, data=subset(dataS, Religion==2&MLAOrd==1))
summary(mcandid2)

# Muslims for Hindu candidate with pre-treatment controls 
mcandid2c <- lm(Candidate~Treat+Gender+Age+EducationHigh+as.factor(Comp), data=subset(dataS, Religion==2&MLAOrd==1))
summary(mcandid2c)

# Hindus for Muslim candidate
mcandidH <- lm(Candidate~Treat, data=subset(dataS, Religion==1&MLAOrd==0))
summary(mcandidH)

# Hindus for Muslim candidate with pre-treatment controls 
mcandidHc <- lm(Candidate~Treat+Gender+Age+EducationHigh+as.factor(Comp), data=subset(dataS, Religion==1&MLAOrd==0))
summary(mcandidHc)

# Hindus for Hindu candidate
mcandidH2 <- lm(Candidate~Treat, data=subset(dataS, Religion==1&MLAOrd==1))
summary(mcandidH2)

# Hindus for Hindu candidate with pre-treatment controls
mcandidH2c <- lm(Candidate~Treat+Gender+Age+EducationHigh+as.factor(Comp), data=subset(dataS, Religion==1&MLAOrd==1))
summary(mcandidH2c)

# Plot overall evaluation of Muslim candidate
models <- list(
  model1=Ocandid,
  model2=Ocandidc,
  model3=Ocandid2,
  model4=Ocandid2c,
  model5 = mcandid,
  model6 = mcandidc,
  model7 = mcandid2,
  model8 = mcandid2c,
  model9 = mcandidH,
  model10 = mcandidHc,
  model11 = mcandidH2,
  model12 = mcandidH2c)

# Make GG plot
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
                               model1 = "No Controls",
                               model2 =  "Controls",
                               model3 =  "No Controls",
                               model4 = "Controls",
                               model5 = "No Controls",
                               model6 =  "Controls",
                               model7 =  "No Controls",
                               model8 = "Controls",
                               model9 = "No Controls",
                               model10 =  "Controls",
                               model11 =  "No Controls",
                               model12 = "Controls"),
         Respondent = c(rep("Overall", 8), rep("Muslims", 8), rep("Hindus", 8)),
         Respondent = fct_relevel(Respondent, "Overall", "Hindus", "Muslims"),
         MLA = c(rep("(B) Muslim MLA",4), rep("(A) Hindu MLA", 4), rep("(B) Muslim MLA",4), rep("(A) Hindu MLA", 4), rep("(B) Muslim MLA",4), rep("(A) Hindu MLA", 4))) 

df %>% filter(model == "No Controls") %>% 
  ggplot(aes(x = Treatment, col = Respondent, y = Coefficient, shape = Respondent)) + 
  geom_hline(yintercept = 0, colour = gray(1/2), lty=1) +
  geom_point(size=2, position = position_dodge(0.4)) +
  geom_point(size=2,position = position_nudge(x=0.175), alpha=0.3, 
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
  facet_grid(MLA~.) +
  ylim(-0.35, 0.3) +
  scale_colour_manual(values=c( "darkgrey", "orange", "darkgreen")) +
  custom_theme() +
  theme(legend.position = "bottom") 

# Make figure 8  -------------------------------------------- 
ggsave(path="Output/Main", filename="Figure8.pdf", height=4.5, width=6) 


# Appendix Table B.8 with full regression output # ---------------------------
stargazer(Ocandid, Ocandidc, Ocandid2, Ocandid2c, mcandid, mcandidc, mcandid2, mcandid2c, mcandidH, mcandidHc, mcandidH2, mcandidH2c,
          dep.var.labels = "Politician Evaluation",
          title = "\\textbf{Treatment Effects on Politican Evaluation} \\\\ (Full Regression Output for Figure 8 of the Main Article)",
          column.labels = c("Muslim MLA", "Muslim MLA", "Hindu MLA", "Hindu MLA", "Muslim MLA", "Muslim MLA", "Hindu MLA", "Hindu MLA", "Muslim MLA", "Muslim MLA", "Hindu MLA", "Hindu MLA"),
          omit = "Constant",
          covariate.labels = c("Exclusive", "Inclusive", "Female", "Age", "Education High", "Hindu Majority", "Muslim Majority"),
          keep.stat = c("n", "adj.rsq"),
          out = here("Output/Appendix/TableB8.tex"), label = "H3Bfull2")

# Manually add another lair of column labels in latex: column.labels = c("\\textit{Overall}", "\\textit{Overall}", "\\textit{Overall}", "\\textit{Overall}", "\\textit{Muslims}", "\\textit{Muslims}", "\\textit{Muslims}", "\\textit{Muslims}", "\\textit{Hindus}", "\\textit{Hindus}", "\\textit{Hindus}", "\\textit{Hindus}"),


# End Analysis # -------------------------------------------------------


