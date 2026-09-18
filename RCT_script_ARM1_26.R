####################################################################
##  Advanced Research Methods I (Causal Inference) -- Week 2 lab
##  Replication: Olken, Benjamin (2007), "Monitoring Corruption:
##  Evidence from a Field Experiment in Indonesia", JPE 115(2).
##
##  Outcome:   pct_missing    -- share of expenditure unaccounted for
##  Treatment: treat_invite   -- invitation to accountability meetings
##  Covariates: head_edu, mosques, pct_poor, total_budget
####################################################################


## ---- 0. Packages -------------------------------------------------
## Run these ONCE, then leave them commented out.

# install.packages(c("sandwich", "lmtest", "effectsize", "pwr", "ri2"))

library(sandwich)
library(lmtest)
library(effectsize)
library(pwr)
library(ri2)


## ---- 1. Data -----------------------------------------------------
## Put olken.csv in the same folder as this script, then use
## Session > Set Working Directory > To Source File Location.
## (Avoid hard-coded paths -- they break on everyone else's machine.)

olken <- read.csv("olken.csv")

names(olken)
str(olken)
table(olken$treat_invite)


## ---- 2. Balance tests --------------------------------------------
## Regress treatment on each baseline covariate. Under successful
## randomisation these coefficients should be indistinguishable
## from zero.

summary(lm(treat_invite ~ head_edu,     data = olken))
summary(lm(treat_invite ~ mosques,      data = olken))
summary(lm(treat_invite ~ pct_poor,     data = olken))
summary(lm(treat_invite ~ total_budget, data = olken))

## QUESTION: one of four tests significant at 5% -- is that evidence
## that randomisation failed?

## A joint test is the better tool, because it asks whether the
## covariates TOGETHER predict assignment:

balance_joint <- lm(treat_invite ~ head_edu + mosques + pct_poor +
                      total_budget, data = olken)
summary(balance_joint)   # look at the F-statistic and its p-value


## ---- 3. The estimate ---------------------------------------------

## (1) bivariate
model1 <- lm(pct_missing ~ treat_invite, data = olken)
summary(model1)

## (2) with covariates
model2 <- lm(pct_missing ~ treat_invite + head_edu + mosques +
               pct_poor + total_budget, data = olken)
summary(model2)

## (3) with heteroskedasticity-robust standard errors
coeftest(model2, vcov = vcovHC(model2, type = "HC0"))

## QUESTION: why add covariates to a randomised experiment at all?
## (Not to remove bias -- to reduce residual variance and tighten
## the standard errors. If the coefficient MOVES a lot, that is a
## warning about the randomisation, not a better estimate.)


## ---- 4. Attrition -------------------------------------------------
## Drop villages with no recorded outcome. Note: use is.na(), not
## a comparison with the string "na".

olken_full <- subset(olken, !is.na(pct_missing))

## group sizes before and after
table(olken$treat_invite)
table(olken_full$treat_invite)

## distributions in the two arms
boxplot(pct_missing ~ treat_invite, data = olken_full,
        xlab = "Invited to accountability meetings",
        ylab = "Share of expenditure missing")

## QUESTION: if villages were more likely to go missing in the
## treatment arm, what has gone wrong -- and what did we call it
## last week?


## ---- 5. Is the null informative? ----------------------------------
## The estimate is close to zero. Before concluding "no effect",
## work out what effect this study COULD have detected.

treated   <- olken_full$pct_missing[olken_full$treat_invite == 1]
untreated <- olken_full$pct_missing[olken_full$treat_invite == 0]

m1 <- mean(untreated)
m2 <- mean(treated)
diff <- m2 - m1
diff

## pooled standard deviation, by hand ...
n1 <- length(untreated); n2 <- length(treated)
s1 <- sd(untreated);     s2 <- sd(treated)
pooled <- sqrt(((n1 - 1) * s1^2 + (n2 - 1) * s2^2) / (n1 + n2 - 2))
pooled

## ... and with a package (note: do not name the object sd_pooled,
## that would mask the function)
pooled_check <- sd_pooled(untreated, treated)
pooled_check

## standardised effect size actually observed
d_observed <- diff / pooled
d_observed          # very small

## What power did the study have against a HALF standard deviation
## effect? Groups are unequal, so specify both sizes.
pwr.t2n.test(n1 = n1, n2 = n2, d = 0.5)

## Try it for a small effect too:
pwr.t2n.test(n1 = n1, n2 = n2, d = 0.1)

pwr.t2n.test(n1 = n1, n2 = n2, d = 0.2) ## out of curiosity in class

pwr.t2n.test(n1 = n1, n2 = n2, d = 0.25)

pwr.t2n.test(n1 = n1, n2 = n2, d = 0.3)


## And in the other direction: how large would EACH group need to be
## to detect d = 0.1 with 80% power? (n is what we solve for, so it
## is left as NULL.)
pwr.t.test(n = NULL, d = 0.1, power = 0.8,
           type = "two.sample", sig.level = 0.05)

## Vary d, power and sig.level. How does the required n move?


## ---- 6. Randomisation inference -----------------------------------
## Rather than assuming a sampling distribution, rebuild the null
## directly: reassign treatment at random many times and see where
## the real estimate falls.

n_total   <- nrow(olken_full)
n_treated <- sum(olken_full$treat_invite == 1)
n_total; n_treated

declaration <- declare_ra(N = n_total, m = n_treated)

ri_data <- data.frame(Z = olken_full$treat_invite,
                      Y = olken_full$pct_missing)

set.seed(43)

rcheck <- conduct_ri(
  formula          = Y ~ Z,
  declaration      = declaration,
  sharp_hypothesis = 0,
  data             = ri_data
)

summary(rcheck)
plot(rcheck)

## The sharp null says the treatment did nothing for EVERY village --
## stronger than saying the average effect is zero, and it rules out
## effects that cancel out.
##
## Compare this p-value with the one from model1. Close agreement
## means the conventional standard errors were not misleading here.
## When the two disagree, trust the randomisation inference: it
## assumes almost nothing.
