####################################################################
##  Advanced Research Methods I (Causal Inference) -- Week 4 lab
##  Instrumental variables
##
##  Part A: Card (1995), college proximity as an instrument for
##          years of schooling.
##  Part B: a simulation in which we know the truth, to see what a
##          weak instrument actually does.
##
##  No data file needed -- the Card data ships with the wooldridge
##  package.
####################################################################


## ---- 0. Packages -------------------------------------------------
# install.packages(c("AER", "wooldridge"))

library(AER)          # ivreg()
library(wooldridge)   # the card data


## ---- 1. The data -------------------------------------------------

data(card)

## lwage    log hourly wage
## educ     years of schooling
## nearc4   grew up near a four-year college (0/1)
## exper, expersq, black, south, smsa   controls

dim(card)
table(card$nearc4)
summary(card[, c("lwage", "educ", "nearc4")])

## Keep the control set in one place so every model uses the same one.
controls <- "exper + expersq + black + south + smsa"


## ---- 2. The confounded benchmark ---------------------------------

ols <- lm(as.formula(paste("lwage ~ educ +", controls)), data = card)
summary(ols)

## QUESTION: which way is this biased, and why?
## (Standard story: upward. Ability raises schooling AND wages, so
## the coefficient absorbs some of it. Hold that prediction.)


## ---- 3. First stage ----------------------------------------------
## The only assumption you can actually test.

first_stage <- lm(as.formula(paste("educ ~ nearc4 +", controls)),
                  data = card)
summary(first_stage)

## The F-statistic on the EXCLUDED instrument specifically -- not the
## overall F of the regression. Compare the model with and without it.

fs_restricted <- lm(as.formula(paste("educ ~", controls)), data = card)
anova(fs_restricted, first_stage)     # the F here is the one to report

## QUESTION: is it in the direction you expected? Would you build an
## estimate on it?


## ---- 4. Reduced form, and Wald by hand ----------------------------

reduced_form <- lm(as.formula(paste("lwage ~ nearc4 +", controls)),
                   data = card)
summary(reduced_form)

## The Wald ratio: reduced form divided by first stage.
wald <- coef(reduced_form)["nearc4"] / coef(first_stage)["nearc4"]
wald

## QUESTION: why is the reduced-form coefficient so much smaller than
## the Wald estimate?


## ---- 5. Two-stage least squares, properly -------------------------
## Everything after the | is the instrument set: the excluded
## instrument plus all exogenous controls.

iv_model <- ivreg(
  as.formula(paste("lwage ~ educ +", controls,
                   "|  nearc4 +", controls)),
  data = card
)

summary(iv_model, diagnostics = TRUE)

## Confirm it matches the hand-rolled Wald estimate:
c(hand_rolled = unname(wald), ivreg = unname(coef(iv_model)["educ"]))

## Side by side with OLS:
c(OLS = unname(coef(ols)["educ"]),
  IV  = unname(coef(iv_model)["educ"]))

## QUESTION (groups, 10 min): you predicted OLS was biased UPWARD.
## IV came out HIGHER. Give two explanations -- one that saves the
## instrument, one that kills it.


## ---- 6. Why not just run two regressions yourself? ----------------
## You can. The coefficient is right and the standard error is wrong,
## because the second stage treats the fitted values as data rather
## than as something you estimated.

card$educ_hat <- fitted(first_stage)
manual <- lm(as.formula(paste("lwage ~ educ_hat +", controls)),
             data = card)

## Coefficients agree:
c(manual = unname(coef(manual)["educ_hat"]),
  ivreg  = unname(coef(iv_model)["educ"]))

## Standard errors do NOT:
c(manual = unname(summary(manual)$coefficients["educ_hat", "Std. Error"]),
  ivreg  = unname(summary(iv_model)$coefficients["educ", "Std. Error"]))

## Use ivreg(). Always.


####################################################################
## PART B: what a weak instrument actually does
####################################################################

## Here we KNOW the true effect, because we made it up. True effect
## of D on Y is 1. U confounds both, so OLS will be biased upward.
## pi controls the strength of the first stage.

simulate_once <- function(n = 1000, pi = 0.5) {
  Z <- rnorm(n)                          # the instrument
  U <- rnorm(n)                          # unobserved confounder
  D <- pi * Z + U + rnorm(n)             # treatment
  Y <- 1 * D + 2 * U + rnorm(n)          # TRUE effect of D on Y is 1

  ols_est <- coef(lm(Y ~ D))["D"]
  iv_est  <- coef(ivreg(Y ~ D | Z))["D"]
  f_stat  <- summary(lm(D ~ Z))$fstatistic[1]

  c(ols = unname(ols_est), iv = unname(iv_est), F = unname(f_stat))
}

set.seed(2026)

## Strong instrument: IV recovers the truth, OLS does not.
strong <- replicate(500, simulate_once(pi = 0.5))
rowMeans(strong)

## Now weaken it, step by step.
for (p in c(0.5, 0.2, 0.1, 0.05, 0.02)) {
  res <- replicate(500, simulate_once(pi = p))
  cat(sprintf("pi = %.2f | mean F = %6.1f | mean OLS = %5.2f | mean IV = %5.2f\n",
              p, mean(res["F", ]), mean(res["ols", ]), mean(res["iv", ])))
}

## Read the last column downwards. The true effect is 1.
##
## The IV estimate does not just get noisy as the instrument weakens.
## It drifts back toward the OLS estimate -- back toward exactly the
## bias you used an instrument to escape.
##
## Look at where that starts happening relative to F = 10.

## How variable is it? Compare the spread at pi = 0.5 and pi = 0.05:
sd(replicate(500, simulate_once(pi = 0.50))["iv", ])
sd(replicate(500, simulate_once(pi = 0.05))["iv", ])

## TAKEAWAY: a paper reporting an IV estimate without a first stage
## has not given you enough to judge it. That is your job next week.
