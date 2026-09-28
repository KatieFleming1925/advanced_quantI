
###################################
###################################
#####  REPLICATION FILE FOR: ######
###  MY HISTORY OR OUR HISTORY? ###
###################################
###################################

# ========================================================================= #
# - Script: Load required packages
# - Author: Nicholas Haas (nick.haas@ps.au.dk) 
#           Emmy Lindstam (emmy.lindstam@ie.edu)
# ========================================================================= #

# Required packages -------------------------------------------------------

## general purpose
library(MASS)
library(tidyverse)
library(here)
library(broom)
library(marginaleffects)

## Indices
# install.packages("devtools")
devtools::install_github("graemeblair/stdidx")
library(stdidx)

## Plots / tables
library(stargazer)
library(gridExtra)
library(ggeffects)
library(ggrepel)
library(emmeans)
library(tidytext)
library(estimatr)
library(stopwords)


# Custom functions --------------------------------------------------------

# Recoding function
recode_treatment <- function(x){
  recode_factor(x, 
                `2` = "Exclusive",
                `1` = "Control",
                `3` = "Inclusive")
}

# Make a custom theme for GGplot figures:
custom_theme <- function() {
  theme_minimal() +
    theme(
      legend.title = element_blank(),
      legend.text = element_text(size = 11),
      strip.text.x = element_text(size = 11),
      axis.text.x = element_text(size = 11),
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.line = element_blank()
    )
}