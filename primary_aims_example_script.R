# Virtual Module 2: Primary Aims analyses in a Prototypical SMART
# CATIE 2023
# Data-science for Dynamic Intervention Decision-making (D3C)
# University of Michigan

# Intro ------------------------------------------------------------------------

# In this small demo you will apply what you learned in the
# V2 learning module and run some code yourself!
# Don't worry if this breaks or fails-- the goal is just to attempt some
# hands-on data analysis!

# Outline ----------------------------------------------------------------------

# 1. Load and setup the simulated ADHD data (done for you)
# 2. Assign a weight of 2 to responders and a weight of 4 to non-responders (done for you)
# 3. Replicate responders (done for you)
# 4. Estimate parameters in regression model (done for you)
# 5. Estimate all possible pair-wise differences of the mean
#    under each adaptive intervention

################################################################################
# Part 1: Setup
################################################################################

library(dplyr)
library(geepack)
source("R/estimate.R")

# Load data
dat_adhd <- read.csv("data/adhd-simulated-2023.csv")
# Select the variables you will need
dat_smart <- dat_adhd %>% select(ID, Y2, A1, A2, R, odd, severity, priormed, race)
# Center each each baseline covariate by its own grand mean
dat_smart <- dat_smart %>%
  mutate(odd_c = odd - mean(odd),
         severity_c = severity - mean(severity),
         priormed_c = priormed - mean(priormed),
         race_c = race - mean(race))

################################################################################
# Part 2: Assign weights
################################################################################

# We need to create a new column that contains the weight that we will assign to
# non-responders and responders in the dataset
dat_smart <- dat_smart %>%
  mutate(design_weights = if_else(R == 1, # this is the condition that is checked
                                  2, # if condition is TRUE, design_weight is 2
                                  4)) # if condition is FALSE, design_weight is 4

################################################################################
# Part 3: Replicate responders
################################################################################

# We restructure the data set such that instead of one observation per responder,
# the new dataset includes two identical observations per responder.

# Save non-responders' observations into the variable rows_not_to_replicate
rows_not_to_replicate <- dat_smart %>% filter(R==0)

# Save responders' observations into the variable rows_to_replicate
rows_to_replicate <- dat_smart %>% filter(R==1)
# In the next two lines, we create two copies of rows_to_replicate.
# For the first copy (see next line) assign a value of +1 to A2.
plus_one_pseudodata <- rows_to_replicate %>% mutate(A2 = +1)
# For the second copy (see next line) assign a value of -1 to A2.
minus_one_pseudodata <- rows_to_replicate %>% mutate(A2 = -1)

# This is the new dataset where each responder has 2 rows and each
# non-responder has 1 row
dat_smart_replicated <- rbind(rows_not_to_replicate,
                              plus_one_pseudodata,
                              minus_one_pseudodata)

dat_smart_replicated <- dat_smart_replicated %>% arrange(ID, A1, R, A2)

# Inspect a couple of rows to verify that each responder has
# two rows (which are exact copies of each other, except for the value of A2!)
# and a weight of 2
dat_smart_replicated %>% filter(R == 1) %>% head(., n = 10)

# Inspect a couple of rows to verify that each non-responder has
# one row and a weight of 4
dat_smart_replicated %>% filter(R == 0) %>% head(., n = 10)

################################################################################
# Part 4: Estimate parameters in regression model
################################################################################

# Estimate regression model used for the third type of primary aim -------------
model <- geeglm(Y2 ~ A1 + A2 + I(A1*A2) + odd_c + severity_c + priormed_c + race_c,
                id = ID,  # remember to tell the computer which column contains the participant ID's
                data = dat_smart_replicated,  # remember to use the weighted and replicated dataset
                weights = design_weights,  # remember to tell the computer to weight each row appropriately
                corstr = "independence",  # ask the computer to treat the replicates as independent units
                std.err = "san.se") # ask the computer to give you robust standard errors

# Inspect parameter estimates
summary(model)

################################################################################
# Part 5: Estimate all pair-wise differences
################################################################################

L <- rbind(
  # These statements get the estimated end-of-study outcome mean under each AI
  "End-of-study mean: AI#1 (MED, AUGMENT)"    = c(1, -1, -1,  1, rep(0,4)),
  "End-of-study mean: AI#2 (BMOD, AUGMENT)"   = c(1,  1, -1, -1, rep(0,4)),
  "End-of-study mean: AI#3 (MED, INTENSIFY)"  = c(1, -1,  1, -1, rep(0,4)),
  "End-of-study mean: AI#4 (BMOD, INTENSIFY)" = c(1,  1,  1,  1, rep(0,4))
  )

# CODE-ALONG TASK:
# In the next lines, complete the specification of D
# so that we are able to estimate all six pairwise differences.
# Note that D should have six rows, but it currently has 5 rows.
# Which pair has not yet been included in D?
D <- rbind(
  # These statements get all pairwise differences
  "Difference in end-of-study mean: (MED, AUGMENT) vs. (BMOD, AUGMENT)" = c(L["End-of-study mean: AI#1 (MED, AUGMENT)",] - L["End-of-study mean: AI#2 (BMOD, AUGMENT)",]),
  "Difference in end-of-study mean: (MED, AUGMENT) vs. (MED, INTENSIFY)" = c(L["End-of-study mean: AI#1 (MED, AUGMENT)",] - L["End-of-study mean: AI#3 (MED, INTENSIFY)",]),
  "Difference in end-of-study mean: (MED, AUGMENT) vs. (BMOD, INTENSIFY)" = c(L["End-of-study mean: AI#1 (MED, AUGMENT)",] - L["End-of-study mean: AI#4 (BMOD, INTENSIFY)",]),
  "Difference in end-of-study mean: (BMOD, AUGMENT) vs. (MED, INTENSIFY)" = c(L["End-of-study mean: AI#2 (BMOD, AUGMENT)",] - L["End-of-study mean: AI#3 (MED, INTENSIFY)",]),
  "Difference in end-of-study mean: (BMOD, AUGMENT) vs. (BMOD, INTENSIFY)" = c(L["End-of-study mean: AI#2 (BMOD, AUGMENT)",] - L["End-of-study mean: AI#4 (BMOD, INTENSIFY)",]),
  "Difference in end-of-study mean: (MED, INTENSIFY) vs. (BMOD, INTENSIFY)" = c(L["End-of-study mean: AI#3 (MED, INTENSIFY)",] - L["End-of-study mean: AI#4 (BMOD, INTENSIFY)",])
  )

# CODE-ALONG TASK:
# In the next line, type in a call to the estimate function
estimate(model, D)


