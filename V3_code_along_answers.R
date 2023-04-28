# Virtual Module 3: Secondary Aims analyses in a SMART
# CATIE 2023
# Data-science for Dynamic Intervention Decision-making (D3C)
# University of Michigan

# Intro -----------------------------------------------------------------

# In this small demo you will apply what you learned in the
# V3 learning module and run some code yourself!
# Don't worry if this breaks or fails-- the goal is just to attempt some
# hands-on data analysis!

# Outline ---------------------------------------------------------------

# 1. Setup and load the simulated ADHD data (done for you)
# 2. Visualize baseline variable ODD
# 3. Fit a moderated regression model for ODD
# 4. Test if ODD is a useful tailoring variable

# Setup -------------------------------------------------------------------

library(geepack)
library(emmeans)

# Load data
dat_adhd <- read.csv("data/adhd-simulated-2023.csv")

# ODD diagnosis before the first-stage intervention (yes; odd = 1)
table(dat_adhd$odd)

# Fit moderated regression for ODD --------------------------------------------------

fit <- geeglm(Y2 ~ severity + priormed + race + odd + A1 + A1:odd,
              data = dat_adhd,
              id = ID)

summary(fit)


# Test if ODD is useful tailoring variable --------------------------------

# Use functions from the emmeans package
em <- emmeans(fit, ~ A1 | odd, weights = "proportional")
summary(em)

contrast(em, method = "revpairwise")

emmip(em, A1 ~ odd, style = "factor")

