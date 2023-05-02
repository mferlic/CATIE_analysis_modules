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
prop.table(table(dat_adhd$odd))

# Fit second-stage moderated regression for ODD --------------------------------------------------

# Subset to non-responders
dat_adhd_nr <- subset(dat_adhd, R == 0)

# Fit GEE interacting ODD with A2
fit <- geeglm(Y2 ~ severity + priormed + adherence + race + odd + A1 + A2*odd,
              data = dat_adhd_nr,
              id = ID)

summary(fit)

# Test if ODD is a useful tailoring variable --------------------------------
# Use functions from the emmeans package
em <- emmeans(fit, ~ A2 | odd, weights = "proportional")
summary(em)

contrast(em, method = "revpairwise")

emmip(em, A2 ~ odd, style = "factor")

# still better to AUG(-1)
