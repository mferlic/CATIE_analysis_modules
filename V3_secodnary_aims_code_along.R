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

# 1. Setup and load the simulated ADHD data
# 2. Center baseline covariates and build contrast matrix
# 3. Use qlaci to determine if ODD is useful for tailoring first-stage
#     intervention options in a more deeply-tailored AI.
# 4. Plot

# Setup -------------------------------------------------------------------

library(qlaci)
library(dplyr)
library(ggplot2)

# Load data
dat_adhd <- read.csv("data/adhd-simulated-2023.csv")

# Proportion of ODD diagnosis before the first-stage intervention (yes, odd = 1; no, odd = 0)
prop.table(table(dat_adhd$odd))

# Center and build contrast matrix ----------------------------------------

# Grand mean center all baseline covariates, append '_c' for centered
dat_adhd_c <- dat_adhd %>% mutate(odd_c = odd - mean(odd),
                                  severity_c = severity - mean(severity),
                                  priormed_c = priormed - mean(priormed),
                                  race_c = race - mean(race))

# Contrast matrix for first-stage tailoring
c1 <-
  rbind(
    "Mean Y under bmod, ODD true"          = c(1, rep(0, 3), 1,  1,  1),
    "Mean Y under med, ODD true"           = c(1, rep(0, 3), 1, -1, -1),
    "Mean diff (bmod-med) for ODD true"    = c(0, rep(0, 3), 0,  2,  2),
    "Mean Y under bmod, ODD false"       = c(1, rep(0, 3), 0,   1,  0),
    "Mean Y under med, ODD false"        = c(1, rep(0, 3), 0,  -1, 0),
    "Mean diff (bmod-med) for ODD false" = c(0, rep(0, 3), 0,  2,  0)
  )

colnames(c1) <- c("Int", "severity_c", "race_c", "priormed_c", "odd", "A1", "A1:odd")

print(c1)

# qlaci -------------------------------------------------------------------

# qlaci uses bootstrap resampling so set the seed for reproducibility
set.seed(1234)

# This code chunk fits Q-learning to the ADHD SMART data.
# The second-stage tailoring (adherence) is identical to the V3 follow along.
# However, we've now decided to see if ODD (instead of priormed) is useful for
# tailoring first-stage intervention constructing in a more deeply-tailored AI.

q1 <-  with(dat_adhd_c,
            qlaci::qlaci(H10 = cbind(1, severity_c, race_c, priormed_c, odd),
                    H11 = cbind(A1 = 1, "A1:odd" = odd),
                    A1 = A1,
                    Y1 = rep(0, nrow(dat_adhd_c)),
                    H20 = cbind(1, odd_c, severity_c, race_c, priormed_c, A1, adherence),
                    H21 = cbind(A2 = 1, "A2:A1" = A1, "A2:adherence" = adherence),
                    A2 = A2,
                    Y2 = Y2,
                    S = 1 - R,
                    c1 = t(c1))
            )

# ci1 contains the estimated "contrast" (c1) of the stage 1 regression parameters.
print(q1)


# Plot contrasts ----------------------------------------------------------

df <- data.frame(q1$ci1)[c(3,6), ]
df$ODD <- as.factor(c(1,0))

df %>% ggplot(aes(x = ODD, y = est)) +
  geom_point() +
  geom_errorbar(aes(ymin = low, ymax = upp), width = 0.2) +
  geom_hline(yintercept = 0, linetype="dashed") +
  scale_y_continuous(limits = c(-0.5, NA)) +
  xlab("Levels of ODD") +
  labs(title = "Mean diff (BMOD(1) - MED(-1))") +
  theme_bw()



