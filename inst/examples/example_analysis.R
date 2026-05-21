# Example R analysis script to demonstrate r2sas conversion
# Run: r2sas::r2sas_file("example_analysis.R")

# Load data
df <- read.csv("clinical_trial.csv")

# Summary statistics
summary(df)

# Frequency table
table(df$treatment, df$response)

# Linear regression
fit_lm <- lm(bmi ~ age + sex + treatment, data = df)
summary(fit_lm)

# Logistic regression
fit_logit <- glm(response ~ age + bmi + treatment, family = binomial, data = df)
summary(fit_logit)

# t-test comparing two groups
t.test(df$bmi[df$treatment == "A"], df$bmi[df$treatment == "B"])

# Chi-square test
chisq.test(table(df$sex, df$response))

# ANOVA
fit_aov <- aov(bmi ~ treatment, data = df)
summary(fit_aov)

# dplyr pipeline
library(dplyr)
high_risk <- df %>%
  filter(age > 65) %>%
  select(id, age, bmi, response) %>%
  arrange(desc(bmi))

# Plot
library(ggplot2)
ggplot(df, aes(x = age, y = bmi)) + geom_point()

# Write output
write.csv(high_risk, "high_risk_patients.csv")
