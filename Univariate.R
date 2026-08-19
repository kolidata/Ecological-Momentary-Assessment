library(haven)
library(dplyr)
library(nlme)
ER_data2 <- read_sav("/Users/kakolikhatun/Downloads/ema2.sav")
ER_data_clean <- ER_data %>%
  filter(
    !is.na(PAIN_SEV) &
      !is.na(PAIN_INT) &
      !is.na(PAIN_NA) &
      !is.na(WORRY) &
      !is.na(SELF_CRITICISM) &
      !is.na(EXPERIENTIAL_AVOIDANCE) &
      !is.na(RUMINATION) &
      !is.na(EXPRESSIVE_SUPPRESSION) &
      !is.na(ACCEPTANCE) &
      !is.na(PROBLEM_SOLVING)
  ) %>%
  mutate(
    log_PAIN_SEV = log(PAIN_SEV + 1),
    log_PAIN_INT = log(PAIN_INT + 1),
    log_PAIN_NA  = log(PAIN_NA + 1),
    StudyID = factor(StudyID)
  )
fit_painsev <- lme(
  fixed = log_PAIN_SEV ~ WORRY + EXPERIENTIAL_AVOIDANCE + SELF_CRITICISM +
    RUMINATION + EXPRESSIVE_SUPPRESSION + ACCEPTANCE + PROBLEM_SOLVING + TIME01,
  random = ~ 1 | StudyID,
  method = "REML",
  data = ER_data_clean,
  na.action = na.omit
)
summary(fit_painsev)
fit_painint <- lme(
  fixed = log_PAIN_INT ~ WORRY + EXPERIENTIAL_AVOIDANCE + SELF_CRITICISM +
    RUMINATION + EXPRESSIVE_SUPPRESSION + ACCEPTANCE + PROBLEM_SOLVING + TIME01,
  random = ~ 1 | StudyID,
  method = "REML",
  data = ER_data_clean,
  na.action = na.omit
)
summary(fit_painint)
fit_painna <- lme(
  fixed = log_PAIN_NA ~ WORRY + EXPERIENTIAL_AVOIDANCE + SELF_CRITICISM +
    RUMINATION + EXPRESSIVE_SUPPRESSION + ACCEPTANCE + PROBLEM_SOLVING + TIME01,
  random = ~ 1 | StudyID,
  method = "REML",
  data = ER_data_clean,
  na.action = na.omit
)
summary(fit_painna)

