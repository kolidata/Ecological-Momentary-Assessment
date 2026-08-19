library(readxl)
library(dplyr)
library(tidyr)
library(nlme)
library(haven)
ER_data2 <- read_sav("/Users/kakolikhatun/Downloads/ema2.sav")

resp_vars <- c("PAIN_SEV", "PAIN_INT", "PAIN_NA")
pred_vars <- c(
  "WORRY",
  "EXPERIENTIAL_AVOIDANCE",
  "SELF_CRITICISM",
  "RUMINATION",
  "EXPRESSIVE_SUPPRESSION",
  "ACCEPTANCE",
  "PROBLEM_SOLVING"
)

needed_vars <- c("StudyID", "TIME01", resp_vars, pred_vars)

# Complete cases across ALL needed variables
ER_cc <- ER_data2 %>%
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
  )

# Stack the 3 outcomes
ER_stack <- ER_cc %>%
  pivot_longer(
    cols = all_of(resp_vars),
    names_to = "Outcome",
    values_to = "Y_raw"
  ) %>%
  mutate(
    Y = log(Y_raw + 1),   
    Outcome = factor(
      Outcome,
      levels = c("PAIN_SEV", "PAIN_INT", "PAIN_NA")
    ),
    StudyID = factor(StudyID)
  ) %>%
  arrange(StudyID, Outcome, TIME01)
# outcome-specific fixed-effect columns
ER_stack <- ER_stack %>%
  mutate(
    sev_ind = as.numeric(Outcome == "PAIN_SEV"),
    int_ind = as.numeric(Outcome == "PAIN_INT"),
    na_ind  = as.numeric(Outcome == "PAIN_NA")
  ) %>%
  mutate(
    # intercept blocks
    Intercept_1 = sev_ind,
    Intercept_2 = int_ind,
    Intercept_3 = na_ind,
    
    # TIME01 blocks
    TIME01_1 = TIME01 * sev_ind,
    TIME01_2 = TIME01 * int_ind,
    TIME01_3 = TIME01 * na_ind,
    
    # covariate blocks for outcome 1 (PAIN_SEV)
    WORRY_1                  = WORRY                  * sev_ind,
    EXPERIENTIAL_AVOIDANCE_1 = EXPERIENTIAL_AVOIDANCE * sev_ind,
    SELF_CRITICISM_1         = SELF_CRITICISM         * sev_ind,
    RUMINATION_1             = RUMINATION             * sev_ind,
    EXPRESSIVE_SUPPRESSION_1 = EXPRESSIVE_SUPPRESSION * sev_ind,
    ACCEPTANCE_1             = ACCEPTANCE             * sev_ind,
    PROBLEM_SOLVING_1        = PROBLEM_SOLVING        * sev_ind,
    
    # covariate blocks for outcome 2 (PAIN_INT)
    WORRY_2                  = WORRY                  * int_ind,
    EXPERIENTIAL_AVOIDANCE_2 = EXPERIENTIAL_AVOIDANCE * int_ind,
    SELF_CRITICISM_2         = SELF_CRITICISM         * int_ind,
    RUMINATION_2             = RUMINATION             * int_ind,
    EXPRESSIVE_SUPPRESSION_2 = EXPRESSIVE_SUPPRESSION * int_ind,
    ACCEPTANCE_2             = ACCEPTANCE             * int_ind,
    PROBLEM_SOLVING_2        = PROBLEM_SOLVING        * int_ind,
    
    # covariate blocks for outcome 3 (PAIN_NA)
    WORRY_3                  = WORRY                  * na_ind,
    EXPERIENTIAL_AVOIDANCE_3 = EXPERIENTIAL_AVOIDANCE * na_ind,
    SELF_CRITICISM_3         = SELF_CRITICISM         * na_ind,
    RUMINATION_3             = RUMINATION             * na_ind,
    EXPRESSIVE_SUPPRESSION_3 = EXPRESSIVE_SUPPRESSION * na_ind,
    ACCEPTANCE_3             = ACCEPTANCE             * na_ind,
    PROBLEM_SOLVING_3        = PROBLEM_SOLVING        * na_ind,
  )

# Build fixed-effect design matrix X 
fixed_formula <- ~ 0 +
  Intercept_1 + WORRY_1 + EXPERIENTIAL_AVOIDANCE_1 + SELF_CRITICISM_1 +
  RUMINATION_1 + EXPRESSIVE_SUPPRESSION_1 + ACCEPTANCE_1 +
  PROBLEM_SOLVING_1 + TIME01_1 +
  Intercept_2 + WORRY_2 + EXPERIENTIAL_AVOIDANCE_2 + SELF_CRITICISM_2 +
  RUMINATION_2 + EXPRESSIVE_SUPPRESSION_2 + ACCEPTANCE_2 +
  PROBLEM_SOLVING_2 + TIME01_2 +
  Intercept_3 + WORRY_3 + EXPERIENTIAL_AVOIDANCE_3 + SELF_CRITICISM_3 +
  RUMINATION_3 + EXPRESSIVE_SUPPRESSION_3 + ACCEPTANCE_3 +
  PROBLEM_SOLVING_3 + TIME01_3

X_mat <- model.matrix(fixed_formula, data = ER_stack)

dim(X_mat)
colnames(X_mat)

# Create stacked response vector Y
Y_vec <- ER_stack$Y
length(Y_vec)

# Build random-effect design matrix Z (intercepts only)
rand_formula <- ~ 0 + Intercept_1 + Intercept_2 + Intercept_3

Z_mat <- model.matrix(rand_formula, data = ER_stack)

dim(Z_mat)
colnames(Z_mat)

# Quick check for one subject
one_id <- levels(ER_stack$StudyID)[2]

Yi <- ER_stack %>%
  filter(StudyID == one_id) %>%
  pull(Y)

Xi <- X_mat[ER_stack$StudyID == one_id, , drop = FALSE]
Zi <- Z_mat[ER_stack$StudyID == one_id, , drop = FALSE]

length(Yi)
dim(Xi)
dim(Zi)


# Multivariate LME — random intercepts only (3x3 covariance)
# pdSymm gives an unstructured 3x3 covariance matrix across
# the three outcome-specific random intercepts, capturing
# between-subject correlations across pain outcomes.
# weights = varIdent allows outcome-specific residual SDs.

fit_mv <- lme(
  fixed = Y ~ 0 +
    Intercept_1 + WORRY_1 + EXPERIENTIAL_AVOIDANCE_1 + SELF_CRITICISM_1 +
    RUMINATION_1 + EXPRESSIVE_SUPPRESSION_1 + ACCEPTANCE_1 +
    PROBLEM_SOLVING_1 +  TIME01_1 +
    Intercept_2 + WORRY_2 + EXPERIENTIAL_AVOIDANCE_2 + SELF_CRITICISM_2 +
    RUMINATION_2 + EXPRESSIVE_SUPPRESSION_2 + ACCEPTANCE_2 +
    PROBLEM_SOLVING_2 +  TIME01_2 +
    Intercept_3 + WORRY_3 + EXPERIENTIAL_AVOIDANCE_3 + SELF_CRITICISM_3 +
    RUMINATION_3 + EXPRESSIVE_SUPPRESSION_3 + ACCEPTANCE_3 +
    PROBLEM_SOLVING_3 + TIME01_3,
  
  # Random intercepts only — 3x3 unstructured covariance matrix
  random = list(
    StudyID = pdSymm(~ 0 + Intercept_1 + Intercept_2 + Intercept_3)
  ),
  # Outcome-specific residual variances
  weights = varIdent(form = ~ 1 | Outcome),
  data = ER_stack,
  method = "REML"
  )
summary(fit_mv)
sink("/Users/kakolikhatun/Downloads/fit_mv.txt")
summary(fit_mv)
sink()
