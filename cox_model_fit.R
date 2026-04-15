library(survival)
library(ggplot2)

# ------------------------------------------------------------------
# Assume dataset 'bc_data' exists with columns:
# time (follow-up time), status (1=event, 0=censored),
# age, stage (factor), treatment (factor)
# ------------------------------------------------------------------

# Synthetic bc_data for development purposes
set.seed(42)
n <- 200
bc_data <- data.frame(
  time      = rexp(n, rate = 0.1),
  status    = rbinom(n, 1, 0.7),
  age       = rnorm(n, mean = 55, sd = 10),
  stage     = factor(sample(c("I","II","III"), n, replace = TRUE)),
  treatment = factor(sample(c("A","B"), n, replace = TRUE))
)


# Ensure factors
bc_data$stage <- as.factor(bc_data$stage)
bc_data$treatment <- as.factor(bc_data$treatment)

# ------------------------------------------------------------------
# 1. Fit Cox PH model
# ------------------------------------------------------------------
cox_model <- coxph(Surv(time, status) ~ age + stage + treatment, data = bc_data)
summary(cox_model)

# ------------------------------------------------------------------
# 2. Kaplan-Meier curves (by treatment as example)
# ------------------------------------------------------------------
km_fit <- survfit(Surv(time, status) ~ treatment, data = bc_data)

# Convert to data frame for ggplot
km_df <- data.frame(
  time = km_fit$time,
  surv = km_fit$surv,
  strata = rep(names(km_fit$strata), km_fit$strata)
)

# Clean strata labels
km_df$treatment <- sub("treatment=", "", km_df$strata)

# Plot
ggplot(km_df, aes(x = time, y = surv, colour = treatment)) +
  geom_step() +
  labs(title = "Kaplan-Meier Survival Curves",
       x = "Time",
       y = "Survival probability") +
  theme_minimal()

# ------------------------------------------------------------------
# 3. Predict survival for a new patient
# ------------------------------------------------------------------
# Example: user-specified profile
new_patient <- data.frame(
  age = 55,
  stage = factor("II", levels = levels(bc_data$stage)),
  treatment = factor("A", levels = levels(bc_data$treatment))
)

# Predict survival curve for new patient
pred_fit <- survfit(cox_model, newdata = new_patient)

# Extract median survival (if exists)
median_surv <- summary(pred_fit)$table["median"]

print(median_surv)

# Convert predicted survival to data frame
pred_df <- data.frame(
  time = pred_fit$time,
  surv = pred_fit$surv
)

# Plot predicted survival curve
ggplot(pred_df, aes(x = time, y = surv)) +
  geom_step(colour = "blue") +
  labs(title = "Predicted Survival Curve for New Patient",
       x = "Time",
       y = "Survival probability") +
  theme_minimal()


# ------------------------------------------------------------------
# Serialise model and metadata for MCP server
# ------------------------------------------------------------------

# Save fitted model
saveRDS(cox_model, file = "cox_model.rds")

# Save factor levels for input validation at API layer
model_metadata <- list(
  stage_levels    = levels(bc_data$stage),
  treatment_levels = levels(bc_data$treatment),
  age_range       = range(bc_data$age),
  covariates      = c("age", "stage", "treatment")
)

saveRDS(model_metadata, file = "model_metadata.rds")

cat("Model serialised successfully.\n")
cat("Stage levels:    ", paste(model_metadata$stage_levels, collapse = ", "), "\n")
cat("Treatment levels:", paste(model_metadata$treatment_levels, collapse = ", "), "\n")
cat("Age range:       ", model_metadata$age_range[1], "to", model_metadata$age_range[2], "\n")

# ------------------------------------------------------------------
# Serialise pre-computed KM curves
# ------------------------------------------------------------------
km_fit <- survfit(Surv(time, status) ~ treatment, data = bc_data)

km_curves_data <- data.frame(
  time      = km_fit$time,
  surv      = km_fit$surv,
  treatment = sub("treatment=", "",
                  rep(names(km_fit$strata), km_fit$strata))
)

saveRDS(km_curves_data, file = "km_curves.rds")
cat("KM curves serialised successfully.\n")