# plumber.R
library(plumber)
library(survival)

# ------------------------------------------------------------------
# Load model and metadata once at startup
# ------------------------------------------------------------------
cox_model      <- readRDS("cox_model.rds")
model_metadata <- readRDS("model_metadata.rds")
km_curves_data <- readRDS("km_curves.rds")

# ------------------------------------------------------------------
# Input validation helper
# ------------------------------------------------------------------
validate_input <- function(age, stage, treatment) {
  errors <- c()
  
  if (!is.numeric(age) || age < 0 || age > 120)
    errors <- c(errors, "age must be numeric between 0 and 120")
  
  if (!stage %in% model_metadata$stage_levels)
    errors <- c(errors, paste("stage must be one of:", 
                              paste(model_metadata$stage_levels, collapse = ", ")))
  
  if (!treatment %in% model_metadata$treatment_levels)
    errors <- c(errors, paste("treatment must be one of:", 
                              paste(model_metadata$treatment_levels, collapse = ", ")))
  errors
}

# ------------------------------------------------------------------
# Endpoint 1: predict survival for a new patient
# ------------------------------------------------------------------
#* @param age Numeric age of patient
#* @param stage Disease stage (I, II, or III)
#* @param treatment Treatment arm (A or B)
#* @get /predict_survival
function(age, stage, treatment) {
  
  age <- as.numeric(age)
  
  errors <- validate_input(age, stage, treatment)
  if (length(errors) > 0)
    stop(paste(errors, collapse = "; "))
  
  new_patient <- data.frame(
    age       = age,
    stage     = factor(stage,     levels = model_metadata$stage_levels),
    treatment = factor(treatment, levels = model_metadata$treatment_levels)
  )
  
  pred_fit    <- survfit(cox_model, newdata = new_patient)
  median_surv <- summary(pred_fit)$table["median"]
  
  list(
    median_survival = as.numeric(median_surv),
    survival_curve  = data.frame(
      time = pred_fit$time,
      surv = pred_fit$surv
    )
  )
}

# ------------------------------------------------------------------
# Endpoint 2: pre-computed KM curves
# ------------------------------------------------------------------
#* @get /km_curves
function() {
  km_curves_data
}

# ------------------------------------------------------------------
# Endpoint 3: model metadata
# ------------------------------------------------------------------
#* @get /metadata
function() {
  model_metadata
}