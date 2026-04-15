library(httr)

base_url <- "http://localhost:8000"

# ------------------------------------------------------------------
# Test 1: metadata
# ------------------------------------------------------------------
resp <- GET(paste0(base_url, "/metadata"))
cat("Status:", status_code(resp), "\n")
print(content(resp))

# ------------------------------------------------------------------
# Test 2: KM curves
# ------------------------------------------------------------------
resp <- GET(paste0(base_url, "/km_curves"))
cat("Status:", status_code(resp), "\n")
head(as.data.frame(content(resp)))

# ------------------------------------------------------------------
# Test 3: predict survival
# ------------------------------------------------------------------
resp <- GET(paste0(base_url, "/predict_survival"),
            query = list(age = 55, stage = "II", treatment = "A"))
cat("Status:", status_code(resp), "\n")
print(content(resp)$median_survival)