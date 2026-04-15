#!/usr/bin/env Rscript

# ==========================================================================
# MCP stdio server for Cox PH model
# Protocol: JSON-RPC 2.0 over newline-delimited stdin/stdout
# Spec version: 2025-03-26
# ==========================================================================

# ==========================================================================
# CRITICAL: Nothing must reach stdout before JSON-RPC responses.
# Redirect the message stream to stderr FIRST, then load packages
# with all warnings and messages suppressed.
# ==========================================================================

# Redirect R's message stream (warnings, messages, conditions) to stderr
sink(stderr(), type = "message")

# Suppress absolutely everything during package loading
suppressWarnings(suppressPackageStartupMessages({
  library(jsonlite)
  library(survival)
}))

# --------------------------------------------------------------------------
# Configuration — adjust these paths to match your environment
# --------------------------------------------------------------------------
MODEL_DIR <- Sys.getenv("COX_MODEL_DIR", ".")

# --------------------------------------------------------------------------
# Load serialised model artefacts
# --------------------------------------------------------------------------
load_artefacts <- function(dir) {
  cox_path  <- file.path(dir, "cox_model.rds")
  meta_path <- file.path(dir, "model_metadata.rds")
  km_path   <- file.path(dir, "km_curves.rds")
  
  missing <- !file.exists(c(cox_path, meta_path, km_path))
  if (any(missing)) {
    stop("Missing artefact(s): ",
         paste(c(cox_path, meta_path, km_path)[missing], collapse = ", "))
  }
  
  list(
    cox_model = readRDS(cox_path),
    metadata  = readRDS(meta_path),
    km_curves = readRDS(km_path)
  )
}

# --------------------------------------------------------------------------
# Logging — always to stderr so it never pollutes the JSON-RPC stream
# --------------------------------------------------------------------------
log_msg <- function(...) {
  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " [MCP] ",
      paste0(...), "\n", file = stderr())
}

# --------------------------------------------------------------------------
# JSON-RPC helpers
# --------------------------------------------------------------------------
send_response <- function(response) {
  json <- toJSON(response, auto_unbox = TRUE, null = "null")
  cat(json, "\n", sep = "", file = stdout())
  flush(stdout())
  log_msg(">>> ", json)
}

make_result <- function(id, result) {
  list(jsonrpc = "2.0", id = id, result = result)
}

make_error <- function(id, code, message, data = NULL) {
  err <- list(code = code, message = message)
  if (!is.null(data)) err$data <- data
  list(jsonrpc = "2.0", id = id, error = err)
}

# JSON-RPC 2.0 standard error codes
PARSE_ERROR      <- -32700L
INVALID_REQUEST  <- -32600L
METHOD_NOT_FOUND <- -32601L
INVALID_PARAMS   <- -32602L

# --------------------------------------------------------------------------
# Tool definitions (MCP tools/list response)
# --------------------------------------------------------------------------
tool_definitions <- function(meta) {
  list(
    list(
      name = "predict_survival",
      description = paste(
        "Predict survival curve and median survival time for a new patient",
        "using a fitted Cox proportional hazards model.",
        "Returns time points, survival probabilities, and median survival."
      ),
      inputSchema = list(
        type = "object",
        properties = list(
          age = list(
            type = "number",
            description = sprintf(
              "Patient age (numeric, range %.0f-%.0f in training data)",
              meta$age_range[1], meta$age_range[2]
            )
          ),
          stage = list(
            type = "string",
            description = "Cancer stage",
            enum = meta$stage_levels
          ),
          treatment = list(
            type = "string",
            description = "Treatment group",
            enum = meta$treatment_levels
          )
        ),
        required = c("age", "stage", "treatment")
      )
    ),
    list(
      name = "get_km_curves",
      description = paste(
        "Retrieve pre-computed Kaplan-Meier survival curves stratified",
        "by treatment group. Returns time, survival probability, and",
        "treatment label for each curve point."
      ),
      inputSchema = list(
        type = "object",
        properties = setNames(list(), character(0))
      )
    ),
    list(
      name = "get_model_metadata",
      description = paste(
        "Retrieve metadata about the fitted Cox PH model, including",
        "factor levels for stage and treatment, and the age range",
        "observed in the training data."
      ),
      inputSchema = list(
        type = "object",
        properties = setNames(list(), character(0))
      )
    )
  )
}

# --------------------------------------------------------------------------
# Tool execution handlers
# --------------------------------------------------------------------------
handle_predict_survival <- function(args, artefacts) {
  meta <- artefacts$metadata
  model <- artefacts$cox_model
  
  
  # --- Validate inputs ---
  if (is.null(args$age) || is.null(args$stage) || is.null(args$treatment)) {
    return(list(
      content = list(list(type = "text",
                          text = "Error: age, stage, and treatment are all required.")),
      isError = TRUE
    ))
  }
  
  age <- as.numeric(args$age)
  if (is.na(age)) {
    return(list(
      content = list(list(type = "text",
                          text = "Error: age must be numeric.")),
      isError = TRUE
    ))
  }
  
  if (!(args$stage %in% meta$stage_levels)) {
    return(list(
      content = list(list(type = "text",
                          text = sprintf("Error: stage must be one of: %s",
                                         paste(meta$stage_levels, collapse = ", ")))),
      isError = TRUE
    ))
  }
  
  if (!(args$treatment %in% meta$treatment_levels)) {
    return(list(
      content = list(list(type = "text",
                          text = sprintf("Error: treatment must be one of: %s",
                                         paste(meta$treatment_levels, collapse = ", ")))),
      isError = TRUE
    ))
  }
  
  # --- Build newdata and predict ---
  new_patient <- data.frame(
    age       = age,
    stage     = factor(args$stage, levels = meta$stage_levels),
    treatment = factor(args$treatment, levels = meta$treatment_levels)
  )
  
  pred_fit <- survfit(model, newdata = new_patient)
  
  # Extract median survival
  median_surv <- summary(pred_fit)$table["median"]
  if (is.na(median_surv)) median_surv <- NULL
  
  # Build survival curve data
  surv_curve <- data.frame(
    time = pred_fit$time,
    survival = round(pred_fit$surv, 6)
  )
  
  result <- list(
    patient_profile = list(
      age = age,
      stage = args$stage,
      treatment = args$treatment
    ),
    median_survival = median_surv,
    survival_curve = surv_curve
  )
  
  list(
    content = list(list(
      type = "text",
      text = toJSON(result, auto_unbox = TRUE, pretty = TRUE)
    )),
    isError = FALSE
  )
}

handle_get_km_curves <- function(args, artefacts) {
  km <- artefacts$km_curves
  list(
    content = list(list(
      type = "text",
      text = toJSON(km, auto_unbox = TRUE, pretty = TRUE)
    )),
    isError = FALSE
  )
}

handle_get_metadata <- function(args, artefacts) {
  meta <- artefacts$metadata
  list(
    content = list(list(
      type = "text",
      text = toJSON(meta, auto_unbox = TRUE, pretty = TRUE)
    )),
    isError = FALSE
  )
}

# --------------------------------------------------------------------------
# Method dispatcher
# --------------------------------------------------------------------------
dispatch <- function(method, id, params, artefacts, meta) {
  switch(method,
         
         "initialize" = {
           # Echo back the protocol version the client requested
           client_version <- params$protocolVersion %||% "2025-03-26"
           # CRITICAL: tools must be {} not [] — use named list or setNames
           send_response(make_result(id, list(
             protocolVersion = client_version,
             capabilities = list(
               tools = setNames(list(), character(0))
             ),
             serverInfo = list(
               name    = "cox-model-server",
               version = "0.1.0"
             )
           )))
         },
         
         # Notification — no response required
         "notifications/initialized" = {
           log_msg("Client confirmed initialisation")
         },
         
         "initialized" = {
           log_msg("Client confirmed initialisation (short form)")
         },
         
         "ping" = {
           send_response(make_result(id, list()))
         },
         
         "tools/list" = {
           send_response(make_result(id, list(
             tools = tool_definitions(meta)
           )))
         },
         
         "tools/call" = {
           tool_name <- params$name
           tool_args <- params$arguments %||% list()
           
           handler <- switch(tool_name,
                             "predict_survival"   = handle_predict_survival,
                             "get_km_curves"      = handle_get_km_curves,
                             "get_model_metadata" = handle_get_metadata,
                             NULL
           )
           
           if (is.null(handler)) {
             send_response(make_error(id, INVALID_PARAMS,
                                      sprintf("Unknown tool: %s", tool_name)))
           } else {
             tool_result <- tryCatch(
               handler(tool_args, artefacts),
               error = function(e) {
                 list(
                   content = list(list(type = "text",
                                       text = sprintf("Internal error: %s", conditionMessage(e)))),
                   isError = TRUE
                 )
               }
             )
             send_response(make_result(id, tool_result))
           }
         },
         
         # Default: method not found
         {
           if (!is.null(id)) {
             send_response(make_error(id, METHOD_NOT_FOUND,
                                      sprintf("Method not found: %s", method)))
           }
         }
  )
}

# --------------------------------------------------------------------------
# Null-coalescing operator (base R doesn't have %||% before 4.4)
# --------------------------------------------------------------------------
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}

# --------------------------------------------------------------------------
# Main loop
# --------------------------------------------------------------------------
main <- function() {
  log_msg("Starting Cox model MCP server")
  log_msg("Model directory: ", MODEL_DIR)
  
  artefacts <- tryCatch(
    load_artefacts(MODEL_DIR),
    error = function(e) {
      log_msg("FATAL: ", conditionMessage(e))
      quit(status = 1)
    }
  )
  
  meta <- artefacts$metadata
  log_msg("Artefacts loaded successfully")
  log_msg("  Stage levels: ", paste(meta$stage_levels, collapse = ", "))
  log_msg("  Treatment levels: ", paste(meta$treatment_levels, collapse = ", "))
  log_msg("  Age range: ", meta$age_range[1], " - ", meta$age_range[2])
  log_msg("Listening on stdin...")
  
  con <- file("stdin", open = "r", blocking = TRUE)
  
  while (TRUE) {
    line <- tryCatch(
      readLines(con, n = 1, warn = FALSE),
      error = function(e) {
        log_msg("readLines error: ", conditionMessage(e))
        character(0)
      }
    )
    
    # EOF — client closed stdin, shut down gracefully
    if (length(line) == 0) {
      log_msg("stdin closed, shutting down")
      break
    }
    
    # Strip BOM if present (Windows can inject these)
    line <- sub("^\xEF\xBB\xBF", "", line)
    line <- sub("^\uFEFF", "", line)
    
    # Skip blank lines
    if (nchar(trimws(line)) == 0) next
    
    log_msg("<<< ", line)
    
    # Parse JSON-RPC message
    msg <- tryCatch(
      fromJSON(line, simplifyVector = FALSE),
      error = function(e) NULL
    )
    
    if (is.null(msg)) {
      send_response(make_error(NULL, PARSE_ERROR, "Failed to parse JSON"))
      next
    }
    
    # Validate JSON-RPC 2.0
    if (is.null(msg$jsonrpc) || msg$jsonrpc != "2.0") {
      send_response(make_error(msg$id, INVALID_REQUEST,
                               "Only JSON-RPC 2.0 is supported"))
      next
    }
    
    if (is.null(msg$method)) {
      send_response(make_error(msg$id, INVALID_REQUEST,
                               "Missing method field"))
      next
    }
    
    # Dispatch
    dispatch(
      method    = msg$method,
      id        = msg$id,        # NULL for notifications
      params    = msg$params %||% list(),
      artefacts = artefacts,
      meta      = meta
    )
  }
  
  log_msg("Server stopped")
}

# --------------------------------------------------------------------------
# Entry point
# --------------------------------------------------------------------------
main()