#!/usr/bin/env Rscript

# ==========================================================================
# test_mcp_server.R
# Sends JSON-RPC messages to mcp_server.R via a pipe and prints responses.
# Run from the directory containing your .rds artefacts and mcp_server.R
#
# Usage:  Rscript test_mcp_server.R
# ==========================================================================

library(jsonlite)

# --- Configuration --------------------------------------------------------
server_script <- file.path(getwd(), "mcp_server.R")
model_dir     <- getwd()

cat("Starting MCP server subprocess...\n")
cat("Script: ", server_script, "\n")
cat("Model dir: ", model_dir, "\n\n")

# Open a two-way pipe to the server
# On Windows, use pipe(); on Unix, use pipe() with open = "r+"
# For cross-platform, we use processx if available, otherwise system2

# --- Helper: send one message and collect response ------------------------
send_and_receive <- function(proc, message) {
  json <- toJSON(message, auto_unbox = TRUE)
  cat(">>> SENDING:\n", json, "\n\n")

  # Write to process stdin
  writeLines(json, proc$get_input_connection())

  # Small delay to let server process
 Sys.sleep(0.5)

  # Read available output
  output <- proc$read_output_lines()
  if (length(output) > 0) {
    for (line in output) {
      parsed <- tryCatch(fromJSON(line, simplifyVector = FALSE), error = function(e) NULL)
      if (!is.null(parsed)) {
        cat("<<< RESPONSE:\n", toJSON(parsed, auto_unbox = TRUE, pretty = TRUE), "\n\n")
      }
    }
  }

  # Also print any stderr (server logs)
  err_output <- proc$read_error_lines()
  if (length(err_output) > 0) {
    cat("--- SERVER LOG ---\n")
    cat(paste(err_output, collapse = "\n"), "\n")
    cat("------------------\n\n")
  }
}

# --- Check for processx --------------------------------------------------
if (!requireNamespace("processx", quietly = TRUE)) {
  cat("Installing processx for subprocess management...\n")
  install.packages("processx", repos = "https://cloud.r-project.org")
}

library(processx)

# Start the server as a subprocess
proc <- process$new(
  command = "Rscript",
  args = server_script,
  stdin = "|",
  stdout = "|",
  stderr = "|",
  env = c(Sys.getenv(), COX_MODEL_DIR = model_dir)
)

cat("Server PID:", proc$get_pid(), "\n")
Sys.sleep(2)  # Let the server load artefacts

# Print any startup logs
startup_err <- proc$read_error_lines()
if (length(startup_err) > 0) {
  cat("--- STARTUP LOG ---\n")
  cat(paste(startup_err, collapse = "\n"), "\n")
  cat("-------------------\n\n")
}

# ==========================================================================
# Test 1: Initialize
# ==========================================================================
cat("=" , rep("=", 60), "\n", sep = "")
cat("TEST 1: initialize\n")
cat(rep("=", 61), "\n", sep = "")

send_and_receive(proc, list(
  jsonrpc = "2.0",
  id = 1L,
  method = "initialize",
  params = list(
    protocolVersion = "2025-03-26",
    capabilities = list(),
    clientInfo = list(name = "test-client", version = "1.0.0")
  )
))

# ==========================================================================
# Test 2: notifications/initialized (no response expected)
# ==========================================================================
cat(rep("=", 61), "\n", sep = "")
cat("TEST 2: notifications/initialized\n")
cat(rep("=", 61), "\n", sep = "")

json <- toJSON(list(
  jsonrpc = "2.0",
  method = "notifications/initialized"
), auto_unbox = TRUE)
writeLines(json, proc$get_input_connection())
cat(">>> SENT (notification, no response expected):\n", json, "\n\n")
Sys.sleep(0.3)

# Drain any logs
err <- proc$read_error_lines()
if (length(err) > 0) {
  cat("--- SERVER LOG ---\n", paste(err, collapse = "\n"), "\n---\n\n")
}

# ==========================================================================
# Test 3: tools/list
# ==========================================================================
cat(rep("=", 61), "\n", sep = "")
cat("TEST 3: tools/list\n")
cat(rep("=", 61), "\n", sep = "")

send_and_receive(proc, list(
  jsonrpc = "2.0",
  id = 2L,
  method = "tools/list",
  params = list()
))

# ==========================================================================
# Test 4: tools/call — predict_survival
# ==========================================================================
cat(rep("=", 61), "\n", sep = "")
cat("TEST 4: tools/call predict_survival\n")
cat(rep("=", 61), "\n", sep = "")

send_and_receive(proc, list(
  jsonrpc = "2.0",
  id = 3L,
  method = "tools/call",
  params = list(
    name = "predict_survival",
    arguments = list(
      age = 55,
      stage = "II",
      treatment = "A"
    )
  )
))

# ==========================================================================
# Test 5: tools/call — get_km_curves
# ==========================================================================
cat(rep("=", 61), "\n", sep = "")
cat("TEST 5: tools/call get_km_curves\n")
cat(rep("=", 61), "\n", sep = "")

send_and_receive(proc, list(
  jsonrpc = "2.0",
  id = 4L,
  method = "tools/call",
  params = list(
    name = "get_km_curves",
    arguments = list()
  )
))

# ==========================================================================
# Test 6: tools/call — get_model_metadata
# ==========================================================================
cat(rep("=", 61), "\n", sep = "")
cat("TEST 6: tools/call get_model_metadata\n")
cat(rep("=", 61), "\n", sep = "")

send_and_receive(proc, list(
  jsonrpc = "2.0",
  id = 5L,
  method = "tools/call",
  params = list(
    name = "get_model_metadata",
    arguments = list()
  )
))

# ==========================================================================
# Test 7: Invalid tool name (error handling)
# ==========================================================================
cat(rep("=", 61), "\n", sep = "")
cat("TEST 7: tools/call with unknown tool\n")
cat(rep("=", 61), "\n", sep = "")

send_and_receive(proc, list(
  jsonrpc = "2.0",
  id = 6L,
  method = "tools/call",
  params = list(
    name = "nonexistent_tool",
    arguments = list()
  )
))

# ==========================================================================
# Test 8: Invalid stage value (validation)
# ==========================================================================
cat(rep("=", 61), "\n", sep = "")
cat("TEST 8: tools/call predict_survival with invalid stage\n")
cat(rep("=", 61), "\n", sep = "")

send_and_receive(proc, list(
  jsonrpc = "2.0",
  id = 7L,
  method = "tools/call",
  params = list(
    name = "predict_survival",
    arguments = list(
      age = 55,
      stage = "IV",
      treatment = "A"
    )
  )
))

# ==========================================================================
# Clean up
# ==========================================================================
cat(rep("=", 61), "\n", sep = "")
cat("Shutting down server...\n")
proc$kill()
cat("Done.\n")
