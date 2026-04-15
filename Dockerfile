FROM rstudio/plumber:latest

# Install the survival package
RUN R -e "install.packages('survival', repos='https://cloud.r-project.org')"

# Set the working directory
WORKDIR /app

# Copy your API script and model artefacts into the container
COPY . /app

# Start the Plumber API on the port provided by the host
CMD ["R", "-e", "pr <- plumber::pr('plumber.R'); pr$run(host='0.0.0.0', port=as.numeric(Sys.getenv('PORT', 10000)))"]