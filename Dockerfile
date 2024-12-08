FROM rocker/Rstudio

RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \    # For curl support in R
    libxml2-dev \             # For XML parsing in R
    libssl-dev \              # For secure connections in R
    libgit2-dev \             # For Git integration in R
    && rm -rf /var/lib/apt/lists/*  # Clean up to reduce image size

RUN R -e "install.packages(c('tidyverse', 'ggplot2', 'randomForest', 'lme4', 'broom', 'glmmTMB', 'broom.mixed'), repos = 'https://cloud.r-project.org/', dependencies = TRUE, quiet = TRUE)"

CMD ["R"]
