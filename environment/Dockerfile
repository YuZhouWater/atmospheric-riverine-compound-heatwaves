FROM registry.codeocean.com/codeocean/r-studio:r-4.5.1-1.2204.0-rstudio-server-2025.05.1-513-ubuntu22.04

ARG DEBIAN_FRONTEND=noninteractive

# --- system deps (needed mainly for sf / tmap / geobr / tigris, etc.) ---
RUN apt-get update && apt-get install -y --no-install-recommends \
    gdal-bin libgdal-dev \
    libgeos-dev \
    libproj-dev proj-data proj-bin \
    libudunits2-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

# --- install R packages ---
# (Using Posit Package Manager binaries on Ubuntu 22.04 to reduce compile pain)
RUN R -e "options(repos=c(CRAN='https://packagemanager.posit.co/cran/__linux__/jammy/latest')); \
          install.packages(c( \
            'tidyverse', \
            'dataRetrieval', \
            'heatwaveR', \
            'scales','gridExtra','cowplot','patchwork','ggpubr', \
            'tmap','tmaptools','sf','sp','leaflet','usmap', \
            'xts','stringr','RColorBrewer','rstatix', \
            'geobr','urltools','curl','tigris', \
            'openair' \
          ))"