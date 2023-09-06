FROM rocker/r-ver:4.3.1

ARG DEBIAN_FRONTEND=noninteractive

############################
### Install APT packages ###
############################

# cron for cronjobs
# get list of other deps using R/find_deps.R

RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    cron \
    libcurl4-openssl-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libfribidi-dev \
    libharfbuzz-dev \
    libicu-dev \
    libjpeg-dev \
    libpng-dev \
    libssl-dev \
    libtiff-dev \
    libxml2-dev \
    make \
    pandoc \
    zlib1g-dev \
  && apt-get clean

####################################
### Install R packages with renv ###
####################################

# Create directory for renv project library
RUN mkdir /renv

# Modify Rprofile.site so renv uses /renv for project library
RUN echo 'Sys.setenv(RENV_PATHS_LIBRARY = "/renv")' >> /usr/local/lib/R/etc/Rprofile.site

# Initialize a 'dummy' project and restore the renv library.
# Since the library path is specified as above, the library will be restored to /renv
RUN mkdir /tmp/project

COPY ./renv.lock /tmp/project

WORKDIR /tmp/project

# Restore, but don't use cache
RUN Rscript -e 'install.packages("renv"); renv::consent(provided = TRUE); renv::settings$use.cache(FALSE); renv::init(bare = TRUE); renv::restore()'

############
### Cron ###
############

# cron is used to run R/digest.R automatically once per week

RUN apt-get -y install cron

# Write script to launch R/digest.R from /wd/
RUN echo "#!/bin/bash" >> /home/digest.sh && \
  echo "cd /wd" >> /home/digest.sh && \
  echo "/usr/local/bin/Rscript /wd/R/digest.R" >> /home/digest.sh && \
  chmod 0644 /home/digest.sh

# Create the log file to be able to run tail
RUN touch /var/log/cron.log

# Setup cron job: Run at 12:00am on Monday
RUN (crontab -l ; echo "0 0 * * 1 bash /home/digest.sh >> /var/log/cron.log 2>&1") | crontab

# To run the cron job, provide the command `cron` to `docker run`:
# docker run --rm -dt -v ${PWD}:/wd -w /wd --name digest_cron --user root joelnitta/ppg-voting:latest cron -f
# 
# as long as the container is up, it will run the job once per week

############################
### Set up non-root user ###
############################

ARG USERNAME=ppg
ARG USER_UID=1000
ARG USER_GID=$USER_UID

# Create the user
RUN groupadd --gid $USER_GID $USERNAME \
    && useradd --uid $USER_UID --gid $USER_GID -m $USERNAME \
    # [Optional] Add sudo support. Omit if you don't need to install software
    # in continer.
    && apt-get update \
    && apt-get install -y sudo \
    && echo $USERNAME ALL=\(root\) NOPASSWD:ALL > /etc/sudoers.d/$USERNAME \
    && chmod 0440 /etc/sudoers.d/$USERNAME

# Set the default user. Omit if you want to keep the default as root.
USER $USERNAME

WORKDIR /home/