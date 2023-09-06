# Print out a list of apt-get packages to install to the Dockerfile
# that are needed to install R packages.
# {vetiver} is only used for settting up Dockerfile, so ignored by renv

vetiver:::glue_sys_reqs(renv::dependencies()$Package)
