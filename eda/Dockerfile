FROM rocker/tidyverse
MAINTAINER Eric Bridgeford <ericwb95@gmail.com>

RUN apt-get install -y gsl-bin libgsl-dev vim
RUN Rscript -e "install.packages(c('tidyverse', 'ggplot2', 'mltools', 'data.table'))"
RUN Rscript -e "install.packages('rerf')"
RUN Rscript -e "require(devtools); install_github('neurodata/lol')"
