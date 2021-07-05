# syntax=docker/dockerfile:1.2
FROM rocker/tidyverse:4.1.0
RUN apt-get update && apt-get install -y  git-core libcurl4-openssl-dev libgit2-dev libicu-dev libpng-dev libssl-dev libxml2-dev make pandoc pandoc-citeproc zlib1g-dev libtcl libxt-dev && rm -rf /var/lib/apt/lists/*
RUN echo "options(download.file.method = 'libcurl', Ncpus = 8)" >> /usr/local/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'
RUN Rscript -e 'remotes::install_version("scales",upgrade="never", version = "1.1.1")'
RUN Rscript -e 'remotes::install_version("rlang",upgrade="never", version = "0.4.11")'
RUN Rscript -e 'remotes::install_version("htmltools",upgrade="never", version = "0.5.1.1")'
RUN Rscript -e 'remotes::install_version("DT",upgrade="never", version = "0.18")'
RUN Rscript -e 'remotes::install_version("processx",upgrade="never", version = "3.5.2")'
RUN Rscript -e 'remotes::install_version("attempt",upgrade="never", version = "0.3.1")'
RUN Rscript -e 'remotes::install_version("BBmisc",upgrade="never", version = "1.11")'
RUN Rscript -e 'remotes::install_version("plotly",upgrade="never", version = "4.9.4.1")'
RUN Rscript -e 'remotes::install_version("rpart.plot",upgrade="never", version = "3.0.9")'
RUN Rscript -e 'remotes::install_version("shiny",upgrade="never", version = "1.6.0")'
RUN Rscript -e 'remotes::install_version("testthat",upgrade="never", version = "3.0.4")'
RUN Rscript -e 'remotes::install_version("config",upgrade="never", version = "0.3.1")'
RUN Rscript -e 'remotes::install_version("foreach",upgrade="never", version = "1.5.1")'
RUN Rscript -e 'remotes::install_version("spelling",upgrade="never", version = "2.2")'
RUN Rscript -e 'remotes::install_version("gtools",upgrade="never", version = "3.9.2")'
RUN Rscript -e 'remotes::install_version("partykit",upgrade="never", version = "1.2-13")'
RUN Rscript -e 'remotes::install_version("party",upgrade="never", version = "1.3-7")'
RUN Rscript -e 'remotes::install_version("rattle",upgrade="never", version = "5.4.0")'
RUN Rscript -e 'remotes::install_version("fastcluster",upgrade="never", version = "1.2.3")'
RUN Rscript -e 'remotes::install_version("factoextra",upgrade="never", version = "1.0.7")'
RUN Rscript -e 'remotes::install_version("dbscan",upgrade="never", version = "1.1-8")'
RUN Rscript -e 'remotes::install_version("shinyjs",upgrade="never", version = "2.0.0")'
RUN Rscript -e 'remotes::install_version("shinycssloaders",upgrade="never", version = "1.0.0")'
RUN Rscript -e 'remotes::install_version("shinydashboard",upgrade="never", version = "0.7.1")'
RUN Rscript -e 'remotes::install_version("golem",upgrade="never", version = "0.3.1")'
RUN Rscript -e 'remotes::install_version("Rdpack",upgrade="never", version = "2.1.2")'
RUN Rscript -e 'remotes::install_version("doParallel",upgrade="never", version = "1.0.16")'
RUN Rscript -e 'remotes::install_version("collapse",upgrade="never", version = "1.6.0")'
RUN Rscript -e 'remotes::install_version("tikzDevice",upgrade="never", version = "0.12.3.1")'
RUN Rscript -e 'remotes::install_version("ggtext",upgrade="never", version = "0.1.1")'
RUN Rscript -e 'remotes::install_version("ecr",upgrade="never", version = "2.1.0")'
RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone
RUN rm -rf .Rprofile
#RUN R -e 'install.packages("remotes")'
#RUN R -e 'remotes::install_local(upgrade="never")'
RUN R -e 'install.packages(".", repos=NULL, type="source")'
EXPOSE 8080
CMD R -e "options('shiny.port'=8080,shiny.host='0.0.0.0');SCORER::run_app()"
