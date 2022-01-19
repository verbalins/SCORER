FROM rocker/r-ver:4.1.2
RUN apt-get update && apt-get install -y  git-core libcurl4-openssl-dev libgit2-dev libicu-dev libpng-dev libssl-dev libxml2-dev make pandoc pandoc-citeproc zlib1g-dev && rm -rf /var/lib/apt/lists/*
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl', Ncpus = 4)" >> /usr/local/lib/R/etc/Rprofile.site
RUN R -e 'install.packages("remotes")'
RUN Rscript -e 'remotes::install_version("scales",upgrade="never", version = "1.1.1")'
RUN Rscript -e 'remotes::install_version("rlang",upgrade="never", version = "0.4.12")'
RUN Rscript -e 'remotes::install_version("foreach",upgrade="never", version = "1.5.1")'
RUN Rscript -e 'remotes::install_version("htmltools",upgrade="never", version = "0.5.2")'
RUN Rscript -e 'remotes::install_version("DT",upgrade="never", version = "0.20")'
RUN Rscript -e 'remotes::install_version("processx",upgrade="never", version = "3.5.2")'
RUN Rscript -e 'remotes::install_version("attempt",upgrade="never", version = "0.3.1")'
RUN Rscript -e 'remotes::install_version("BBmisc",upgrade="never", version = "1.11")'
RUN Rscript -e 'remotes::install_version("plotly",upgrade="never", version = "4.10.0")'
RUN Rscript -e 'remotes::install_version("rpart.plot",upgrade="never", version = "3.1.0")'
RUN Rscript -e 'remotes::install_version("shiny",upgrade="never", version = "1.7.1")'
RUN Rscript -e 'remotes::install_version("testthat",upgrade="never", version = "3.1.1")'
RUN Rscript -e 'remotes::install_version("config",upgrade="never", version = "0.3.1")'
RUN Rscript -e 'remotes::install_version("Rdpack",upgrade="never", version = "2.1.3")'
RUN Rscript -e 'remotes::install_version("gtools",upgrade="never", version = "3.9.2")'
RUN Rscript -e 'remotes::install_version("partykit",upgrade="never", version = "1.2-15")'
RUN Rscript -e 'remotes::install_version("rattle",upgrade="never", version = "5.4.0")'
RUN Rscript -e 'remotes::install_version("fastcluster",upgrade="never", version = "1.2.3")'
RUN Rscript -e 'remotes::install_version("factoextra",upgrade="never", version = "1.0.7")'
RUN Rscript -e 'remotes::install_version("dbscan",upgrade="never", version = "1.1-10")'
RUN Rscript -e 'remotes::install_version("shinyjs",upgrade="never", version = "2.1.0")'
RUN Rscript -e 'remotes::install_version("shinycssloaders",upgrade="never", version = "1.0.0")'
RUN Rscript -e 'remotes::install_version("shinydashboard",upgrade="never", version = "0.7.2")'
RUN Rscript -e 'remotes::install_version("janitor",upgrade="never", version = "2.1.0")'
RUN Rscript -e 'remotes::install_version("golem",upgrade="never", version = "0.3.1")'
RUN Rscript -e 'remotes::install_version("doParallel",upgrade="never", version = "1.0.16")'
RUN Rscript -e 'remotes::install_version("tikzDevice",upgrade="never", version = "0.12.3.1")'
RUN Rscript -e 'remotes::install_version("ggtext",upgrade="never", version = "0.1.1")'
RUN Rscript -e 'remotes::install_version("ecr",upgrade="never", version = "2.1.0")'
RUN Rscript -e 'remotes::install_version("tidyverse",upgrade="never", version = "1.3.1")'
RUN mkdir /build_zone
ADD . /build_zone
WORKDIR /build_zone
RUN R -e 'remotes::install_local(upgrade="never")'
RUN rm -rf /build_zone
EXPOSE 80
CMD R -e "options('shiny.port'=80,shiny.host='0.0.0.0');SCORER::run_app()"
