# To create this image you need to first install docker (www.docker.com)
# Then, from your command line and being in this folder, run

# docker build -t sposm_ide .

# After the docker image has been built you can run it by

# docker run -d -p 8787:8787 -e PASSWORD=??? --name sposm sposm_ide

# Do not forget to set a passord, as depending on your configuration
# the container might be accessible in the network!

# Access your new integrated development environment in your browser
# using the user 'rstudio' and your password from above:

# http://localhost:8787

# When you are done, you can stop the container instance

# docker stop sposm

# And (maybe) delete it (ATTENTION: Everything you did in the container is lost)

# docker rm sposm

# If you also want to delete the image that was created from the Dockerfile

# docker image rm sposm_ide

# Below you will find the actual Dockerfile

FROM rocker/verse:3.6.0

MAINTAINER Joachim Gassen "gassen@wiwi.hu-berlin.de" 

# Install some additional tools

RUN apt-get update \
  && apt-get install -y --no-install-recommends nano curl python3-pip lbzip2


# Install some additional tex packages

RUN tlmgr option repository http://mirror.ctan.org/systems/texlive/tlnet

RUN tlmgr install multirow xcolor colortbl wrapfig float tabu varwidth \
  threeparttable threeparttablex environ trimspaces ulem makecell endfloat \
  setspace footmisc csquotes beamer translator\
  && tlmgr path add 
  

# And some R packages

RUN  R -e "install.packages(c('kableExtra', 'ExPanDaR', 'ggmap', 'tidyr', \
  'tufte', 'showtext', 'cowplot', 'DiagrammeR', 'leaflet', 'widgetframe', \
  'zipcode', 'shiny', 'shinyjs', 'grid', 'gridExtra', 'ggwordcloud', 'tm', \
  'qrcode', repos = c(CRAN = 'https://mran.microsoft.com/snapshot/2019-09-25'))"

# Replacement repo as MRAN was flaky over the last days
# c(CRAN = 'https://cran.uni-muenster.de/')

RUN R -e "devtools::install_github('bergant/datamodelr')" 
RUN R -e "devtools::install_github('wmurphyrd/fiftystater')" 

RUN R -e "webshot::install_phantomjs()"

RUN mv /root/bin/phantomjs /usr/local/bin

RUN chown -R rstudio:staff /opt/TinyTeX