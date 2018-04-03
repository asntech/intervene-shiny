# README #

**Intervene-app: is an interactive Shiny app visualization of multiple lists and genomic region sets**

### Here is a demo ###

https://asntech.shinyapps.io/intervene/


install.packages(c("shiny","shinydashboard", "devtools", "d3heatmap", "plotly", "gplots", ggplot2", "gridExtra", "plyr", "UpSetR", "colourpicker", "corrplot", "BBmisc", "readr", "DT"));

source("https://bioconductor.org/biocLite.R"); biocLite(c("RBGL","graph"))
library(devtools); install_github("js229/Vennerable");