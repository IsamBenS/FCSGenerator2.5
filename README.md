# FCSGenerator2.5
FCS Generator is a tool dedicated used to generate "ground truth" FCS files for benchmarking dimension reduction or clustering algorithms. FCSGenerator allows creating highly customizable FCS files from scratch or by use of clustered cytometry data.
 
	
## Requirements
  * software: R(Version 3.5.1), Rstudio(optional)
  * R packages: shiny, shinydashboard, shinyjs, flowCore, ggplot2, d3r, reshape2, ggridges, truncnorm, ncdfFlow, ggcyto, sunburstR, heatmaply, shinyHeatmaply, tcltk
  
## Quick installation guide

  1. Run the following commands in R/RStudio:
```
source("https://bioconductor.org/biocLite.R")
biocLite("Biobase")
biocLite("flowCore")
biocLite("ncdfFlow")
biocLite("ggcyto")
install.packages(c("shiny", "shinyjs", "shinydashboard", "ggplot2", "d3r", "reshape2", "ggridges", "truncnorm", "sunburstR", "heatmaply", "shinyHeatmaply", "tcltk"))

```
  >You may be asked to reload your environment, if so, accept.
  
  2. Then next commands:
```
library("devtools")
install_github("isambens/fcsgenerator2.5")
```

  
## Launching the shiny application

  Run the following commands in R/RStudio:
```
library("FCSGenerator2")
FCSGenerator2.run()
```