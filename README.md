# ASICA Workshop: Visualisation using R and ggplot2

## Pre workshop setup

Please follow the instructions below to install as much as possible
before the workshop. We will be using public wifi and cannot rely
on good internet connectivity during the workshop.

The sites below have versions for Windows and Mac. Choose appropriately.

1. Install the most recent version of R (4.2.1) from this site: [https://cran.ms.unimelb.edu.au/](https://cran.ms.unimelb.edu.au/). It looks old fashioned, but is the correct place.
* There are two flavours of R for the Mac - R-4.2.1-arm64.pkg for newer M1 Macs and R-4.2.1.pkg for Intel Macs
1. Install Rstudio from this site: [https://www.rstudio.com/products/rstudio/download/#download](https://www.rstudio.com/products/rstudio/download/#download). You need the RStudio desktop.
1. Start RStudio and run the following command while connected to the internet:
```R
install.packages(c("tidyverse", "lemon", "gert"))
```
1. Copy this repository using the following command from inside RStudio:
```R
gert::git_clone("https://github.com/richardbeare/ASCIA_Vis_Workshop.git", "ASCIA_Vis_Workshop")
```
1. We will attempt to use a tool called "etherpads" for communication during the workshop:
[https://pad.carpentries.org/ASCIA_Vis_Workshop](https://pad.carpentries.org/ASCIA_Vis_Workshop)
1. Retrieve updates to this material using:
```R
gert::git_pull()
```
with the repository set as your current working directory.
