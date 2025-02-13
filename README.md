# What is optTMT ?

When using TMT multiplexing in a proteomics experiment, reporter ion interference, also named as cross-label isotopic impurity can arise from manufacture level impurities and experimental error. This can lead to false positive. To prevent this, planning the right experiemental design is crucial. This package provide functions and Shiny application to find the optimal experimental design for your proteomics experiment using TMT based on your own parameters.

If you prefer not installing R and only use the Shiny application, you can download an exe file containing the optTMT Shiny application which you can use on any Windows computer ZENODO_link
You can also go to https://marc-antoinegerault.shinyapps.io/TMT_optimization/ to directly start using the optTMT app on shinyapps website. 

## How to install IMPRINTS.CETSA.app ?  
First go to Rstudio then type and run the following commands in R console:

```c
if(!requireNamespace("devtools", quietly = TRUE)){
   install.packages("devtools")
} 
devtools::install_github("mgerault/optTMT")

library(optTMT)
```

If you want to use the app just type :

```c
runoptTMT()
```
 
