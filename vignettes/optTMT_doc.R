## ----eval=FALSE---------------------------------------------------------------
# if(!requireNamespace("devtools", quietly = TRUE)){ #check if you already have the devtools package
#  install.packages("devtools")  #if not, install it
# }
# devtools::install_github("mgerault/optTMT")

## ----warning=FALSE, message=FALSE---------------------------------------------
library(optTMT)

## ----warning=FALSE------------------------------------------------------------
# first experiment
opt_tmt10 <- tmt_optimal(TMTset = "10", ncond = 3, rep = 3, nmix = 1)

# second experiment
opt_tmt16 <- tmt_optimal(TMTset = "16", ncond = 6, rep = c(rep(3,5), 1))

## ----eval=FALSE---------------------------------------------------------------
# myTMT_interference <- read_tmtinterference("path_to_TMT_product_data_sheet.pdf")

## ----warning=FALSE------------------------------------------------------------
# TMT 10 plex, 3 conditions in triplicates
our_first_design <- c(rep(1:3,3), "Mix")
names(our_first_design) <- colnames(opt_tmt10)[1:10]

paste0("The global interference for this design is ", tmt_interference_noise(our_first_design), "%")
print("The channel affected are:")
tmt_interference_links(our_first_design)

## ----warning=FALSE------------------------------------------------------------
# TMT 10 plex, 3 conditions in triplicates
optimal_design <- opt_tmt10[nrow(opt_tmt10),-11]
optimal_design <- unlist(as.list(optimal_design)) # convert data.frame of one row in a named vector

paste0("The global interference for the optimal design is ", tmt_interference_noise(optimal_design), "%")
print("The channel affected are:")
tmt_interference_links(optimal_design)

## ----warning=FALSE, fig.width=12, fig.height=4--------------------------------
draw_design(optimal_design)

## ----warning=FALSE------------------------------------------------------------
all_my_designs <- possible_designs(optimal_design)
all_my_designs

## ----eval=FALSE---------------------------------------------------------------
# openxlsx::write.xlsx(all_my_designs, "Optimal_designs.xlsx")

## ----eval=FALSE---------------------------------------------------------------
# runoptTMT()  # this function will directly start the app

