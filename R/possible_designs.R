#' possible_designs
#'
#' Function to get up to 50 equivalent designs producing the same global interference
#'
#' @param design A named vector stating the which sample is assigned to which TMT channel.
#' @param computing_all Logical to tell if you to compute all possible equivalent design.
#'   If FALSE, up to 50 designs will be kept as the number of possible design can quickly
#'   explode with higher number of conditions and bigger TMT set. Default is FALSE.
#'
#' @return A data.frame containing all or up to 50 equivalent designs
#'
#' @export

possible_designs <- function(design, computing_all = FALSE){
  # getting permutations
  conditions <- unique(design)
  if(any("NA" %in% conditions)){
    conditions <- conditions[-which(conditions == "NA")]
  }
  if(any(is.na(conditions))){
    conditions <- na.omit(conditions)
  }
  if(any(grepl("^Mix", conditions))){
    conditions <- conditions[-grep("^Mix", conditions)]
  }
  conditions <- sort(conditions)
  conditions_perm <- multicool::allPerm(multicool::initMC(conditions))

  # reducing number of designs
  if(nrow(conditions_perm) > 50 & !computing_all){
    conditions_perm <- conditions_perm[floor(seq(1, nrow(conditions_perm), length.out = 50)),]
  }

  alldesigns <- apply(conditions_perm, 1,
                      function(x){
                        newdes <- rep(NA, length(design))
                        for(i in conditions){
                          newdes[which(design == i)] <- x[which(conditions == i)]
                        }
                        if(any(grepl("^Mix", design))){
                          newdes[grep("^Mix", design)] <- grep("^Mix", design, value = TRUE)
                        }
                        newdes
                      })

  # shaping
  alldesigns <- as.data.frame(alldesigns)
  colnames(alldesigns) <- paste0("design_", 1:ncol(alldesigns))
  alldesigns$Mass.Tag <- names(design)
  alldesigns <- alldesigns[,c(ncol(alldesigns), 1:(ncol(alldesigns) - 1))]

  return(alldesigns)
}
