#' possible_designs
#'
#' Function to get all equivalent design producing the same global interference
#'
#' @param design A named vector stating the which sample is assigned to which TMT channel.
#'
#' @return A data.frame containing all equivalent designs
#'
#' @export

possible_designs <- function(design){
  # getting permuations
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
