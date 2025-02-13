#' tmt_interference_links
#'
#' Function to compute which channel will generate interference in your experimental design
#'
#' @param design A named vector stating the which sample is assigned to which TMT channel.
#' @param tmt_correction A data.frame of three columns stating the amount of impurities in %
#'   of the corresponding channels. If NULL, takes the default data from optTMT.
#'
#' @return A data.frame of two columns stating which channels interfere
#'
#' @export

tmt_interference_links <- function(design, tmt_correction = NULL){
  if(is.null(tmt_correction)){
    tmt_correction <- tmt_interference_data[[paste(length(design))]]
  }
  else if(is.character(tmt_correction)){
    if(grepl("\\.(xlsx|txt|pdf|csv)$", tmt_correction)){
      tmt_correction <- read_tmtinterference(tmt_correction)
      if(is.null(tmt_correction)){
        message("Error: TMT interference data file returned NULL when loading ! Check your file")
        return()
      }
      else{
        tmt_correction <- tmt_correction %>%
          tidyr::gather("rm", "value", -Mass.Tag) %>%
          dplyr::select(-rm) %>%
          dplyr::mutate(to = sub("\\)", "", sub(".*\\(", "", value)),
                        value = as.numeric(sub("%.*", "", value))) %>%
          dplyr::filter(!is.na(value))
      }
    }
    else{
      message("Error: Your TMT interference data file should be either an xlsx, csv, txt or pdf file")
      return()
    }
  }
  else if("data.frame" %in% class(tmt_correction)){
    if(ncol(tmt_correction) != 3){
      message("Error: The TMT interference data should only contain 3 columns")
      return()
    }
    else if(!("Mass.Tag" %in% colnames(tmt_correction))){
      message("Error: The TMT interference data should contain the column 'Mass.Tag'")
      return()
    }
    else if(all(c("+1", "-1") %in% colnames(tmt_correction))){
      tmt_correction <- tmt_correction %>%
        tidyr::gather("rm", "value", -Mass.Tag) %>%
        dplyr::select(-rm) %>%
        dplyr::mutate(to = sub("\\)", "", sub(".*\\(", "", value)),
                      value = as.numeric(sub("%.*", "", value))) %>%
        dplyr::filter(!is.na(value))
    }
    else if(!all(c("value", "to") %in% colnames(tmt_correction))){
      message("Error: The TMT interference data should either contain the column 'Mass.Tag', 'value' and 'to' or 'Mass.Tag', '-1' and '+1'")
      return()
    }
  }
  else{
    message("Error: The TMT interference data can only be a data.frame, a file or NULL")
    return()
  }

  if(!all(names(design) %in% unique(tmt_correction$Mass.Tag))){
    message("Error: The 'Mass.Tag' from TMT interference data doesn't fit with the design provided.")
    return()
  }

  interf_global <- mapply(function(y,n){
    interf <- tmt_correction[which(tmt_correction$Mass.Tag == n),2:3]
    interf_val <- 0
    interf_tores <- NULL
    if(!is.na(y)){ # if empty channel, no noise
      interf_to <- design[interf$to] # which channel will be impacted
      if(any(is.na(interf_to))){ # if empty channel, no noise
        interf_to <- interf_to[-which(is.na(interf_to))] # which channel will be impacted
      }
      if(!all(interf_to == y)){ # if same channel, not impacted
        if(any(grepl("^[m|M]i[x$|x ]", interf_to))){ # if mix channel, it interfere with other but doesn't get impact since we remove it
          interf_to <- interf_to[-grep("^[m|M]i[x$|x ]", interf_to)]
        }
        if(length(interf_to)){
          interf_tores <- names(interf_to)[which(interf_to != y)] # which channel will be impacted
        }
      }
    };
    interf_tores
  },
  design, names(design))
  interf_global <- interf_global[!unlist(lapply(interf_global, function(x) !as.logical(length(x))))]

  if(length(interf_global)){
    interf_global <- mapply(function(y, n){
      data.frame(from = n, to = y)
    },
    interf_global, names(interf_global), SIMPLIFY = FALSE)
    interf_global <- as.data.frame(Reduce(rbind, interf_global))
  }
  else{
    interf_global <- NULL
  }

  return(interf_global)
}
