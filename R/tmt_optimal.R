#' tmt_optimal
#'
#' Function to compute the optimal experimental design
#'
#' @param TMTset Character specifying the size of the TMT set. Either 10, 11, 16, 18, 32 or 35
#' @param ncond The number of unique condition
#' @param rep The number of replicate.
#'   If of length one, it assume the same number of replicates for each condition.
#'   If of the same length as the number of unique condition, it will assume a specific number of
#'   replicate for each condition.
#' @param nmix The number of carrier channel. Only produce interference to other channels.
#' @param tmt_correction A data.frame of three columns stating the amount of impurities in %
#'   of the corresponding channels. If NULL, takes the default data from optTMT.
#' @param exact Logical to compute the exact optimum. To do so, all possible permutations will be computed.
#'   Hence, it cannot be performed for a TMT set of greater size than 11.
#' @param maxiter If exact is FALSE, the maximum number of iteration of the optimization procedure.
#' @param your_design A named vector corresponding to the starting experimental design to optimize.
#'   If NULL, will start with all the unique conditions grouped together.
#'
#' @return A data.frame with all the computed design ordered according their global interference
#'
#' @export

tmt_optimal <- function(TMTset = c("10","11","16","18","32","35"),
                        ncond = 3, rep = 3, nmix = 0, tmt_correction = NULL,
                        exact = FALSE, maxiter = 10, your_design = NULL){
  TMTset <- match.arg(TMTset)
  TMTset <- as.numeric(TMTset)

  # upload tmt correction data
  if(is.null(tmt_correction)){
    tmt_correction <- tmt_interference_data[[paste(TMTset)]]
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
          dplyr::mutate(from = sub("\\)", "", sub(".*\\(", "", value)),
                        value = as.numeric(sub("%.*", "", value))) %>%
          dplyr::filter(!is.na(value))
        colnames(tmt_correction) <- c("to", "value", "Mass.Tag")
        tmt_correction <- tmt_correction[,c("Mass.Tag", "value", "to")]
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
        dplyr::mutate(from = sub("\\)", "", sub(".*\\(", "", value)),
                      value = as.numeric(sub("%.*", "", value))) %>%
        dplyr::filter(!is.na(value))
      colnames(tmt_correction) <- c("to", "value", "Mass.Tag")
      tmt_correction <- tmt_correction[,c("Mass.Tag", "value", "to")]
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

  TMTset <- length(unique(tmt_correction$Mass.Tag))
  if(length(rep) == 1){
    if(ncond*rep > TMTset){
      stop("Not enough channel ! Try to change the TMT set")
    }
  }
  else if(length(rep) == ncond){
    if(sum(rep) > TMTset){
      stop("Not enough channel ! Try to change the TMT set")
    }
  }
  else{
    message("Error: The replicates can only be one same value for all conditions or one for each")
    return()
  }

  TMTion <- list("10" = c("126", "127N", "127C", "128N", "128C", "129N", "129C", "130N", "130C", "131"),
                 "11" = c("126", "127N", "127C", "128N", "128C", "129N", "129C", "130N", "130C", "131N", "131C"),
                 "16" = c("126", "127N", "127C", "128N", "128C", "129N", "129C", "130N", "130C", "131N", "131C", "132N", "132C", "133N", "133C", "134N"),
                 "18" = c("126", "127N", "127C", "128N", "128C", "129N", "129C", "130N", "130C", "131N", "131C", "132N", "132C", "133N", "133C", "134N", "134C", "135N"),
                 "32" = c("126", "127N", "127C", "128N", "128C", "129N", "129C", "130N", "130C", "131N", "131C", "132N", "132C", "133N", "133C", "134N",
                          "127D", "128ND", "128CD", "129ND", "129CD", "130ND", "130CD", "131ND", "131CD", "132ND", "132CD", "133ND", "133CD", "134ND", "134CD", "135ND"),
                 "35" = c("126", "127N", "127C", "128N", "128C", "129N", "129C", "130N", "130C", "131N", "131C", "132N", "132C", "133N", "133C", "134N", "134C", "135N",
                          "127D", "128ND", "128CD", "129ND", "129CD", "130ND", "130CD", "131ND", "131CD", "132ND", "132CD", "133ND", "133CD", "134ND", "134CD", "135ND", "135CD")
  )

  emp <- NULL
  nemp <- 0
  if(length(rep) == 1){
    if(rep*ncond + nmix < TMTset){
      nemp <- TMTset - (rep*ncond + nmix)
      emp <- rep(0, nemp)
    }
  }
  else if(length(rep) == ncond){
    if(sum(rep) + nmix < TMTset){
      nemp <- TMTset - (sum(rep) + nmix)
      emp <- rep(0, nemp)
    }
  }

  mix <- NULL
  if(nmix > 0){
    mix <- rep(ncond + 1, nmix)
  }

  message("Computing...")
  if(exact){
    message("Getting all permutations...")

    if(length(rep) == 1){
      d <- c(rep(1:ncond, each = rep), emp, mix)
    }
    else if(length(rep) == ncond){
      d <- c(unlist(mapply(rep, 1:ncond, rep, SIMPLIFY = FALSE)), emp, mix)
    }

    nbperm <- multicool::multinom(d)
    if(nbperm > 10000){
      if(nbperm > 50000){
        message(paste0("Error: The number of possible permutations for your design is ",
                       nbperm, ", computing all of them would take too much time.",
                       "Set 'exact' to FALSE to use the neighbors method.")
        )
        return(paste0("Warning: The number of possible permutations for your design is ",
                      nbperm, ", it might take some time. Consider using the neighbors method instead."))
      }
      message()
    }

    d <- multicool::allPerm(multicool::initMC(d))
    if(length(emp)){
      d[d == 0] <- NA
    }
    if(length(mix)){
      d[d == ncond + 1] <- "Mix"
    }
    d <- as.data.frame(d)
    colnames(d) <- TMTion[[paste(TMTset)]]

    message(paste("Computing noise from", nrow(d), "different design"))
    d$noise <- apply(d, 1, tmt_interference_noise, tmt_correction) # get noise generated by each possible design
    d <- d[order(d$noise, decreasing = TRUE),]

    message("Done !")
    return(d)
  }
  else{
    if(!is.null(your_design)){
      design <- unname(your_design)
    }
    else{
      if(length(rep) == 1){
        design <- c(rep(1:ncond, each = rep), emp, mix)
      }
      else if(length(rep) == ncond){
        design <- c(unlist(mapply(rep, 1:ncond, rep, SIMPLIFY = FALSE)), emp, mix)
      }
    }
    if(length(emp)){
      design[design == 0] <- NA
    }
    if(length(mix)){
      design[design == ncond + 1] <- "Mix"
    }
    names(design) <- TMTion[[paste(TMTset)]]

    minnoise_best <- tmt_interference_noise(design, tmt_correction)
    minnoise_prev <- minnoise_best
    all_design <- data.frame(t(design), noise = minnoise_best, check.names = FALSE)
    n = 1
    no_change = 0
    while(xor(n <= maxiter, minnoise_best == 0)){
      if(no_change == 1){
        no_change <- 0
        n <- n + 1
        design <- sample(unname(design))
        names(design) <- TMTion[[paste(TMTset)]]
        minnoise_prev <- tmt_interference_noise(design, tmt_correction)
        if(length(emp)){
          design[is.na(design)] <- 0
        }
      }

      if(any(is.na(design))){
          design[is.na(design)] <- 0
      }

      d_neigh <- neighbors(design)
      if(length(emp)){
        d_neigh <- as.data.frame(apply(d_neigh, 2,
                                       function(x){
                                         if(any(x == 0)){
                                           x[which(x == 0)] <- NA
                                         };
                                         x
                                       })
        )
      }
      d_neigh$noise <- apply(d_neigh, 1, tmt_interference_noise, tmt_correction)
      minnoise <- min(d_neigh$noise)

      if(minnoise < minnoise_prev){
        design <- unlist(c(d_neigh[which.min(d_neigh$noise),]))[-(TMTset+1)]
        minnoise_prev <- minnoise
        no_change = 0
      }
      else{
        no_change = 1
      }

      if(minnoise_prev < minnoise_best){
        minnoise_best <- minnoise_prev
        all_design <- as.data.frame(rbind(all_design, data.frame(t(design), noise = minnoise_best, check.names = FALSE)))
        no_change = 0
      }
    }

    message("Done !")
    return(all_design)
  }
}


### Swap two pposition in a vector
swapPositions <- function(l, pos1, pos2){
  l_pos1 = l[pos1]
  l_pos2 = l[pos2]
  l[pos1] = l_pos2
  l[pos2] = l_pos1
  return(l)
}

### get the next permutation of design
nextPermutation <- function(x){
  n = length(x)
  i = 0
  j = 0

  # Find for the pivot element.
  # A pivot is the first element from end of sequence which doesn't follow
  # property of non-increasing suffix
  for(i in (n-1):1){
    if(x[i] < x[i + 1])
      break
  }

  # Check if pivot is not found
  if (i < 0)
    x <- x[length(x):1]

  # if pivot is found
  else{
    # Find for the successor of pivot in suffix
    for(j in n:i){
      if (x[j] > x[i])
        break
    }

    # Swap the pivot and successor
    x = swapPositions(x, i, j)

    # Minimise the suffix part, initializing range
    strt = i+1
    end = length(x)

    x[strt:end] = x[strt:end][length(strt:end):1]
  }
  return(x)
}

### compute the unique neighbors of design
neighbors <- function(l){
  neigh <- matrix(nrow = 0, ncol = length(l), dimnames = list(NULL, names(l)))
  n <- length(l)
  for(i in 1:n){
    for(j in i:n){
      if(i != j & l[i] != l[j]){
        neigh <- rbind(neigh, swapPositions(l, i, j))
      }
    }
  }
  neigh <- as.data.frame(neigh)
  return(neigh)
}
