#' read_tmtinterference
#'
#' Function to read the TMT reporter ion interference value from a file
#'
#' @param f A path to a file containing the TMT reporter ion interference value.
#'   Can be an xlsx, csv, txt tab delimited file or even the pdf product sheet.
#'
#' @return A data.frame containing the TMT reporter ion interference value
#'
#' @export

read_tmtinterference <- function(f){
  if(!file.exists(f)){
    message(paste("Error: Your file", f, "does not exist. Check your spelling"))
    return()
  }

  format <- sub(".*\\.", "", f)

  if(format == "pdf"){
    df <- tabulapdf::extract_tables(f, method = "stream", output = "tibble")
    df <- df[[1]]
    df <- df[,which(apply(df, 2, function(x) any(grepl("^TMT|(?=\\().*(?=\\))", x, perl = TRUE))))]

    nbc <- sum(grepl("TMT", df[[1]]))
    if(ncol(df) != 3){
      tokeep <- apply(df[,-1], 2, function(x) sum(grepl("(?=\\().*(?=\\))", x, perl = TRUE)) >= nbc - 2)
      if(all(!tokeep)){
        tokeep <- apply(df[,-1], 2, function(x) sum(grepl("(?=\\().*(?=\\))", x, perl = TRUE)))
        tokeep <- which(tokeep == max(tokeep))
      }
      else{
        tokeep <- which(tokeep)
      }
      df <- df[,c(1, tokeep+1)]
    }

    if(any(grepl("TMTpro", df[[1]]))){ # for (in theory) any TMTpro; i.e. 16 and 18
      df <- df[-c(1:(grep("TMT", df[[1]])[1] - 1)),]
      colnames(df) <- c("Mass.Tag", "v1", "v2")
      df$Mass.Tag <- sub(".*-", "", df$Mass.Tag)
      df <- as.data.frame(cbind(df[2*(1:(nrow(df)%/%2)) - 1,], df[2*(1:(nrow(df)%/%2)),]))


      torm <- which(apply(df, 2, function(x) all(is.na(x))))
      if(length(torm)){
        df <- df[,-torm]
      }

      df <- data.frame("Mass.Tag" = df$Mass.Tag,
                       "-1" = apply(df[,grep("^v1", colnames(df))], 1,
                                    function(x){
                                      if(any(is.na(x))){
                                        NA
                                      }
                                      else{
                                        paste(x, collapse = " ")
                                      }
                                    }),
                       "+1" = apply(df[,grep("^v2", colnames(df))], 1,
                                    function(x){
                                      if(any(is.na(x))){
                                        NA
                                      }
                                      else{
                                        paste(x, collapse = " ")
                                      }
                                    }),
                       check.names = FALSE)
    }
    else{ # for TMT10 (TMT10 has exposant) --> will need to try for TMT11
      df <- df[-grep("Mass", df[[1]]),]
      colnames(df) <- c("Mass.Tag", "v1", "v2")
      df$Mass.Tag <- sub(".*-", "", df$Mass.Tag)

      # remove uninformative rows (if any)
      torm <- apply(df, 1, function(x) !grepl("^\\d{3}", x[1]) & all(is.na(x[2:3])))
      if(any(torm)){
        df <- df[-which(torm),]
      }

      df <- lapply(1:nbc, function(x){
        idx <- grep("^\\d{3}", df$Mass.Tag)
        if(idx[1] != 1){
          idx <- idx - idx[1] + 1
        }

        if(x != nbc){
          idx <- idx[x]:(idx[x+1]-1)
        }
        else{
          idx <- idx[x]:nrow(df)
        }

        x <- df[idx,]

        data.frame("Mass.Tag" = grep("^\\d{3}", x[[1]], value = TRUE),
                   "-1" = paste(na.omit(x[[2]]), collapse = " "),
                   "+1" = paste(na.omit(x[[3]]), collapse = " "),
                   check.names = FALSE)
      })

      df <- as.data.frame(Reduce(rbind, df))
    }
  }
  else{
    if(format == "xlsx"){
      df <- openxlsx::read.xlsx(f)
    }
    else if(format == "csv"){
      df <- readr::read_csv(f, show_col_types = FALSE)
    }
    else if(format == "txt"){
      df <- readr::read_tsv(f, show_col_types = FALSE)
    }
    else{
      message(paste("Error: The format", format, "is not supported yet"))
      return()
    }

    mass_tag <- which(apply(df, 2, function(x) all(grepl("1(2|3)\\d{1}($|N$|C$|D$)", x))))
    if(length(mass_tag) != 1){
      message("Error: Only one column should contain the mass tag ! Check your file")
      return()
    }
    mass_tag <- df[[mass_tag]]
    mass_tag <- gsub("[^0-9(C|N|D|)]", "", mass_tag)

    noise <- which(apply(df, 2, function(x) all(grepl("^\\d{1,2}(\\.\\d{1,}|)(%|)( |$)", na.omit(x)))))
    if(length(noise) != 2){
      message("Error: Only two columns should contain the ion interferences, the -1 and the +1 ! Check your file")
      return()
    }

    noise_1 <- sapply(noise, function(x) sum(grepl(" .*126.*|  .*127D.*", df[[x]])) == 1)
    if(sum(noise_1) == 1){
      df <- data.frame("Mass.Tag" = mass_tag,
                       "-1" = df[[noise[which(noise_1)]]],
                       "+1" = df[[noise[which(!noise_1)]]],
                       check.names = FALSE)
    }
    else{
      message(paste0("Warning: The columns containing the interference didn't say which channel is affected by which.\n",
                     "Hence the first one will be considered as the -1 and the second as the +1."))
      df <- data.frame("Mass.Tag" = mass_tag,
                       "-1" = df[[noise[1]]],
                       "+1" = df[[noise[2]]],
                       check.names = FALSE)
    }
  }

  if(!(nrow(df) %in% c(10, 11, 16, 18, 32, 35))){
    message(paste("Error: Number of TMT channels can only be 10, 11, 16 or 18; not", nrow(df)))
    return()
  }

  ### checking interference assignment
  if(!all(is.na(df[[2]][1:2]))){
    df[[2]][1:2] <- NA
  }
  if(!all(is.na(df[[3]][(nrow(df) - 1):nrow(df)]))){
    df[[3]][(nrow(df) - 1):nrow(df)] <- NA
  }

  df[,2:3] <- t(apply(df[,2:3], 1,
                      function(x){
                        chidx <- which((df[[2]] == x[[1]] | is.na(df[[2]]) & is.na(x[[1]])) &
                                         (df[[3]] == x[[2]] | is.na(df[[3]]) & is.na(x[[2]])))
                        ch <- c(-2, 2) + chidx
                        ch <- ifelse(ch < 1 | ch > nrow(df), NA, ch)
                        ch <- df$Mass.Tag[ch]

                        xch <- gsub(".*\\(|\\)", "", x)

                        if(any(grepl("%", xch))){
                          message(paste("The channel", df$Mass.Tag[chidx],
                                        "didn't have any assigned channel and has been assigned automatically."))
                          x <- gsub(" \\(.*\\)", "", x)
                          x[which(!is.na(x))] <- paste(x[which(!is.na(x))], paste0("(", na.omit(ch), ")"))
                          x
                        }
                        else{
                          if(!all(ch == xch | is.na(ch) & is.na(xch))){
                            message(paste0("Check the channel ", df$Mass.Tag[chidx],
                                           " in your file; the interference was wrongly assigned"))
                            mapply(function(err, truth, org){
                              sub(err, truth, org)
                            },
                            xch, ch, x)
                          }
                          else{
                            x
                          }
                        }
                      }))

  TMTion <- list("10" = c("126", "127N", "127C", "128N", "128C", "129N", "129C", "130N", "130C", "131"),
                 "11" = c("126", "127N", "127C", "128N", "128C", "129N", "129C", "130N", "130C", "131N", "131C"),
                 "16" = c("126", "127N", "127C", "128N", "128C", "129N", "129C", "130N", "130C", "131N", "131C", "132N", "132C", "133N", "133C", "134N"),
                 "18" = c("126", "127N", "127C", "128N", "128C", "129N", "129C", "130N", "130C", "131N", "131C", "132N", "132C", "133N", "133C", "134N", "134C", "135N"),
                 "32" = c("126", "127N", "127C", "128N", "128C", "129N", "129C", "130N", "130C", "131N", "131C", "132N", "132C", "133N", "133C", "134N",
                          "127D", "128ND", "128CD", "129ND", "129CD", "130ND", "130CD", "131ND", "131CD", "132ND", "132CD", "133ND", "133CD", "134ND", "134CD", "135ND"),
                 "35" = c("126", "127N", "127C", "128N", "128C", "129N", "129C", "130N", "130C", "131N", "131C", "132N", "132C", "133N", "133C", "134N", "134C", "135N",
                          "127D", "128ND", "128CD", "129ND", "129CD", "130ND", "130CD", "131ND", "131CD", "132ND", "132CD", "133ND", "133CD", "134ND", "134CD", "135ND", "135CD")
                 )
  if(any(!(df[[1]] %in% TMTion[[paste(nrow(df))]]))){
    mtag_missing <- TMTion[[paste(nrow(df))]][which(!(TMTion[[paste(nrow(df))]] %in% df[[1]]))]
    mtag_wrong <- df[[1]][which(!(df[[1]]) %in% TMTion[[paste(nrow(df))]])]
    mtag_pb <- data.frame("mtag_missing" = mtag_missing, "mtag_wrong" = mtag_wrong)

    for(i in 1:nrow(mtag_pb)){
      df[[1]][which(df[[1]] == mtag_pb$mtag_wrong)] <- mtag_pb$mtag_missing
      if(any(grepl(paste0("\\(", mtag_pb$mtag_wrong, "\\)"), df[[2]]))){
        df[[2]] <- gsub(paste0("\\(", mtag_pb$mtag_wrong, "\\)"),
                        paste0("(", mtag_pb$mtag_missing, ")"),
                        df[[2]])
      }
      if(any(grepl(paste0("\\(", mtag_pb$mtag_wrong, "\\)"), df[[3]]))){
        df[[3]] <- gsub(paste0("\\(", mtag_pb$mtag_wrong, "\\)"),
                        paste0("(", mtag_pb$mtag_missing, ")"),
                        df[[3]])
      }

      message(paste("The Mass.Tag mislabelled as", mtag_pb$mtag_wrong,
                    "in your data has been named", mtag_pb$mtag_missing,
                    "by default."))
    }
  }

  return(df)
}
