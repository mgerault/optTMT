#' @title Run the app for optimizing your TMT experimental design
#'
#' @description
#'  \code{runoptTMT} works exactly the same as runExample from \code{\link{shiny}} package.
#'
#' @export

runoptTMT <- function() {
  appDir <- system.file("shiny-examples", "myapp", package = "optTMT")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `optTMT`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}

### function to normalize node size in visnetwork --> only used for the Shiny Application
# both captials --> + 1 letter = + 6
# both minors --> + 1 letter = + 2
# capitals to minor = + 2
# one minor in plus compare to capitals = + 2
# one capital in plus compare to minors = + 4
norm_font_size <- function(s, ref_font_size = 16){
  font_size <- rep(NA, length(s))

  sl <- nchar(s)
  sl_diff <- max(sl) - sl

  n_cap_minor <- t(sapply(s,
                          function(x){
                            x <- strsplit(x, "")[[1]]
                            n_cap <- sum(grepl("[[:upper:]]", x))
                            n_minor <- length(x) - n_cap;
                            c("n_cap" = n_cap, "n_minor" = n_minor)
                          })
  )
  if(all(sl_diff == 0)){
    same_n_caps <- apply(n_cap_minor, 2, var)
    if(all(same_n_caps == 0)){
      # no change
      font_size <- rep(ref_font_size, length(s))
    }
    else{
      # capitals to minor = + 2
      font_size <- ref_font_size + 2*(max(n_cap_minor[,"n_cap"]) - n_cap_minor[,"n_cap"])
    }
  }
  else{
    n_cap_minor <- t(apply(n_cap_minor, 1, function(x) n_cap_minor[which.max(sl),] - x))
    font_size <- ref_font_size + n_cap_minor[,1]*4 + n_cap_minor[,2]*2
  }
  return(font_size)
}

