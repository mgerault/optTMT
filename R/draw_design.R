#' draw_design
#'
#' Function to plot your experimental design
#'
#' @param design A named vector stating the which sample is assigned to which TMT channel.
#' @param tmt_correction A data.frame of three columns stating the amount of impurities in %
#'   of the corresponding channels. If NULL, takes the default data from optTMT.
#'
#' @return A ggplot object showing which channel produce interference in your experimental design.
#'
#' @export

draw_design <- function(design, tmt_correction = NULL){
  TMTion <- list("10" = c("126", "127N", "127C", "128N", "128C", "129N", "129C", "130N", "130C", "131"),
                 "11" = c("126", "127N", "127C", "128N", "128C", "129N", "129C", "130N", "130C", "131N", "131C"),
                 "16" = c("126", "127N", "127C", "128N", "128C", "129N", "129C", "130N", "130C", "131N", "131C", "132N", "132C", "133N", "133C", "134N"),
                 "18" = c("126", "127N", "127C", "128N", "128C", "129N", "129C", "130N", "130C", "131N", "131C", "132N", "132C", "133N", "133C", "134N", "134C", "135N"),
                 "32" = c("126", "127N", "127C", "128N", "128C", "129N", "129C", "130N", "130C", "131N", "131C", "132N", "132C", "133N", "133C", "134N",
                          "127D", "128ND", "128CD", "129ND", "129CD", "130ND", "130CD", "131ND", "131CD", "132ND", "132CD", "133ND", "133CD", "134ND", "134CD", "135ND"),
                 "35" = c("126", "127N", "127C", "128N", "128C", "129N", "129C", "130N", "130C", "131N", "131C", "132N", "132C", "133N", "133C", "134N", "134C", "135N",
                          "127D", "128ND", "128CD", "129ND", "129CD", "130ND", "130CD", "131ND", "131CD", "132ND", "132CD", "133ND", "133CD", "134ND", "134CD", "135ND", "135CD")
  )
  TMT <- TMTion[[paste(length(design))]]

  noise <- design
  names(noise) <- TMT
  noise <- tmt_interference_noise(noise, tmt_correction)

  TMT <- factor(TMT, levels = TMT)

  df <- data.frame(x = TMT, y = 1, d = as.character(design), d_lab = as.character(design))
  if(any(is.na(df$d_lab))){
    df$d_lab[which(is.na(df$d_lab))] <- "NA"
  }

  fills <- table(df$d_lab)
  cols <- c("#FF1F1F", "#1F49FF", "#26A419", "orange", "#CE50FF", "#00C07E", "#A1D102",
            "#88281D", "#11007A", "#97FF6A", "#FFA653", "#8200FF", "#186D36", "#FF849E",
            "#E9D308", "#608ABD", "#0C930C", "#CF1F14", "#CF14C6", "#67F57C", "#4904A0",
            "#FF6D6D", "#9EFFA5", "#C96B1D", "#00FF0E", "#FFEB00", "#FF0045", "#0500FF",
            "#88B00A", "#FF2700", "#00A6FF", "#BB2172", "#276C1D", "#FFD52E", "#F63E15FF")
  names(cols) <- 1:length(cols)
  fills <- sapply(names(fills),
                  function(cn){
                    if(cn == "NA"){
                      "white"
                    }
                    else if(grepl("^Ref", cn, ignore.case = TRUE)){
                      "#9D825D"
                    }
                    else if(grepl("^Mix", cn)){
                      "#EBB564"
                    }
                    else{
                      cols[[cn]]
                    }})
  ncond <- sum(names(fills) %in% LETTERS)

  g <- ggplot(df, aes(x, y)) +
    geom_point(size = 18, shape = 21, stroke = 1.25,
               show.legend = FALSE, color = "black",
               aes(fill = d_lab)) +
    scale_fill_manual(values = fills)

  df_noise <- tmt_interference_links(design, tmt_correction)
  if(!is.null(df_noise)){
    df_noise$y1 <- apply(df_noise, 1, function(y){
      y <- as.numeric(gsub("\\D", "", y))
      y <- sign(diff(y))
      if(y == 1){
        y <- 0.99
      }
      else{
        y <- 1.01
      };
      y
    })
    df_noise$y2 <- df_noise$y1
    g <- g +
      geom_curve(data = df_noise,
                 aes(x = from, y = y1, xend = to, yend = y2),
                 arrow = arrow(length = unit(0.2, "npc")),
                 linewidth = 1.5,
                 color = "#FF000091"
      )
  }

  g <- g +
    geom_text(color = "black", show.legend = FALSE, aes(label = d_lab),
              fontface = "bold", size = 8) +
    geom_text(color = "black", show.legend = FALSE, fontface = "bold", size = 8,
              data = data.frame(x = TMT, y = 0.95), aes(x, y, label = x)) +
    theme_void() +
    theme(plot.margin = margin(0,5,0,5),
          plot.title = element_text(face = "bold", size = 24),
          plot.subtitle = element_text(size = 20)) +
    labs(title = paste0("Optimal design for TMT", paste(length(design)),
                        "-plex with ", ncond, " conditions"),
         subtitle = paste0("Global Interference: ", noise, "%")) +
    scale_y_continuous(limits = c(0.95, 1.05),
                       breaks = c(0.95, 0.975, 1, 1.025, 1.05)) +
    coord_fixed(ratio = 25)

  return(g)
}
