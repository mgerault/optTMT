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
  g <- ggplot(df, aes(x, y)) +
    geom_point(size = 12, show.legend = FALSE, aes(color = d))

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
    geom_text(color = "black", show.legend = FALSE, aes(label = d_lab)) +
    theme(axis.title = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y = element_blank(),
          panel.background = element_rect(fill = "white"),
          panel.border = element_rect(color = "black", fill = NA)
    ) +
    labs(color = "", subtitle = paste0("Noise: ", noise, "%")) +
    scale_y_continuous(limits = c(0.95, 1.05),
                       breaks = c(0.95, 0.975, 1, 1.025, 1.05)) +
    coord_fixed(ratio = 25)

  return(g)
}
