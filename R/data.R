#' TMT report ion interference value
#'
#' The TMT report ion interference value for all TMT set possible (10, 11, 16, 18, 32, 35)
#'
#' @format A list of 6 data.frame corresponding to the TMT report ion interference value for the TMT set 10, 11, 16, 18, 32, 35. Each data.frame contains 3 columns:
#' \describe{
#'   \item{Mass.tag}{TMT Mass.Tag}
#'   \item{value}{Amount of impurites in percentage}
#'   \item{to}{From which channel the impuritie is}
#' }
#'
#' @source The data for the TMT10-plex correspond to the product number 90110, lot number ZL401547;
#'   for the TMT11-plex correspond to the product number A37725, lot number ZJ396353;
#'   for the TMT16-plex correspond to the product number A44522, lot number AA397559;
#'   for the TMT18-plex correspond to the same data given for TMT16-plex plus data fro the product number A52048, lot number AA401375;
#'   for the TMT32-plex correspond to the same data given for TMT16-plex plus product number A40000817, lot number ZL403757;
#'   for the TMT35-plex correspond to the same data given for TMT18-plex plus product number A40000817, lot number ZL403757 and product number A40000853, lot number ZJ407036.
"tmt_interference_data"

