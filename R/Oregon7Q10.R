#' Oregon 7Q10
#'
#' This function returns the lowest seven-day average discharge expected once every 10 years
#' based on a continuous record of data.  The 7Q10 can reflect an annual, seasonal, or monthly
#' statistic.
#' @param station Oregon Water Resources Divison (http://apps.wrd.state.or.us/apps/sw/hydro_near_real_time/) Station ID number
#' @param start Start date, in the format of "mm/dd/yyyy", for the period of calculation
#' @param end End date, in the format of "mm/dd/yyyy", for the period of calculation
#' @param period The temporal scale for the 7Q10 calculation, can be "A" for annual, "Spring" for 21 March to 20 June, "Summer" for 21 June to 20 September, "Fall" for 21 September to 20 December, "Winter" for 21 December to 20 March, or the specific month.
#' @keywords 7Q10
#' @keywords Flow
#' @export
#' @examples
#' Oregon7Q10()

# Load required packages----
require(PearsonDS)
require(tidyverse)

Oregon7Q10 <- function(station, start, end, period) {
              # Scan in data from http://apps.wrd.state.or.us/apps/sw/hydro_near_real_time/
              flow <- scan(paste0("http://apps.wrd.state.or.us/apps/sw/hydro_near_real_time/hydro_download.aspx?station_nbr=",
                                   station,
                                  "&start_date=",
                                   start,
                                  "%2012:00:00%20AM&end_date=",
                                   end,
                                  "%2012:00:00%20AM&dataset=MDF&format=html"),
                           what = "character", sep = "\n")

              #Get the line with the header information
              header.ln <- grep("station_nbr", flow)

              # Column header names, split on "\t", create char vect, then get rid of spaces
              header.nm<-gsub("(^+ )|( +$)", "", do.call(cbind,strsplit(UY.flow[header.ln], split="\t")))
              header.nm<-gsub("^\\s+|\\s+$", "", header.nm)
              header.nm<-gsub("<pre>", "", header.nm)
}
