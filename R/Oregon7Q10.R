#' Oregon 7Q10
#'
#' This function returns the lowest seven-day average discharge expected once every 10 years
#' based on a continuous record of data.  The 7Q10 can reflect an annual, seasonal, or monthly
#' statistic. Values from three methods are returned: USGS, EPA, and PearsonDS
#' References:
#' https://www.epa.gov/waterdata/dflow
#' https://nepis.epa.gov/Exe/ZyPDF.cgi/30001JEH.PDF?Dockey=30001JEH.PD
#' http://deq1.bse.vt.edu/sifnwiki/index.php/Multiple_ways_to_calculate_7Q10
#' @param station Oregon Water Resources Divison (http://apps.wrd.state.or.us/apps/sw/hydro_near_real_time/) Station ID number
#' @param start Start date, in the format of "mm/dd/yyyy", for the period of calculation
#' @param end End date, in the format of "mm/dd/yyyy", for the period of calculation
#' @param period The temporal scale for the 7Q10 calculation, can be "Annual", "Spring" for 21 March to 20 June, "Summer" for 21 June to 20 September, "Fall" for 21 September to 20 December, "Winter" for 21 December to 20 March, or the specific month. Default is "Annual".
#' @param wy.start Optional. The water year beginning date (excluding year). The date should be in character format "mm-dd". If not specified the default is "10-01". Only applicable for annual periods.
#' @param methodtype Method for calculating 7Q10. Options are "USGS", "EPA", "PearsonDS", or "All" for comparison purposes.  Default is "EPA".
#' @keywords 7Q10
#' @keywords Flow
#' @keywords Oregon
#' @export
#' @examples
#' Oregon7Q10()

# Load required packages----
require(PearsonDS)
require(dplyr)

Oregon7Q10 <- function(station, start, end, period = "Annual", wy.start = "10-01", methodtype = "EPA") {
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
              header.nm<-gsub("(^+ )|( +$)", "", do.call(cbind,strsplit(flow[header.ln], split="\t")))
              header.nm<-gsub("^\\s+|\\s+$", "", header.nm)
              header.nm<-gsub("<pre>", "", header.nm)

              # Grab numeric data
              num.1st <- header.ln + min(grep("^.*[0-9]",
                                              flow[header.ln + 1:length(flow)]))

              flow.data <- flow[num.1st:length(flow)]

              flow.list <- list()

              # Seperate data on at least two spaces then create data frame using rbind
              for (y in 1:length(flow.data)) {
                flow.list[[y]] <- as.data.frame(strsplit(flow.data[y], split = "\t"))
                names(flow.list[[y]]) <- y
                flow.list[[y]] <- t(flow.list[[y]])
              }

              # Set blank data frame
              flow.df <- data.frame(stringsAsFactors = F)

              # Build dataframe with rbind fill so that empty cells don't repeat
              for (z in 1:length(flow.list)) {
                flow.df <- bind_rows(flow.df, as.data.frame(flow.list[[z]],
                                                        stringsAsFactors = F))
              }

              # Give columns the appropriate names
              names(flow.df) <- header.nm

              # format data columns
              flow.df$record_date <- as.POSIXct(flow.df$record_date, format = "%m-%d-%Y", tz = "America/Los_Angeles")
              flow.df$mean_daily_flow_cfs <- as.numeric(flow.df$mean_daily_flow_cfs)

              if (period == "Annual") {
                flow.df <- flow.df
              } else {
                if (period == "")
              }

              # Assign data frame to package environment
              assign("flow.data.frame", flow.df, environ = package:Oregon7Q10)

              # Calculation of 7Q10 with USGS method----

              # Calculation of with EPA method----

              # Calculation of 7Q10 with PearsonDS method----
              # From Virgina DEQ (http://deq1.bse.vt.edu/sifnwiki/index.php/Multiple_ways_to_calculate_7Q10)
              pars <- PearsonDS:::pearsonIIIfitML(log(flow.df$mean_daily_flow_cfs[!is.na(flow.df$mean_daily_flow_cfs)]))
              x7q10 <- exp(qpearsonIII(0.1, params = pars$par)) # 7Q10
              assign("PearsonDS.7q10", x7q10, environ = package:Oregon7Q10)
              print(paste("PearsonDS Method =", x7q10))
}
