#' Oregon 7Q10
#'
#' This function returns the lowest seven-day average discharge expected once every 10 years
#' based on a continuous record of data.  The 7Q10 can reflect an annual, seasonal, or monthly
#' statistic. Values from one or all of three methods (USGS, EPA, or PearsonDS) can be returned.
#' References:
#' https://www.epa.gov/waterdata/dflow
#' https://nepis.epa.gov/Exe/ZyPDF.cgi/30001JEH.PDF?Dockey=30001JEH.PD
#' http://deq1.bse.vt.edu/sifnwiki/index.php/Multiple_ways_to_calculate_7Q10
#' https://github.com/rmichie/DFLOW_R
#' @param Station_ID Oregon Water Resources Divison (http://apps.wrd.state.or.us/apps/sw/hydro_near_real_time/) Station ID number
#' @param start_date Start date, in the format of "mm/dd/yyyy", for the period of calculation
#' @param end_date End date, in the format of "mm/dd/yyyy", for the period of calculation
#' @param period The temporal scale for the 7Q10 calculation, can be "Annual" or "Custom". Default is "Annual".
#' @param custom_start If "Custom" is designated as the period, the start date (excluding year) of the custom period in the character format of "mm-dd".
#' @param custom_end If "Custom" is designated as the period, the end date (excluding year) of the custom period in the character format of "mm-dd".
#' @param wy_start Only applicable for the "Annual" period. The water year beginning date (excluding year). The date should be in the character format of "mm-dd". If not specified the default is "10-01".
#' @param method_type Method for calculating 7Q10. Options are "USGS", "EPA", "PearsonDS", or "All".  Default is "EPA".
#'
#' @keywords 7Q10
#' @keywords Flow
#' @keywords Oregon
#' @export
#' @examples
#' Oregon7Q10()

# Load required packages----
require(PearsonDS)
require(dplyr)

Oregon7Q10 <- function(Station_ID, start_date, end_date, period = "Annual", custom_start = NA, custom_end = NA, wy_start = "10-01", method_type = "EPA") {

              # Check to see if station format is valid
              # Note: this is coarse; OWRD does not seem to have a way to output a station list in html for checking directly, which would be ideal
              if (exists("Station_ID") == F) {
                stop("Please enter a valid Station_ID")
              }

              # Define a vector of character lengths possible for a Station_ID
              Station_ID_lengths <- c(3, 4, 8, 15) # Note that this was interpreted from the OWRD website; can modify if needed
              if (any(nchar(Station_ID) == Station_ID_lengths) == F) {
                stop("Please enter a valid Station_ID")
              }

              # QA/QC check on start_date and end_date
              if (exists("start_date") == F) {
                stop("Please enter a valid start_date")
              }
              if(grepl("\\d\\d/\\d\\d/\\d\\d\\d\\d", start_date, fixed = T) == F) {
                stop("Please check the format of your start date: months and days need to have two digits and year needs to have four digits")
              }

              if (exists("start_date") == F) {
                stop("Please enter a valid end_date")
              }
              if (grepl("\\d\\d/\\d\\d/\\d\\d\\d\\d", end_date, fixed = T) == F) {
                stop("Please check the format of your start date: months and days need to have two digits and year needs to have four digits")
              }

              # Check to see if start_date is before end_date
              if (as.POSIXct(start_date, format = "%m/%d/%Y") > as.POSIXct(end_date, format = "%m/%d/%Y")) {
                stop("Please check the order of your start and end dates")
              }

              # Check to see if period is valid
              if (exists("period") == F) {
                stop("Please enter a valid period")
              }

              # First need to define all possible periods in a vector
              all_periods <- c("Annual", "annual", "Custom", "custom")

              if (any(period == all_periods) == F) {
                stop("Please enter a valid period")
              }

              # Check custom date formats
              if (period == "Custom" | period == "custom") {
                if (is.na(custom_start) == T | is.na(custom_end) == T) {
                  stop("Please enter custom start and end dates in the format of 'mm-dd'")
                  } else {
                if (grepl("\\d\\d-\\d\\d", custom_start, fixed = T) == F | grepl("\\d\\d-\\d\\d", custom_end, fixed = T) == F) {
                  stop("Please enter a valid format for custom start and end dates in the format of 'mm-dd'")
                  }
                }
              }

              # Check see is wy_start format is valid
              if (exists("wy_start") == F) {
                stop("Please enter a valid start date for the water year in the format of 'mm-dd'")
              }
              if (grepl("\\d\\d-\\d\\d", wy_start, fixed = T) == F) {
                stop("Please enter a valid start date for the water year:  format needs to be 'mm-dd'")
              }

              # Check methodtype to make sure it is valid
              # First define methods
              method_types <- c("USGS", "EPA", "PearsonDS", "All")

              if (exists("method_type") == F) {
                stop("Please enter a type of method for calculating 7Q10")
              }

              if (any(method_type == method_types) == F) {
                stop("Please enter a valid method type for calculating 7Q10")
              }

              # Data processing
              # Scan in data from http://apps.wrd.state.or.us/apps/sw/hydro_near_real_time/
              flow.df <- read.table(paste0("http://apps.wrd.state.or.us/apps/sw/hydro_near_real_time/hydro_download.aspx?station_nbr=",
                          Station_ID,
                          "&start_date=",
                          start_date,
                          "%2012:00:00%20AM&end_date=",
                          end_date,
                          "%2012:00:00%20AM&dataset=MDF&format=html"), header = T, fill = T, stringsAsFactors = F)

              # Need to drop row with "</pre>" from data.frame
              flow.df <- flow.df[! flow.df[, 1]  %in% "</pre>", ]

              # format data columns
              flow.df$record_date <- as.POSIXct(flow.df$record_date, format = "%m-%d-%Y", tz = "America/Los_Angeles")
              flow.df$mean_daily_flow_cfs <- as.numeric(flow.df$mean_daily_flow_cfs)

              # Getting the flow data frame into subset needed for the calculation of 7Q10
              if (period == "Annual" | period == "annual") {
                flow.df <- flow.df
              }
              if (period == "Custom" | period == "custom") {
                  custom_start.nm <-as.numeric(gsub("-", "", custom_start))
                  custom_end.nm <- as.numeric(gsub("-", "", custom_end))
                  if (custom_start.nm < custom_end.nm) {
                    flow.df <- subset(flow.df, as.numeric(paste0(format.Date(flow.df$record_date, "%m"),
                                                                 format.Date(flow.df$record_date, "%d"))) >= as.numeric(gsub("-", "", custom_start)) &
                                               as.numeric(paste0(format.Date(flow.df$record_date, "%m"),
                                                                 format.Date(flow.df$record_date, "%d"))) <= as.numeric(gsub("-", "", custom_end)))


                  }

                }
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
