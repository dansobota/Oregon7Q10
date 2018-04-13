#' Oregon 7Q10
#'
#' This package contains a function returns the lowest seven-day average discharge expected once every 10 years
#' based on a continuous record of data.  The 7Q10 can reflect an annual, seasonal, or monthly
#' statistic. Methods are consistent with the EPA's DFLOW methods.  Also included is a function,
#' OWRD_stations, that links you to the Oregon Water Resources website to browse station IDs.
#' References:
#' https://www.epa.gov/waterdata/dflow
#' https://nepis.epa.gov/Exe/ZyPDF.cgi/30001JEH.PDF?Dockey=30001JEH.PD
#' http://deq1.bse.vt.edu/sifnwiki/index.php/Multiple_ways_to_calculate_7Q10
#' https://github.com/rmichie/DFLOW_R
#' @param Station_ID Oregon Water Resources Divison (http://apps.wrd.state.or.us/apps/sw/hydro_near_real_time/) Station ID number
#' @param start_date Start date, in the format of "mm/dd/yyyy", for the period of calculation
#' @param end_date End date, in the format of "mm/dd/yyyy", for the period of calculation
#' @param period The temporal scale for the 7Q10 calculation, can be "Annual" or "Custom". Default is "Annual"
#' @param custom_start If "Custom" is designated as the period, the start date (excluding year) of the custom period in the character format of "mm-dd"
#' @param custom_end If "Custom" is designated as the period, the end date (excluding year) of the custom period in the character format of "mm-dd"
#' @param wy_start Only applicable for the "Annual" period. The water year beginning date (excluding year). The date should be in the character format of "mm-dd". If not specified the default is "10-01"
#' @param method_type The method used to calculate 7Q10.  Options are "Distribution Free", "log Pearson Type III", or "Both"
#'
#' @keywords 7Q10
#' @keywords Flow
#' @keywords Oregon
#' @export
#' @examples Oregon7Q10(Station_ID, start_date, end_date, period = "Annual", custom_start = NA, custom_end = NA, wy_start = "10-01", method_type = "Both")

# Load required packages----

# Function to get 7Q10
#' @export
Oregon7Q10 <- function(Station_ID, start_date, end_date, period = "Annual", custom_start = NA, custom_end = NA, wy_start = "10-01", method_type = "Both") {

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
              if(grepl("^\\d{2}/\\d{2}/\\d{4}$", start_date) == F) {
                stop("Please check the format of your start date: months and days need to have two digits and year needs to have four digits")
              }

              if (exists("start_date") == F) {
                stop("Please enter a valid end_date")
              }
              if (grepl("^\\d{2}/\\d{2}/\\d{4}$", end_date) == F) {
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
                if (grepl("^\\d{2}-\\d{2}$", custom_start) == F | grepl("^\\d{2}-\\d{2}$", custom_end) == F) {
                  stop("Please enter a valid format for custom start and end dates in the format of 'mm-dd'")
                  }
                }
              }

              # Check see is wy_start format is valid
              if (exists("wy_start") == F) {
                stop("Please enter a valid start date for the water year in the format of 'mm-dd'")
              }
              if (grepl("^\\d{2}-\\d{2}$", wy_start) == F) {
                stop("Please enter a valid start date for the water year:  format needs to be 'mm-dd'")
              }

              # Check methodtype to make sure it is valid
              if (exists("method_type") == F) {
                stop("Please enter a type of method for calculating 7Q10")
              }

              all_methods <- c("Both", "both", "Distribution Free", "distribution free", "log Pearson Type III")

              if (any(method_type == all_methods) == F) {
                stop("Please enter a valid method type")
              }

              # Data processing
              # Scan in data from http://apps.wrd.state.or.us/apps/sw/hydro_near_real_time/
              flow.df <- read.delim(paste0("http://apps.wrd.state.or.us/apps/sw/hydro_near_real_time/hydro_download.aspx?station_nbr=",
                          Station_ID,
                          "&start_date=",
                          start_date,
                          "%2012:00:00%20AM&end_date=",
                          end_date,
                          "%2012:00:00%20AM&dataset=MDF&format=html"), sep = "\t", header = T, stringsAsFactors = F)

              # Need to drop row with "</pre>" from data.frame
              flow.df <- flow.df[! flow.df[, 1]  %in% "</pre>", ]

              # format data columns
              flow.df$record_date <- as.POSIXct(flow.df$record_date, format = "%m-%d-%Y", tz = "America/Los_Angeles")
              flow.df$mean_daily_flow_cfs <- as.numeric(flow.df$mean_daily_flow_cfs)

              # Getting the flow data frame into the subset needed for the calculation of 7Q10
              if (period == "Annual" | period == "annual") {
                flow.df <- flow.df
              }

              if (period == "Custom" | period == "custom") {
                  custom_start.nm <-as.numeric(gsub("-", "", custom_start))
                  custom_end.nm <- as.numeric(gsub("-", "", custom_end))
                  if (custom_start.nm > custom_end.nm) {
                    flow.df <- subset(flow.df, as.integer(paste0(format.Date(flow.df$record_date, "%m"),
                                                                 format.Date(flow.df$record_date, "%d"))) >= as.integer(gsub("-", "", custom_start)) |
                                               as.integer(paste0(format.Date(flow.df$record_date, "%m"),
                                                                 format.Date(flow.df$record_date, "%d"))) <= as.integer(gsub("-", "", custom_end)))
                  } else {
                    flow.df <- subset(flow.df, as.integer(paste0(format.Date(flow.df$record_date, "%m"),
                                                                 format.Date(flow.df$record_date, "%d"))) <= as.integer(gsub("-", "", custom_start)) &
                                               as.integer(paste0(format.Date(flow.df$record_date, "%m"),
                                                          format.Date(flow.df$record_date, "%d"))) >= as.integer(gsub("-", "", custom_end)))
                  }
              }

              # Check to see if NAs for flow are present
              if (any(is.na(flow.df$mean_daily_flow_cfs) == T) == T) {
                warning(paste0("There are ", sum(is.na(flow.df$mean_daily_flow_cfs)),
                               " missing mean daily flows (NA) in the selected data"))
              }

              # Calculate the minimum of 7-day average daily flow per water year
              # First assign water year; depends on whether Annual (flexible start) vs. custom,
              # which will be assigned by the year in which the majority of the data occur

              if (period == "Custom" | period == "custom") {
                flow.df$wy.year <- ifelse(as.integer(paste0(format.Date(flow.df$record_date, "%Y"))) >
                                            as.integer(gsub("-", "", start_date)),
                                          as.integer(format.Date(flow.df$record_date, "%Y")),
                                          as.integer(format.Date(flow.df$record_date, "%Y")) - 1)
              }

              if (period =="Annual" | period == "annual") {

                if (as.integer(gsub("-", "", wy_start)) >= 621) { # Using the approximate day of the summer solstice as the hydrologic break point

                        # After the start of summer
                        flow.df$wy.year <- ifelse(as.integer(paste0(format.Date(flow.df$record_date, "%m"),
                                                                     format.Date(flow.df$record_date, "%d"))) >= as.integer(gsub("-", "", wy_start)),
                                                   as.integer(format.Date(flow.df$record_date, "%Y")) + 1,
                                                   as.integer(format.Date(flow.df$record_date, "%Y")))
                  } else {
                        # Before the start of summer
                        flow.df$wy.year <- ifelse(as.integer(paste0(format.Date(flow.df$record_date, "%m"),
                                                                    format.Date(flow.df$record_date, "%d"))) >= as.integer(gsub("-", "", wy_start)),
                                                  as.integer(format.Date(flow.df$record_date, "%Y")),
                                                  as.integer(format.Date(flow.df$record_date, "%Y")) - 1)
                  }

              }

              # Calculation of 7-day average flow for entire period
              # Note: 7-day average daily flows
              flow.df$day.7.mean <- zoo::rollapply(zoo::zoo(flow.df$mean_daily_flow_cfs), 7, mean, na.rm = F, fill = NA, align = "right")

              # Assign data frame to package environment
              assign("flow.data.frame", flow.df, envir = package:Oregon7Q10)

              # Calculation of 7Q10 with EPA methods for DFLOW----
              # Source: https://nepis.epa.gov/Exe/ZyPDF.cgi?Dockey=P100BK6P.txt

              # Distribution-free method

              # The expression for xQy is:

              # xQy = (1-e) X(ml) + eX(m2)

              # where:
              # X(m) = the m-th lowest annual low flow of record
              # ml = [(n+1)/y]
              # m2 = [(n+l)/y] + 1
              # [z] = the largest integer less than or equal to z
              # e = (n+l)/y - [(n+l)/y]
              # This method is only appropriate when the desired return period is less than n/5 years

              # First exclude incomplete years and years with >10% NAs

              # Get length of period by water year
              if (period == "Annual" | period == "annual") {
                wy.length <- 365
              } else {
                if (as.integer(gsub("-", "", custom_start) > as.integer(gsub("-", "", custom_end)))) {
                  # Using non-leap years 1998 and 1999 for intervals
                    wy.length <- as.integer(as.Date(paste0(custom_end, "-1999"), format = "%m-%d-%Y") - as.Date(paste0(custom_start, "-1998"), format = "%m-%d-%Y"))
                } else {
                   wy.length <- as.integer(as.Date(paste0(custom_start, "-1999"), format = "%m-%d-%Y") - as.Date(paste0(custom_end, "-1999"), format = "%m-%d-%Y"))
                }
              }


              # Getting the number of days in each water year queried
              flow.df %>%
                dplyr::group_by(wy.year) %>%
                  dplyr::summarise(no_rows = length(wy.year)) -> obs.wy.length

              # Getting an index to evaluate completeness of rows
              obs.wy.length$wy.ratio <- obs.wy.length$no_rows / wy.length

              # Get number of days with NA values for 7-day average flow
              flow.df %>%
                dplyr::group_by(wy.year) %>%
                  dplyr::summarise(no_rows = sum(is.na(day.7.mean))) -> obs.na.length

              # Getting percent of NAs
              obs.na.length$per_NAs <- obs.na.length$no_rows / obs.wy.length$no_rows

              # Getting final data frame and excluding water years with incomplete year coverage or >10% NAs for 7-day average flows
              list(flow.df, subset(obs.wy.length, select = -no_rows), select(obs.na.length, select= -no_rows)) %>%
               purrr::reduce(dplyr::left_join, by = "wy.year") %>%
                  subset(wy.ratio >= 1 & per_NAs <= 0.1) -> flow.df.final

              # Calculate minimum flow for each water year
              flow.df.final %>%
                dplyr::group_by(wy.year) %>%
                  dplyr::summarise(min_7Q = min(day.7.mean, na.rm = T)) -> min.flow.wy

              assign("min.flow.data.frame", min.flow.wy, envir = package:Oregon7Q10)

              # Calculation of parameters
              x <- 7
              y <- 10
              m1 <- trunc((length(min.flow.wy$min_7Q) + 1) / y)
              m2 <- trunc((length(min.flow.wy$min_7Q) + 1) / y) + 1
              e <- ((length(min.flow.wy$min_7Q) + 1) / y) - trunc((length(min.flow.wy$min_7Q) + 1) / y)

              X1 <- min.flow.wy$min_7Q[rank(min.flow.wy$min_7Q, ties.method = "first") == m1]
              X2 <- min.flow.wy$min_7Q[rank(min.flow.wy$min_7Q, ties.method = "first") == m2]

              DF7Q10 <- (1 - e) * X1 + e * X2

              # Pearson Type III method

              u <- mean(log(min.flow.wy$min_7Q))
              s <- sd(log(min.flow.wy$min_7Q))
              g <- moments::skewness(log(min.flow.wy$min_7Q))
              z <- 4.91 * ((1/y)^0.14 - (1 - 1/y)^0.14)

              K <- (2 / g) * (((1 + (g * z)/6 - (g^2)/36)^3) - 1)

              PT7Q10 <- exp(u + K * s)

              # Put values in package environment
              assign("Dist.Free.7Q10", DF7Q10, envir = package:Oregon7Q10)
              assign("Pearson.Type.III.7Q10", PT7Q10, envir = package:Oregon7Q10)

              # Print output
              if (method_type == "Both" | method_type == "both") {
                print(paste0("Distribution Free 7Q10 (cfs): ", round(DF7Q10, digits = 1)))
                print(paste0("log Pearson Type III 7Q10 (cfs): ", round(PT7Q10, digits = 1)))
              }

              if (method_type == "Distribution Free" | "distribution free") {
                print(paste0("Distribution Free 7Q10 (cfs): ", round(DF7Q10, digits = 1)))
              }

              if (method_type == "log Pearson Type III") {
                print(paste0("log Pearson Type III 7Q10 (cfs): ", round(PT7Q10, digits = 1)))
              }
}
