# Oregon7Q10
Functions to calculate 7Q10 statistics in Oregon

This package contains a function returns the lowest seven-day average discharge expected once every 10 years based on a continuous record of data.  The 7Q10 can reflect an annual, seasonal, or monthly statistic. Methods are consistent with the EPA's DFLOW methods.

References:

https://www.epa.gov/waterdata/dflow

https://nepis.epa.gov/Exe/ZyPDF.cgi/30001JEH.PDF?Dockey=30001JEH.PD

http://deq1.bse.vt.edu/sifnwiki/index.php/Multiple_ways_to_calculate_7Q10

https://github.com/rmichie/DFLOW_R

Parameters:

Station_ID: Oregon Water Resources Divison (http://apps.wrd.state.or.us/apps/sw/hydro_near_real_time/) Station ID number

start_date: Start date, in the format of "mm/dd/yyyy", for the period of calculation

end_date: End date, in the format of "mm/dd/yyyy", for the period of calculation

period: The temporal scale for the 7Q10 calculation, can be "Annual" or "Custom". Default is "Annual"

custom_start: If "Custom" is designated as the period, the start date (excluding year) of the custom period in the character format of "mm-dd"

custom_end: If "Custom" is designated as the period, the end date (excluding year) of the custom period in the character format of "mm-dd"

wy_start: Only applicable for the "Annual" period. The water year beginning date (excluding year). The date should be in the character format of "mm-dd". If not specified the default is "10-01"

method_type: The method used to calculate 7Q10.  Options are "Distribution Free", "log Pearson Type III", or "Both"

Keywords: 7Q10, Flow, Oregon

Example:

Oregon7Q10(Station_ID, start_date, end_date, period = "Annual", custom_start = NA, custom_end = NA, wy_start = "10-01", method_type = "Both")
