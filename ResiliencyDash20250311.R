rm(list=ls())
options(scipen=999) #disables scientific notation

#install.packages("ggplot2")
#install.packages("ggalluvial")
#install.packages("stringdist") #For Fuzzy Match
#install.packages("viridis")
#install.packages("DT")
#install.packages("ZipcodeR)
#install.packages("geosphere")

library(ggplot2)
library(readxl)
library(dplyr)
library(tidyr)
library(writexl)
library(readr)
library(data.table)
library(knitr)
library(viridis)
library(scales)
library(DT)
library(zipcodeR)
library(geosphere)
library(leaflet)
library(reshape2)

#For Rosalie or Peggy - set the file path accordingly (look at the change to Q4 at the end, Peggy)
setwd("C:/Users/MMcWeeney/Potomac Institute for Policy Studies/AF Industrial Base - AQX OCEA - Documents/AQX OCEA/CLIN008/06DeliverableDrafts/Q4")
#setwd("/Users/rosalieloewen/Library/CloudStorage/OneDrive-PotomacInstituteforPolicyStudies/AQX OCEA/CLIN008/06DeliverableDrafts/Q4")
file_path<-  "/Users/rosalieloewen/Library/CloudStorage/OneDrive-PotomacInstituteforPolicyStudies/AQX OCEA/CLIN008/06DeliverableDrafts/Q4"

#The full dataset is rather large, so we have dummy data sets for coding practice
#prime_all<- fread("/Users/rosalieloewen//Library/CloudStorage/OneDrive-PotomacInstituteforPolicyStudies/AQX OCEA/CLIN008/06DeliverableDrafts/Q4/prime_all.csv")

#OR Read in a dummy file with either 5C, 10K, or 100K (check end of file)
prime_all_dummy <- fread("/Users/rosalieloewen//Library/CloudStorage/OneDrive-PotomacInstituteforPolicyStudies/AQX OCEA/CLIN008/06DeliverableDrafts/Q4/prime_all_dummy5C.csv")
#prime_all_dummy <- fread("C:/Users/MMcWeeney/Potomac Institute for Policy Studies/AF Industrial Base - AQX OCEA - Documents/AQX OCEA/CLIN008/06DeliverableDrafts/Q4/prime_all_dummy5C.csv")
#prime_all_dummy <- fread("/Users/rosalieloewen//Library/CloudStorage/OneDrive-PotomacInstituteforPolicyStudies/AQX OCEA/CLIN008/06DeliverableDrafts/Q4/prime_all_dummy10K.csv")
prime_all_dummy <- fread("/Users/rosalieloewen//Library/CloudStorage/OneDrive-PotomacInstituteforPolicyStudies/AQX OCEA/CLIN008/06DeliverableDrafts/Q4/prime_all_dummy100K.csv")

#redefine prime_all depending on which dataset you chose
prime_all<-prime_all_dummy
rm(prime_all_dummy)

#Adjust the prime_all data set----------
#Lets select for just the variables we use in this analysis
cols_keep <- c("federal_action_obligation", "action_date_fiscal_year", "recipient_uei", 
               "recipient_name", "primary_place_of_performance_zip_4", 
               "naics_code", "naics_description")

# Subset the data.table to only these columns (this creates a new data.table)
prime_all <- prime_all[, ..cols_keep]

#advance ability to look at positive or negative federal action obligations...
prime_all$FACpos<-ifelse(prime_all$federal_action_obligation>0,prime_all$federal_action_obligation,NA)
prime_all$FACneg<-ifelse(prime_all$federal_action_obligation<0,prime_all$federal_action_obligation,NA)
MedianContract<- (prime_all[, median(FACpos, na.rm = TRUE)])


#Set up data and functions to get your NAICS codes in order--------
# Convert naics_code to character in place
prime_all[, naics_code := as.character(naics_code)]
# Create a unique lookup table for naics_code using the USASpend descriptions... and its first description that is not blank
prime_naics_lookup <- prime_all[, .(naics_description = 
                                      naics_description[which(naics_description != "" & !is.na(naics_description))[1]]), 
                                by = naics_code]

#This is the official lookup document from the department of commerce
NAICS_lookup <- fread("/Users/rosalieloewen//Library/CloudStorage/OneDrive-PotomacInstituteforPolicyStudies/AQX OCEA/CLIN008/06DeliverableDrafts/Q3/Resilience/ContractDataRaw/NAICS_lookup.csv")
#NAICS_lookup <- fread("C:/Users/MMcWeeney/Potomac Institute for Policy Studies/AF Industrial Base - AQX OCEA - Documents/AQX OCEA/CLIN008/06DeliverableDrafts/Q3/Resilience/ContractDataRaw/NAICS_lookup.csv")

NAICS_lookup[, naics_code := `2022 NAICS US   Code`]
NAICS_lookup[, naics_description := `2022 NAICS US Title`]

#Create a function that does a simple then recursive lookup
lookup_naics_recursive <- function(code) {
  # Ensure the code is a trimmed character string
  code <- trimws(as.character(code))
  # Attempt a full (6-digit) match first on the DOD descriptions (prime_naics_lookup)
  prime_match <- prime_naics_lookup[naics_code == code, naics_description]
  if (length(prime_match) > 0 && !is.na(prime_match[1])) {
    return(prime_match[1])
  }
  # If prime_naics_lookup returned NA, attempt a full match using NAICS_lookup
  full_match <- NAICS_lookup[naics_code == code, naics_description]
  if (length(full_match) > 0 && !is.na(full_match[1])) {
    return(full_match[1])
  }
  # For 6-digit codes, try truncating to 5, 4, 3, then 2 digits
  if (nchar(code) == 6) {
    for (i in seq(5, 2, by = -1)) {
      subcode <- substr(code, 1, i)
      truncated_match <- NAICS_lookup[naics_code == subcode, naics_description]
      if (length(truncated_match) > 0) {
        # Append "other" because the full 6-digit match was not found
        return(paste(truncated_match[1], "(Other)"))
      }
    }
  }
  # If no match is found, return NA
  return(NA)
}

#add lat and lon to the dataset
zip_coords <-zip_code_db[, c("zipcode", "lat", "lng")]
set(prime_all, j = "zip", value = substr(prime_all$primary_place_of_performance_zip_4, 1, 5))
prime_all <- merge(prime_all, zip_coords, by.x = "zip", by.y = "zipcode", all.x = TRUE)


#Lets work on pulling out Dispersion per NAICS to show where there is concentration:---------
#for this analysis, we are going to look only at positive federal_action_obligations, negative ones we will use separately
#Here is where the user filter can take place, i.e. filter by NAICS or by Year....
dt<-prime_all

#Create the DISPERSION METRIC (based on HHI) METRIC

#First, we create a function that calculates the Dispersion using the filtered data----------
#this function also adds outlier flags and a decile marker for the concentration report
MDispersion_function <- function(dt) {
  # Step 1: Collapse the data so that each (naics_code, recipient_uei) appears once.
  dt_net <- dt[, .(uei_sum = sum(FACpos, na.rm = TRUE)),
               by = .(naics_code, recipient_uei)]
  
  # Filter out groups where uei_sum is zero (i.e. no positive obligations) and where there is no NAICS
  dt_net <- dt_net[uei_sum > 0 & !is.na(naics_code)]
  
  # Step 2: Compute totals and counts using the collapsed data.
  dt_totals <- dt_net[, .(total = sum(uei_sum),
                          num_recipients = .N),
                      by = naics_code]
  
  # Step 3: Merge the totals back into the collapsed data.
  dt_net <- merge(dt_net, dt_totals, by = "naics_code")
  
  # Step 4: Calculate each recipient's share (as a percentage) within its naics_code group.
  dt_net[, share := (uei_sum / total) * 100]
  
  # Step 5: Compute  1-HHI for each naics_code by summing the squared shares.
  dt_HHI <- dt_net[, .(DISP = ((10000 - sum(share^2, na.rm = TRUE))/10000),
                       total = total[1],
                       num_recipients = num_recipients[1]),
                   by = naics_code]
  
  # Step 6: Divide `total` spending into deciles
  dt_HHI[, decile := cut(total, 
                            breaks = quantile(total, probs = seq(0, 1, 0.1), na.rm = TRUE), 
                            include.lowest = TRUE, 
                            labels = 1:10)]
  
  # A "high" outlier is defined as HHI > (Q1-1.5*IQR) within that decile.
  dt_HHI[, outlier := {
    q1 <- quantile(DISP, 0.25, na.rm = TRUE)
    q3 <- quantile(DISP, 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    threshold <- q1 - 1.5 * iqr
    DISP < threshold
  }, by = decile]
  
  # Return the results ordered by total (descending)
  dt_HHI[order(-total)]
}
#run function on your chosen datatable and assign the outcome to the global environment
MDispReport<-MDispersion_function(dt)

#Create the Dispersion IQR plot as part of concentration report--------
ggplot(MDispReport, aes(x = decile, y = DISP)) +
  geom_boxplot(aes(group = decile), color = "steelblue", outlier.shape = NA) +  # Exclude default outliers for custom points
  geom_jitter(data = MDispReport[outlier == TRUE], aes(x = decile, y = DISP), 
              color = "red", size = 2, alpha = 0.6) +  # Add custom outlier points
  scale_x_discrete("Spending Decile") +
  scale_y_continuous("Dispersion (IQR distribution)") +
  ggtitle("NAICS Market Dispersion IQR distribution by spending decile with outliers") +
  theme_minimal()

#ggsave("HHIDecile.pdf", width = 16, height = 9, units = "in")
#fwrite(HHI_naics, "HHI_naics.csv")

#Create the concentration outlier list report and table------------
DISPoutlier_list <- MDispReport[outlier == TRUE, .(
  naics_code,
  DISP,
  total,
  num_recipients,
  decile
)][order(-total)]

# Perform the lookup in-place to add NAICS descriptions
#Apply the recursive NAICS lookup function to the list
DISPoutlier_list[, naics_description := sapply(naics_code, lookup_naics_recursive)]

#Output a nice table of the HHI outliers
datatable(DISPoutlier_list, 
          extensions = 'Buttons',
          options = list(
          pageLength = 20,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
          ),
          caption = 'Dispersion Outliers')%>%
  formatRound(columns = "DISP", digits = 3, mark = "") %>%
  formatCurrency(columns = "total", currency = "$", digits = 0)

#Create a yearly weighted Dispersion score and plot------------------
#here is where you can perform a filter
dt<-prime_all

#define a function for the yearly weighted HHI score
weighted_MDISP_year <- function(dt) {
  # Step 1: Collapse the data to one row per fiscal year, NAICS code, and recipient
  dt_net <- dt[, .(uei_sum = sum(FACpos, na.rm = TRUE)),
               by = .(action_date_fiscal_year, naics_code, recipient_uei)]
  
  # Filter out any groups with zero positive spending or missing a NAICS
  dt_net <- dt_net[uei_sum > 0 | !is.na(naics_code)]
 
   # Step 2: For each fiscal year and NAICS code, compute the total spending and recipient count
  dt_totals <- dt_net[, .(total_naics = sum(uei_sum),
                          num_recipients = .N),
                      by = .(action_date_fiscal_year, naics_code)]
  
  # Merge these totals back into the collapsed data
  dt_net <- merge(dt_net, dt_totals, by = c("action_date_fiscal_year", "naics_code"))
  
  # Step 3: Calculate each recipient's share (as a percentage) of spending within the NAICS group for that year
  dt_net[, share := (uei_sum / total_naics) * 100]
  
  # Step 4: Compute the HHI for each fiscal year and NAICS code
  # HHI is the sum of squared shares for that group.
  dt_HHI <- dt_net[, .(DISP = (10000 - sum(share^2, na.rm = TRUE))/10000,
                       total_naics = total_naics[1]),
                   by = .(action_date_fiscal_year, naics_code)]
  
  # Step 5: Compute total spending by fiscal year
  dt_year_total <- dt_HHI[, .(year_total = sum(total_naics)), by = action_date_fiscal_year]
  
  # Merge the total yearly spending back to the NAICS-level HHI
  dt_HHI <- merge(dt_HHI, dt_year_total, by = "action_date_fiscal_year")
  
  # Step 6: For each fiscal year, compute the weighted HHI:
  # For each NAICS code, weight its HHI by its spending share (total_naics/year_total),
  # then sum these weighted HHI values to get a yearly metric.
  dt_weighted <- dt_HHI[, .(
    weighted_DISP = sum((total_naics / year_total) * DISP, na.rm = TRUE),
    total_year   = year_total[1]
  ), by = action_date_fiscal_year]
  
  #order by year
  dt_weighted <- dt_weighted[order(action_date_fiscal_year)]
  
  return(dt_weighted)
}

MDISPYOY<-weighted_MDISP_year(dt)

ggplot(MDISPYOY, aes(x = as.numeric(as.character(action_date_fiscal_year)), y = weighted_DISP)) +
  geom_smooth(method = "lm", se = FALSE, color = "grey", linewidth = 1, linetype = "dashed") +
  geom_col(fill = "steelblue", width = 0.7) +  # Reduce column width to add spacing
  theme_minimal() +                           # Apply a minimal theme
  labs(
    title = "Dollar-weighted Dispersion Score by Fiscal Year",
    x = "Fiscal Year",
    y = "Weighted Dispersion Score"
  ) +
  scale_x_continuous(
    breaks = sort(unique(as.numeric(as.character(MDISPYOY$action_date_fiscal_year))))
  ) +
  theme(
    text = element_text(size = 12),           # Adjust font size
    axis.text.x = element_text(size = 10),    # Ensure labels are clear
    axis.text.y = element_text(size = 10),
    plot.title = element_text(hjust = 0.5)    # Center the title
  )

# Save the chart as a file
#ggsave("DISPYOYWeighted.pdf", width = 9, height = 7, units = "in")
dt<-prime_all
#Create the SFR METRIC---------------
#Lets look at success to failure ratio: we create a failure ratio that measure sumFACpos/(sumFACpos +-sumFACneg)
#we specify for zero values to ensure that we don't divide by zero and to clarify
#To avoid a ton of small adjustments - which are not likely to be material failures we will set the threshold at 0.10 of the median positive spend in the dataset
SFThreshold<-0.10*MedianContract
SuccessFailRatioFunction <- function(dt){
  dt_SF <- dt[, .(
    total_neg      = -sum(FACneg, na.rm = TRUE),
    total_pos      = sum(FACpos, na.rm = TRUE),
    num_recipients = uniqueN(recipient_uei)
  ), by = naics_code]
  
  # Remove rows where both total_neg is below the threshold, those are minor adjustments
  dt_SF <- dt_SF[!(total_neg < SFThreshold)] # #total_pos == 0)]
  
  # Compute SF_ratio:
  # if total_pos == 0 -> 0 (only negatives)
  # if total_neg == 0 -> 1 (only positives)
  # otherwise, total_neg / (total_pos + total_neg)
  dt_SF[, SF_ratio := fifelse(total_pos == 0, 0,
                                     fifelse(total_neg == 0, 1,
                                             total_pos / (total_pos + total_neg)))]
  
  # Divide `total_pos` spending into deciles
  dt_SF[, decile := cut(total_pos, 
                         breaks = quantile(total_pos, probs = seq(0, 1, 0.1), na.rm = TRUE), 
                         include.lowest = TRUE, 
                         labels = 1:10)]
  
  # A "high" outlier is defined as SF_rate > (Q1 - 1.5*IQR) within that decile.
  dt_SF[, outlier := {
    q1 <- quantile(SF_ratio, 0.25, na.rm = TRUE)
    q3 <- quantile(SF_ratio, 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    threshold <- q1 - 1.5 * iqr
    SF_ratio < threshold
  }, by = decile]
  
  # Return the results ordered by total (descending)
 setorder(dt_SF, -total_pos)
  
  return(dt_SF)
      
}

SFresult<-SuccessFailRatioFunction(dt)

#Create the failure report plot and outlier list-------------
ggplot(SFresult, aes(x = decile, y = SF_ratio)) +
  geom_boxplot(aes(group = decile), color = "orange", outlier.shape = NA) +  # Exclude default outliers for custom points
  geom_jitter(data = SFresult[outlier == TRUE], aes(x = decile, y = SF_ratio), 
              color = "red", size = 2, alpha = 0.6) +  # Add custom outlier points
  scale_x_discrete("Spending Decile") +
  scale_y_continuous("Success:Failure Ratio (IQR distribution)") +
  ggtitle("NAICS Success:Failure Ratio IQR distribution by spending decile with outliers") +
  theme_minimal()

#Create the outlier list report
SFoutlier_list <- SFresult[outlier == TRUE, .(
  naics_code,
  SF_ratio,
  total_neg,
  total_pos,
  num_recipients,
  decile
)][order(-total_neg)]


# Perform the lookup in-place to add NAICS descriptions
#Apply the recursive lookup function to the list
SFoutlier_list[, naics_description := sapply(naics_code, lookup_naics_recursive)]

#make a nice table
datatable(SFoutlier_list, 
          extensions = 'Buttons',
          options = list(
            pageLength = 20,
            dom = 'Bfrtip',
            buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
          ),
          caption = 'Success:Fail Ratio Outliers')%>%
  formatRound(columns = "SF_ratio", digits = 2) %>%
  formatCurrency(columns = "total_neg", currency = "$", digits = 0) %>%
  formatCurrency(columns = "total_pos", currency = "$", digits = 0)

#Create the Failure YOY metric and plot, weighted by spend--------
weighted_SF_year <- function(dt) {
  # Step 1: Collapse the data to one row per fiscal year, NAICS code
  dt_net <- dt[, .(
    total_neg = -sum(FACneg, na.rm = TRUE),  # convert negatives to a positive magnitude
    total_pos  = sum(FACpos, na.rm = TRUE),
    num_recipients = uniqueN(recipient_uei)
  ), by = .(action_date_fiscal_year, naics_code)]
  
  # Step 2: Filter out any groups with a zero denominator or missing NAICS
  dt_net <- dt_net[!(total_neg == 0 & total_pos == 0)]
  dt_net <- dt_net[!is.na(naics_code)]
  
  # Step 3: Calculate the SF ratio for each fiscal year and NAICS code.
  # Compute SF_ratio:
  # if total_pos == 0 -> 0 (only negatives)
  # if total_neg == 0 -> 1 (only positives)
  # otherwise, total_neg / (total_pos + total_neg)
  dt_net[, SF_ratio := fifelse(total_pos == 0, 0,
                              fifelse(total_neg == 0, 1,
                                      total_pos / (total_pos + total_neg)))]
  
  #Step 4: Calculate total positive spending for each fiscal year for each NAICS (this will provide the weighting)
  dt_net[, year_total := sum(total_pos, na.rm=T), by = action_date_fiscal_year]
  
  
  # Step 5 For each fiscal year, weight the SF rate by total positive spend:
    dt_weighted <- dt_net[, .(
    weighted_SF_ratio = sum((total_pos / year_total) * SF_ratio, na.rm = TRUE),
    total_year   = year_total[1]
  ), by = action_date_fiscal_year]
  
  # Step 6 order by year
  dt_weighted <- dt_weighted[order(action_date_fiscal_year)]
  
  #Step 7, transform to expand differences near 1
  dt_weighted[, weighted_SF_ratio_adjusted := 1 - (1 - weighted_SF_ratio)^0.5]
  
  return(dt_weighted)
}
SFYOY<-weighted_SF_year(dt)

ggplot(SFYOY, aes(x = as.numeric(as.character(action_date_fiscal_year)), y = weighted_SF_ratio_adjusted)) +
  geom_smooth(method = "lm", se = FALSE, color = "grey", linewidth = 1, linetype = "dashed") +
  geom_col(fill = "orange", width = 0.7) +  # Reduce column width to add spacing
  theme_minimal() +                           # Apply a minimal theme
  labs(
    title = "Dollar Weighted Supply Chain Robustness by Fiscal Year",
    x = "Fiscal Year",
    y = "Adjusted Success:Fail Ratio"
  ) +
  scale_x_continuous(
    breaks = sort(unique(as.numeric(as.character(SFYOY$action_date_fiscal_year))))
  ) +
  theme(
    text = element_text(size = 12),           # Adjust font size
    axis.text.x = element_text(size = 10),    # Ensure labels are clear
    axis.text.y = element_text(size = 10),
    plot.title = element_text(hjust = 0.5)    # Center the title
  )

# Save the chart as a file
#ggsave("FAILYOYWeighted.pdf", width = 9, height = 7, units = "in")

#Create a geographic DISPERSION metric-------------
#Here is where you can filter your dataset 
dt<-prime_all
k_global <-1924519
geodispersion_function<- function (dt, k) {
  # dt is expected to be a data.table with the following columns:
  #   - lat: latitude of the supplier
  #   - lng: longitude of the supplier
  #   - FACpos: dollar weight for the supplier
  #remove if lat or long == NA
  dt <- dt[!is.na(lat) & !is.na(lng)]
  #Also lets aggregate by recipient, NAICS, and location so that we are not measuring the distance
  #between a contract and itself as the contract adjusts within the year.
  dt <- dt[, .(FACpos = sum(FACpos, na.rm = TRUE)), 
           by = .(recipient_uei, lat, lng)]
  if (nrow(dt) == 0) {
    warning("No data remaining after filtering.")
    return(NULL)
  }
  # Extract dollar weights as a vector
  w <- dt$FACpos
  
  # Create a matrix of coordinates (longitude first, then latitude)
  coords <- as.matrix(dt[, .(lng, lat)])
  
  # Compute the full pairwise geodesic distance matrix using the Haversine formula
  # D[i, j] gives the distance between point i and point j.
  D <- distm(coords, coords, fun = distHaversine)
  
  # Compute the weight matrix as the outer product of the weights
  # Here, W[i, j] = w[i] * w[j]. This represents the "interaction" of dollars between supplier i and supplier j.
  W <- outer(w, w, FUN = "*")
  
  # Calculate the numerator: weighted sum of pairwise distances 
  # For each unique pair (i, j) (with i < j), we multiply the distance d(i,j) by the product of their weights (w[i] * w[j]).
  # We sum these products over all pairs. Since D and W are symmetric (d(i,j) = d(j,i)), summing over the full matrix
  # counts each pair twice. Therefore, we divide by 2 to get the sum for unique pairs only.
  numerator <- sum(D * W) / 2
  
  # Calculate the denominator: sum of the weight products for unique pairs
  # This is equivalent to ( (sum(w))^2 - sum(w^2) ) / 2
  denominator <- (sum(w)^2 - sum(w^2)) / 2
  
  # Compute and return the weighted average pairwise distance 
  raw_value <- (numerator / denominator)

  # Apply the transformation: T(x) = x/(x+k)
  scaled_value <- raw_value / (raw_value + k)
  
  return(list(raw = raw_value, scaled = scaled_value))
}

#To properly scale results we need to create a scaling parameter which we will set as the median of the 
#raw values of the 10k dataset
# Compute raw dispersion for each fiscal year to get to an appropriate scaling value
#We wont run this again but this serves to remind us of how we came up with that value ()
#raw_disp_values <- prime_all[, {
#  tmp <- geodispersion_function(.SD, k = 1)  # temporary k value, won't affect raw_value
#  .(raw_dispersion = tmp$raw)
#}, by = action_date_fiscal_year]$raw_dispersion
# Compute global median (or another characteristic value) of raw dispersion
#k_global <- median(raw_disp_values, na.rm = TRUE)
#k_global is 1924519

#compute the metric for each fiscal year: 
GEODISPYOY <- dt[, {
  tmp <- geodispersion_function(.SD, k = k_global)
  .(dispersion_raw = tmp$raw, dispersion_metric = tmp$scaled)
}, by = action_date_fiscal_year][order(action_date_fiscal_year)]


#create a graph for YOY
ggplot(GEODISPYOY, aes(x = as.numeric(as.character(action_date_fiscal_year)), y = dispersion_metric)) +
  geom_smooth(method = "lm", se = FALSE, color = "grey", linewidth = 1, linetype = "dashed") +
  geom_col(fill = "darkgreen", width = 0.7) +  # Reduce column width to add spacing
  theme_minimal() +                           # Apply a minimal theme
  labs(
    title = "Weighted Geographic Dispersion by Fiscal Year",
    x = "Fiscal Year",
    y = "Weighted Geographic Dispersion Metric"
  ) +
  scale_x_continuous(
    breaks = sort(unique(as.numeric(as.character(GEODISPYOY$action_date_fiscal_year))))
  ) +
  theme(
    text = element_text(size = 12),           # Adjust font size
    axis.text.x = element_text(size = 10),    # Ensure labels are clear
    axis.text.y = element_text(size = 10),
    plot.title = element_text(hjust = 0.5)    # Center the title
  )

#compute the metric by NAICS code and flag outliers
GEODISPNAICS <- function (dt, k){
  # Group by NAICS code and compute:
  # - dispersion_raw: the weighted dispersion (via the dispersion function, but using the raw value, not scaled
  # - total_pos: total positive spending (for weighting and decile calculation)
  # - num_recipients: number of unique recipient_uei 
   dt_disp<-dt[, .(
    dispersion_raw = geodispersion_function(.SD, k=k)$raw,
    dispersion_metric = geodispersion_function(.SD, k=k)$scaled,
    total_pos = sum(FACpos, na.rm=T),
    num_recipients = uniqueN(recipient_uei)),
    by = naics_code]
   
  # Remove groups with fewer than 3 recipients
  dt_disp <- dt_disp[num_recipients >= 3]
   
  # Remove rows where dispersion_value is NA (if any)
  dt_disp <- dt_disp[!is.na(dispersion_metric)]
   
  # Create deciles based on total spending (total_pos)
  dt_disp[, decile := cut(total_pos, 
                           breaks = quantile(total_pos, probs = seq(0, 1, 0.1), na.rm = TRUE),
                           include.lowest = TRUE,
                           labels = 1:10)] 
  
  #flag dispersion_metric outliers
  # outlier is defined as having a dispersion_value greater than Q1 - 1.5 * IQR in that decile.
  dt_disp[, outlier := {
    q1 <- quantile(dispersion_metric, 0.25, na.rm = TRUE)
    q3 <- quantile(dispersion_metric, 0.75, na.rm = TRUE)
    iqr <- q3 - q1
    threshold <- q1 - 1.5 * iqr
    dispersion_metric < threshold
  }, by = decile]
  # order the results by descending total spending
  setorder(dt_disp, -total_pos)
  
  return(dt_disp)
}


DISP_by_NAICS<-GEODISPNAICS(prime_all, k=k_global)

#Create the Dispersion report plot and outlier list-------------
ggplot(DISP_by_NAICS, aes(x = decile, y = dispersion_metric)) +
  geom_boxplot(aes(group = decile), color = "darkgreen", outlier.shape = NA) +  # Exclude default outliers for custom points
  geom_jitter(data = DISP_by_NAICS[outlier == TRUE], aes(x = decile, y = dispersion_metric), 
              color = "red", size = 2, alpha = 0.6) +  # Add custom outlier points
  scale_x_discrete("Spending Decile") +
  scale_y_continuous("Geographic Dispersion per contract dollar (IQR distribution)")+

  ggtitle("NAICS Geographic Dispersion per Contract Dollar By Spending Decile") +
  theme_minimal()

#Create the outlier list report
DISPoutlier_list <- DISP_by_NAICS[outlier == TRUE, .(
  naics_code,
  dispersion_metric,
  total_pos,
  num_recipients,
  decile
)][order(-total_pos)]


# Perform the lookup in-place to add NAICS descriptions
#Apply the recursive lookup function to the list
DISPoutlier_list[, naics_description := sapply(naics_code, lookup_naics_recursive)]

#make a nice table
datatable(DISPoutlier_list, 
          extensions = 'Buttons',
          options = list(
            pageLength = 20,
            dom = 'Bfrtip',
            buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
          ),
          caption = 'Geographic Density per dollar')%>%
  formatRound(columns = "dispersion_metric", digits = 3) %>%
  formatCurrency(columns = "total_pos", currency = "$", digits = 0)


#Create a map...

leaflet(data = dt) %>% 
  addTiles() %>% 
  addCircleMarkers(
    ~lng, ~lat,
    radius = ~sqrt(FACpos)/(.02*nrow(dt)),  # adjust scaling as needed
    popup = ~paste("Zip:", zip, "<br>Contract Dollars:", FACpos)
  ) %>%
  setView(lng = -96, lat = 39, zoom = 3.5)


#Create a composite score for the YOY.  
YOYComp<-merge(merge(MDISPYOY, SFYOY, by = "action_date_fiscal_year", all = TRUE),
                   GEODISPYOY, by ="action_date_fiscal_year", all = TRUE)

#Could make a weighted average, but haven't yet...
YOYComp[, composite := (dispersion_metric + weighted_SF_ratio_adjusted + weighted_DISP)]


merged_long <- melt(YOYComp, 
                    id.vars = "action_date_fiscal_year", 
                    measure.vars = c("dispersion_metric", "weighted_SF_ratio_adjusted", "weighted_DISP"),
                    variable.name = "metric", 
                    value.name = "value")

ggplot(merged_long, aes(x = as.factor(action_date_fiscal_year), y = value, fill = metric)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = c("weighted_DISP" = "steelblue",
                               "weighted_SF_ratio_adjusted" = "orange",
                               "dispersion_metric" = "darkgreen")) +
  labs(title = "Composite Index by Fiscal Year",
       x = "Fiscal Year",
       y = "Composite Metric") +
  theme_minimal()
