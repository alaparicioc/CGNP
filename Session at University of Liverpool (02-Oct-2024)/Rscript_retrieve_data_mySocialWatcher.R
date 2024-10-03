############################################################
### CGNP - Collaborative Group in Nowcasting Populations ###
############## --- Session 2nd October 2024 --- ############
############################################################
# Retrieving Facebook data
# See: http://18.135.72.18/api/v1/#content
############################################

################################
# install and load packages
################################
packages <- c("httr", "jsonlite", "tidyverse")
if(!require(packages))install.packages(packages)

library("httr")
library("jsonlite")
library("tidyverse")

options(scipen = 999)


args <- list(token = (token)        #include your token here
             platform = "facebook",
             country = "GB",
             gender = 0,
             date_start = "2024-01-01")


# submit query as GET request
response <- httr::GET(url = 'http://18.135.72.18/', 
                      path = 'api/v1/query',
                      query = args)


# format response as list
response <- jsonlite::fromJSON(httr::content(response, as='text'))


# check status
print(response$status)
print(response$message)


# extract data in various formats
if(response$status == 200){
  
  # json string
  data <- response$data
  
  # json -> list of lists
  data <- jsonlite::fromJSON(data)
  
  # list of lists -> data.frame with cells containing lists
  # note: this is a convenient format for dealing with JSONs for some data.frame cells in R.
  data <- as.data.frame(do.call(cbind, data))
  
  #-- unlist data --#
  
  # identify columns storing json objects
  json_cols <- c('geo_locations','all_fields','targeting','response')
  
  # non-json cells: unlist
  for(name in names(data)[!names(data) %in% json_cols]){
    not_nulls <- which(!unlist(lapply(data[,name], is.null)))
    res <- rep(NA, length(data[,name]))
    res[not_nulls] <- unlist(data[,name])
    data[,name] <- res
  }
  
  # json cells: lists -> json strings
  for(name in json_cols){
    data[,name] <- unlist(lapply(data[,name], jsonlite::toJSON))
  }
}


##########################################
# Automatising retrieving Facebook data
#########################################

generate_ten_day_periods <- function(start_year, end_year) {
  periods <- data.frame()
  for(year in start_year:end_year) {
    for(month in 1:12) {
      start_date <- ymd(paste0(year, "-", month, "-01"))
      month_end_date <- ifelse(month == 2, 
                               start_date + days(ifelse(year %% 4 == 0 && (year %% 100 != 0 || year %% 400 == 0), 28, 27)), # February
                               ifelse(month %in% c(4, 6, 9, 11), 
                                      start_date + days(29), # 30-day months
                                      start_date + days(30))) # 31-day months
      month_end_date <- min(month_end_date, today()) # Adjust for the current date
      
      period_start <- start_date
      while(period_start <= month_end_date) {
        period_end <- min(period_start + days(9), month_end_date)
        periods <- rbind(periods, data.frame(start = period_start, end = period_end))
        period_start <- period_end + days(1)
      }
    }
  }
  return(periods)
}


date_ranges <- generate_ten_day_periods(2024, year(today())) %>%
  filter(start >= as.Date("2024-04-01"),
         end <= as.Date("2024-05-13")) 
View(date_ranges)


# Genders
genders <- c(0, 1, 2)


# Initialise an empty data frame for combined data for facebook
combined_data_f <- data.frame()


# Loop over each date and each gender for Facebook
for(i in 1:nrow(date_ranges)) {
  for(gender in genders) {
    # Query arguments for Facebook
    args <- list(
      token = (token),
      platform = "facebook",
      country = "GB",
      gender = gender,
      date_start = as.character(date_ranges$start[i]),
      date_end = as.character(date_ranges$end[i])
    )
    
    # Submit query as GET request for Facebook
    response <- httr::GET(url = 'http://18.135.72.18/', 
                          path = 'api/v1/query_clean',
                          query = args)
    
    # Format response as list
    response_data <- jsonlite::fromJSON(httr::content(response, as='text'))
    
    # Check status and process data for Facebook
    if(response_data$status == 200) {
      data <- jsonlite::fromJSON(response_data$data)
      data <- as.data.frame(do.call(cbind, data))
      
      # Unlist nested data
      for(name in names(data)) {
        not_nulls <- which(!unlist(lapply(data[,name], is.null)))
        res <- rep(NA, length(data[,name]))
        res[not_nulls] <- unlist(data[,name])
        data[,name] <- res
      }
      
      # Add columns to indicate the source period and gender
      data$source_period <- paste0("Period_", i)
      data$gender <- gender
      data$platform <- "facebook"
      
      # Combine the data
      combined_data_f <- rbind(combined_data_f, data)
    } else {
      print(paste0("Error for period ", i, " and gender ", gender, ": ", response_data$message))
    }
  }
}

################################
# Retrieving Instagram data
################################
# Initialise an empty data frame for combined data for Instagram
combined_data_i <- data.frame()

date_ranges <- generate_ten_day_periods(2024, year(today())) %>%
  filter(start >= as.Date("2024-04-01"),
         end <= as.Date("2024-05-13")) 
View(date_ranges)


# Loop over each date and each gender for Instagram
for(i in 1:nrow(date_ranges)) {
  for(gender in genders) {
    # Query arguments for Facebook
    args <- list(
      token = (token),
      platform = "instagram",
      country = "GB",
      gender = gender,
      date_start = as.character(date_ranges$start[i]),
      date_end = as.character(date_ranges$end[i])
    )
    
    # Submit query as GET request for Facebook
    response <- httr::GET(url = 'http://18.135.72.18/', 
                          path = 'api/v1/query_clean',
                          query = args)
    
    # Format response as list
    response_data <- jsonlite::fromJSON(httr::content(response, as='text'))
    
    # Check status and process data for Facebook
    if(response_data$status == 200) {
      data <- jsonlite::fromJSON(response_data$data)
      data <- as.data.frame(do.call(cbind, data))
      
      # Unlist nested data
      for(name in names(data)) {
        not_nulls <- which(!unlist(lapply(data[,name], is.null)))
        res <- rep(NA, length(data[,name]))
        res[not_nulls] <- unlist(data[,name])
        data[,name] <- res
      }
      
      # Add columns to indicate the source period and gender
      data$source_period <- paste0("Period_", i)
      data$gender <- gender
      data$platform <- "instagram"
      
      # Combine the data
      combined_data_i <- rbind(combined_data_i, data)
    } else {
      print(paste0("Error for period ", i, " and gender ", gender, ": ", response_data$message))
    }
  }
}



# Combine Facebook and Instagram data
combined_data <- rbind(combined_data_f, combined_data_i)




# View the combined data
print(combined_data)
library(httr)
library(jsonlite)
library(dplyr)

# Platforms
platforms <- c("facebook", "instagram")

# Generate daily date ranges from 2024-01-01 to 2024-05-13
date_ranges <- data.frame(
  start = seq(from = as.Date("2024-01-01"), to = as.Date("2024-05-12"), by = "day"),
  end = seq(from = as.Date("2024-01-02"), to = as.Date("2024-05-13"), by = "day")
)

# Genders
genders <- c(0, 1, 2)

# Initialise an empty data frame for combined data for facebook and instagram
combined_data_f <- data.frame()
combined_data_i <- data.frame()

# Loop over each date and each gender for Facebook
for(i in 1:nrow(date_ranges)) {
  for(gender in genders) {
    # Query arguments for Facebook
    args <- list(
      token = (token),
      platform = "facebook",
      country = "GB",
      gender = gender,
      date_start = as.character(date_ranges$start[i]),
      date_end = as.character(date_ranges$end[i])
    )
    
    # Submit query as GET request for Facebook
    response <- httr::GET(url = 'http://18.135.72.18/', 
                          path = 'api/v1/query_clean',
                          query = args)
    
    # Format response as list
    response_data <- jsonlite::fromJSON(httr::content(response, as='text'))
    
    # Check status and process data for Facebook
    if(response_data$status == 200) {
      data <- jsonlite::fromJSON(response_data$data)
      data <- as.data.frame(do.call(cbind, data))
      
      # Unlist nested data
      for(name in names(data)) {
        not_nulls <- which(!unlist(lapply(data[,name], is.null)))
        res <- rep(NA, length(data[,name]))
        res[not_nulls] <- unlist(data[,name])
        data[,name] <- res
      }
      
      # Add columns to indicate the source period and gender
      data$source_period <- paste0("Period_", i)
      data$gender <- gender
      data$platform <- "facebook"
      
      # Combine the data
      combined_data_f <- rbind(combined_data_f, data)
    } else {
      print(paste0("Error for period ", i, " and gender ", gender, ": ", response_data$message))
    }
  }
}

# Loop over each date and each gender for Instagram (similar to Facebook)
for(i in 1:nrow(date_ranges)) {
  for(gender in genders) {
    # Query arguments for Instagram
    args <- list(
      token = (token),
      platform = "instagram",
      country = "GB",
      gender = gender,
      date_start = as.character(date_ranges$start[i]),
      date_end = as.character(date_ranges$end[i])
    )
    
    # Submit query as GET request for Instagram
    response <- httr::GET(url = 'http://18.135.72.18/', 
                          path = 'api/v1/query_clean',
                          query = args)
    
    # Format response as list
    response_data <- jsonlite::fromJSON(httr::content(response, as='text'))
    
    # Check status and process data for Instagram
    if(response_data$status == 200) {
      data <- jsonlite::fromJSON(response_data$data)
      data <- as.data.frame(do.call(cbind, data))
      
      # Unlist nested data
      for(name in names(data)) {
        not_nulls <- which(!unlist(lapply(data[,name], is.null)))
        res <- rep(NA, length(data[,name]))
        res[not_nulls] <- unlist(data[,name])
        data[,name] <- res
      }
      
      # Add columns to indicate the source period and gender
      data$source_period <- paste0("Period_", i)
      data$gender <- gender
      data$platform <- "instagram"
      
      # Combine the data
      combined_data_i <- rbind(combined_data_i, data)
    } else {
      print(paste0("Error for period ", i, " and gender ", gender, ": ", response_data$message))
    }
  }
}

# Combine Facebook and Instagram data
combined_data <- rbind(combined_data_f, combined_data_i)

# View the combined data
print(combined_data)
