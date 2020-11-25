source("script/log_utils.R")
library(R.utils)
library(tidyr)
library(dplyr)
library(stringr)

download_city_csv <- function(df){
  csv_url <- df["listings_data_url"]
  city_name <- df["city"]
  date <- df["date"]
  country <- df["country"]
  gz_name <- "listings.csv.gz"
  path_to_data_dir <- file.path(data_raw_dir, country, city_name, date, gz_name)
  if(!file.exists(file.path(data_raw_dir, country, city_name, date, "listings.csv"))){
    if(file.exists(path_to_data_dir)){
      gunzip(path_to_data_dir, remove = TRUE)
    }
    if(!file.exists(file.path(data_raw_dir, country, city_name, date, "listings.csv"))){
      download.file(url = csv_url,
        mode = "wb",
        destfile=path_to_data_dir)
        gunzip(path_to_data_dir, remove = TRUE)
    }
  }
  gz_name <- "calendar.csv.gz"
  if(!file.exists(file.path(data_raw_dir, country, city_name, date, "calendar.csv"))){
    if(file.exists(path_to_data_dir)){
      gunzip(path_to_data_dir, remove = TRUE)
    }
    if(!file.exists(file.path(data_raw_dir, country, city_name, date, "calendar.csv"))){
      download.file(url = str_replace(csv_url, "listings", "calendar"),
      mode = "wb",
      destfile=str_replace(path_to_data_dir, "listings", "calendar"))
      gunzip(str_replace(path_to_data_dir, "listings", "calendar"), remove = TRUE)
    }
  }
}

download_data <- function(country_name){
  urls_parse <- read.csv(data_url_parse_file)
  urls_parse <- urls_parse %>% filter(country == country_name) %>%
                               arrange(desc(date)) %>%
                               group_by(city) %>%
                               slice(1:3)
  apply(urls_parse,1,check_if_city_dir_exist)
  apply(urls_parse,1,download_city_csv)
}


parse_url <- function(){
  all_urls <- read.csv(data_url_file)
  base_url <- "http://data.insideairbnb.com/"
  all_urls <- all_urls %>% mutate(listings_data_url_parse = str_replace(listings_data_url,base_url,""))
  all_urls <- all_urls %>% separate(listings_data_url_parse, c("country","region","city","date","data","file_name"), '/')
  all_urls <- all_urls %>%select(listings_data_url,country, city, date)
  all_urls <- all_urls %>% arrange(country,city, date)
  write.csv(all_urls,file.path(data_dir,"urls_parse.csv"))
}

check_if_city_dir_exist <- function(df){
  city_name <- df["city"]
  date <- df["date"]
  country <- df["country"]
  if(!dir.exists(file.path(data_raw_dir,country, city_name, date))){
      dir.create(file.path(data_raw_dir,country, city_name, date), recursive=TRUE)
  }
}

# a generic function to prepare data for a specific city, data_date
prepare_data <- function(country, city, data_date)
{
    # Cleaning listings dataframe
    # suppose raw data is stored in data_raw/country/city/data_date/listings.csv.gz
    listings_url <- file.path(data_raw_dir,country, city, data_date, "listings.csv")
    # suppose raw data is stored in data_raw/country/city/data_date/calendar.csv.gz
    calendar_url <- file.path(data_raw_dir,country, city, data_date, "calendar.csv")

    log4r_info((paste0("reading data from ", listings_url)))
    listings <- read.csv(listings_url)
    log4r_info(paste0("reading data from ", calendar_url))
    calendar <- read.csv(calendar_url)

    ## Add Keys: columns city and day date
    listings$city <- city
    listings$data_date <- data_date

    ## Select interesting columns
    ### Most columns don't contain interesting information
    columns_listings <- c("city", "data_date", "id", "neighbourhood_cleansed",
                          "latitude", "longitude",
                          "property_type", "room_type", "accommodates", "bedrooms",
                          "beds", "price", "minimum_nights",  "maximum_nights")

    if(!(FALSE %in% (columns_listings %in% colnames(listings)))){
      listings <- listings %>%
          select(columns_listings) %>%
          arrange(id)


      # Cleaning calendar dataframe

      ## arrange by id and date
      calendar <- calendar %>%
          arrange(listing_id, date)

      ## add day number (starting first day)
      calendar <- calendar %>%
          group_by(listing_id) %>%
          mutate(day_nb = row_number()) %>%
          ungroup()

      ## change available column to binary
      calendar <- calendar %>%
          mutate(available = ifelse(available=="t", 1, 0))

      ## clean price column and transform to numeric
      calendar <- calendar %>%
          mutate(price = str_replace(price, "\\$", ""),
                 adjusted_price = str_replace(adjusted_price, "\\$", ""))
      calendar <- calendar %>%
          mutate(price = str_replace(price, ",", ""),
                 adjusted_price = str_replace(adjusted_price, ",", ""))
      calendar <- calendar %>%
          mutate(price = as.numeric(price),
                 adjusted_price = as.numeric(adjusted_price))

      ## calculate estimated revenue for upcoming day
      calendar <- calendar %>%
          mutate(revenue = price*(1-available))

      ## calculate availability, price, revenue for next 30, 60 days ... for each listing_id
      calendar <- calendar %>%
          group_by(listing_id) %>%
          summarise(availability_30 = sum(available[day_nb<=30], na.rm = TRUE),
                    #availability_60 = sum(available[day_nb<=60], na.rm = TRUE),
                    #availability_90 = sum(available[day_nb<=90], na.rm = TRUE),
                    #availability_365 = sum(available[day_nb<=365], na.rm = TRUE),
                    price_30 = mean(price[day_nb<=30 & available==0], na.rm = TRUE),
                    #price_60 = mean(price[day_nb<=60 & available==0], na.rm = TRUE),
                    #price_90 = mean(price[day_nb<=90 & available==0], na.rm = TRUE),
                    #price_365 = mean(price[day_nb<=365 & available==0], na.rm = TRUE),
                    revenue_30 = sum(revenue[day_nb<=30], na.rm = TRUE),
                    #revenue_60 = sum(revenue[day_nb<=60], na.rm = TRUE),
                    #revenue_90 = sum(revenue[day_nb<=90], na.rm = TRUE),
                    #revenue_365 = sum(revenue[day_nb<=365], na.rm = TRUE)
          )

      listings_cleansed <- listings %>% left_join(calendar, by = c("id" = "listing_id"))

      dir.create(file.path(data_cleansed_dir,country, city, data_date), recursive = TRUE)

      write.csv(listings_cleansed, file.path(data_cleansed_dir, country, city, data_date, data_csv_cleansed_name))
      log4r_info(paste(c("saving data into ", file.path(data_cleansed_dir,country, city, data_date, data_csv_cleansed_name))))
    }
    else{
      log4r_error(paste(c("country ",country, "city ",city," at date ", data_date, " does'nt contains all collumns")))
      write.csv("", file.path(data_raw_dir, country, city, data_date, "listings.csv"))
      write.csv("", file.path(data_raw_dir, country, city, data_date, "calendar.csv"))
    }

}



clean_data <- function(){
# Prepare data for multiple cities
  parse_url()
  download_data("belgium")
  download_data("spain")
  download_data("germany")

  country_to_clean <- list.files(data_raw_dir)
  if( dir.exists(data_raw_dir) ){
    for(country in 1:length(country_to_clean)){
      city_to_clean <- list.files(file.path(data_raw_dir, country_to_clean[country]))
      for(city in 1:length(city_to_clean)){
        city_date_to_clean <- list.files(file.path(data_raw_dir,country_to_clean[country], city_to_clean[city]))
        for(date in 1:length(city_date_to_clean)){
          log4r_info(file.exists(file.path(data_cleansed_dir,country_to_clean[country], city_to_clean[city], city_date_to_clean[date],data_csv_cleansed_name)))
          if(!file.exists(file.path(data_cleansed_dir,country_to_clean[country], city_to_clean[city], city_date_to_clean[date],data_csv_cleansed_name))){
            log4r_info(paste(c("Preparing data for",  city_to_clean[city], "at", city_date_to_clean[date], "compiled"), collapse = " "))
            prepare_data(country_to_clean[country],city_to_clean[city],city_date_to_clean[date])
          }
        }
      }
    }
  }
}
clean_data()
