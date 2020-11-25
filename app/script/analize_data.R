library(dplyr)
library(stringr)
library(ggplot2)
library(data.table)
library(lubridate)
source("script/log_utils.R")
#source("script/data_preparation.R")
#files_paths_to_data_cleansed <- get_listings(1)

#listings <-do.call(rbind,lapply(files_paths_to_data_cleansed, read.csv, row.names=1))

country_list <- list.files(data_cleansed_dir)
#country_list <- append(list.files(data_cleansed_dir), "all country")
country_list_input <- append(country_list,"all country")

cities_list <- c()
for(country in 1:length(country_list)){
    cities_list<- append(cities_list,list.files(file.path(data_cleansed_dir, country_list[country])))
}


cities_list_input <- append(cities_list,"all city")

get_listings <- function(number_date){
  file_path_to_data <- c()
  for(country in 1:length(country_list)){
      for(city in 1:length(cities_list)){
        for(n_date in 1:number_date){
          if(!(NA %in% list.files(file.path(data_cleansed_dir, country_list[country],cities_list[city]))[n_date])){
            file_path_to_data <- append(file_path_to_data,file.path(data_cleansed_dir, country_list[country],cities_list[city],list.files(file.path(data_cleansed_dir, country_list[country],cities_list[city]))[n_date]))
          }
        }
      }
  }
  listings <-do.call(rbind,lapply(file.path(file_path_to_data,"listings.csv"), read.csv, row.names=1))

  listings$bedrooms <- ifelse(listings$bedrooms >= 5, "5+", listings$bedrooms)
  listings$data_date <- ymd(listings$data_date)
  return(listings)
}

get_min_date <- function(dataset){
  return(min(dataset$data_date))
}
get_max_date <- function(dataset){
  return(max(dataset$data_date))
}

get_cities_by_country <- function(input){
  if("all country" %in% input == FALSE){
    cities_list <- c()
    for(i in 1:length(input)){
      cities_list <- append(cities_list,list.files(file.path(data_cleansed_dir ,input[i])))
    }
    cities_list_by_country <- append(cities_list,"all city")
  }else{
    cities_list_by_country <- cities_list_input
  }
  return(cities_list_by_country)
}

get_cities_by_country_2 <- function(input){
    cities_list <- c()
    for(i in 1:length(input)){
      cities_list <- append(cities_list,list.files(file.path(data_cleansed_dir ,input[i])))
    }
  return(cities_list)
}

get_dataset_neighbourhood_cleansed<- function(neighbourhood_percent, city, date){
  listings <- get_listings(date)
  listings_neighbourhood_cleansed <- listings %>% group_by(city, neighbourhood_cleansed) %>% summarise(count = n())%>% mutate(freq = count / sum(count))

  listings_neighbourhood_cleansed_filter <- listings_neighbourhood_cleansed %>% filter(between(freq,neighbourhood_percent[1],neighbourhood_percent[2])) %>% select(neighbourhood_cleansed)
  return(filter(listings,listings$city ==city, listings$neighbourhood_cleansed %in%  listings_neighbourhood_cleansed_filter$neighbourhood_cleansed))
}
get_max_percet_of__neighbourhood_cleansed <- function(city_, date){
  listings_neighbourhood_cleansed <- get_listings(date) %>% group_by(city, neighbourhood_cleansed) %>% summarise(count = n())%>% mutate(freq = count / sum(count))
  listings_neighbourhood_cleansed_freq <- listings_neighbourhood_cleansed %>% filter(city == city_) %>% select(freq)
  return(max(listings_neighbourhood_cleansed_freq$freq))
}

get_dataset_neighbourhood_cleansed_map<- function(neighbourhood_percent, city, date){
  listings <- get_listings(date)
  listings_neighbourhood_cleansed <- listings %>% group_by(city, neighbourhood_cleansed) %>% summarise(count = n())%>% mutate(freq = count / sum(count))
  listings_neighbourhood_cleansed_filter <- listings_neighbourhood_cleansed %>% filter(between(freq,neighbourhood_percent[1],neighbourhood_percent[2])) %>% select(neighbourhood_cleansed)
  return(
    filter(listings,listings$city ==city, listings$neighbourhood_cleansed %in%  listings_neighbourhood_cleansed_filter$neighbourhood_cleansed)
    %>% select(longitude,latitude,property_type,city,revenue_30,availability_30,price,neighbourhood_cleansed,room_type)
    %>% mutate(price = str_replace(price, "\\$", ""))
    %>% mutate(price = str_replace(price, ",", ""))
    %>% mutate(price = as.numeric(price))
  )
}



            ## Preprocess


get_mean <- function(dataset){
  setNames(aggregate(dataset$availability_30, list(dataset$city), mean),c("city","means"))
}


#mean_availability_30 <- setNames(aggregate(listings$availability_30, list(listings$city), mean),c("city","means"))

#mean_revenue_30 <- setNames(aggregate(listings$revenue_30, list(listings$city), mean),c("city","means"))
