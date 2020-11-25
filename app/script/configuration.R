data_dir <- "data"
data_raw_dir <- "data/data_raw"
data_cleansed_dir <- "data/data_cleansed"
data_url_file = "data/all_data_urls.csv"
data_url_parse_file = "data/urls_parse.csv"
citys_names <- list.files(data_dir)
data_csv_cleansed_name <- "listings.csv"
city_variable <- c("All city's" = "all city",
  "Malaga" = "malaga",
  "Mallorca" = "mallorca",
  "Sevilla" = "sevilla")
city_variable_2 <- c("Malaga" = "malaga",
    "Mallorca" = "mallorca",
    "Sevilla" = "sevilla")
distribution_variable_y_list <- c("availability_30", "revenue_30","price_30")
filter_variable <- list("None","bedrooms", "room_type","neighbourhood_cleansed")
filter_variable_2 <- list("bedrooms", "room_type","neighbourhood_cleansed")
country_interset <- list("France", "Germany", "Spain")
