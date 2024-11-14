#install.packages("rjson")
library(rjson)
library(tidyverse)

json_to_frame <- function(json_file){
  #Function converts a single JSON file into a dataframe
  json_data <- fromJSON(file=json_file)
  #Figuring this out was a massive pain
  #Basically, make each observation its own dataframe and rbind them
  salary_df <- do.call(rbind, 
                       lapply(json_data$data, 
                              function(x) {
                                x[sapply(x, is.null)] <- NA #Null and NA in R is weird
                                as.data.frame(x )  #, stringsAsFactors = FALSE
                              }))
  return(salary_df)
}

generate_full_frame <- function(files){
  #Function takes a list of JSON files(with year as name) and binds them into a single dataframe
  full_frame <- data.frame()
  for (file in files){
    year <- basename(file)
    year_df <- json_to_frame(file)
    year_df$year <- year
    
    full_frame <- rbind(full_frame, year_df)
  }
  #Force correct data types
  full_frame$year <- as.integer(full_frame$year)
  full_frame$comp <- as.numeric(full_frame$comp)
  return(full_frame)
}

#List all JSON files
json_files <- list.files(path = "./data/salary/json",
               pattern = "*", 
               full.names = TRUE)

#Build a complete dataframe
full_df <- generate_full_frame(json_files)

#Location and departments are stored in a single string - split
#I Give up trying to do this without tidyverse
full_df <- full_df %>% 
  separate("dept", into = c("location", "department"), sep = " - ")

#Save it (Drop the "long_text" column)
write.csv(full_df[, setdiff(names(full_df), "long_text")], 
          "./data/salary/salary.csv", 
          row.names=FALSE, quote=TRUE)
