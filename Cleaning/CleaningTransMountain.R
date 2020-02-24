library(readxl)
library(dplyr)
library(stringr)

trans_mount_file <- 'Scraped PDF data/Condition 106 Construction Progress Report for September 2019 - A6Y4Y5.xlsx'

num_of_tbls <- length(excel_sheets(trans_mount_file))

trans_mount_data <- list()
for (i in 1:num_of_tbls) {
  trans_mount_data[[i]] <- read_excel(trans_mount_file, sheet = i)
  
  # names(trans_mount_data[i]) <- names(trans_mount_data[[i]][1])
  # names(trans_mount_data[[i]]) <- as.character(trans_mount_data[[i]][1,])
  # trans_mount_data[[i]] <- trans_mount_data[[i]][-1, ]
  
}

trans_mount_data

names(trans_mount_data[2]) <- names(trans_mount_data[[2]][1])
names(trans_mount_data[[2]]) <- as.character(trans_mount_data[[2]][1,])
trans_mount_data[[2]] <- trans_mount_data[[2]][-1, ]


# TransMountains Incidents ------------------------------------------------

incidents <- 
  readr::read_csv('Incidents data/2019-06-30ncdntcmprhnsv-eng.csv') %>%
  janitor::clean_names()

tm_incidents <- 
  incidents %>%
    rename(approx_vol_released = approximate_volume_released_m_u_00b3) %>%
    filter(stringr::str_detect(company, '[Tt]rans\\s*[Mm]ountain')) %>%
    # Coerce 'Not Applicable' and 'Not Provided' to NAs
    mutate(approx_vol_released = as.numeric(approx_vol_released))
  
  
incidents %>%
  readr::write_csv('Incidents data/TransMountainIncidents.csv')

