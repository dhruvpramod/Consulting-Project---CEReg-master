
# NOVA Gas PDF Report Scraper ---------------------------------------------

## The purpose of this code is to scrape any progress report in PDF format
## provided by NOVA Gas Ltd. to the CER, into structured tables in an Excel
## workbook.

# Load libraries used
library(stringr)    # Working with strings/text
library(tabulizer)  # Extracting tables
library(tibble)     # Nicer dataframes
library(tidyr)      # Tidying dataframes
library(xlsx)       # Writing data
library(dplyr)      # Data wrangling
library(janitor)    # Cleaning column names

# Folder where PDFs are located
path <- 'PDFs/NOVA/' 

# Create vector with all file names
pdfs <- list.files(path, pattern = '.pdf') 

# Indicate the file of interest to scrape HERE 
file <- paste0(path, pdfs[4])

# Extracts tables from PDF and returns a list of matrices with found tbls
tbls <- tabulizer::extract_tables(file) 

## Note: {tabulizer} does it's best but the tables in these PDFs are poorly
## formatted for proper extraction so a lot of formatting needs to be done.

# Convert matrices to tibbles and repair column names
for (i in 1:length(tbls)) {
  tbls[[i]] <- as_tibble(tbls[[i]])
  names(tbls[[i]]) <- tbls[[i]][1, ]
  tbls[[i]] <- tbls[[i]][-1, ]
  tbls[[i]] <- clean_names(tbls[[i]], 'upper_camel')
}

# Detect tbls that were extracted but empty
emptytbls <- c()
for (i in 1:length(tbls)) {
  if (nrow(tbls[[i]]) == 0){
    emptytbls <- append(emptytbls, i)
  }
}

# Remove empty tbls
if (!is.null(emptytbls)) {
  tbls <- tbls[-emptytbls]
}

# Formatting function ----------------------------------------------------------------

formatting_function <- function(table) {
  
  ## Function takes a tbl from a NOVA Gas Ltd. report and formats tbls based on
  ## how {tabulizer} extracts tbls similarly from every NOVA Gas report
  
  for (i in 1:ncol(table)) {
    
    if(str_starts(names(table[,i]), 'X')) {
      
      # Find empty columns and remove
      if((table[1,i] == '' && table[3,i] == '')) {
        table <- select(table, -i)
      } else {
        # Fix names that are supposed to be blank
        names(table)[i] <- ''
      }
    }
  }
  
  # Fix tables where the columns need to be shifted to the left
  if (str_detect(str_flatten(names(table)), 'PercentComplete')) {
    
    find_blanks <- c()
    for (i in 1:nrow(table)) {
      if (table[i, 'PercentComplete'] == '') {
        find_blanks <- c(find_blanks, i)
      }
    }
    
    if (length(find_blanks) > 0) {
      
      tbl <- rbind(
        table %>%
          slice(1:(min(find_blanks)-1)),
        table %>%
          slice(min(find_blanks):max(find_blanks)) %>%
          mutate(PercentComplete = Activity, Activity = Station, Station = ''),
        if(max(find_blanks) != nrow(table)) {
          table %>%
            slice((max(find_blanks)+1):nrow(table))
        }
      )
      
    }
    
  }
  
  return(table)
}


# Format tables --------------------------------------------------------

# Loop to format every tbl extracted from PDF
formatted_tbls <- list()
for (i in 1:length(tbls)) {
  formatted_tbls[[i]] <- formatting_function(tbls[[i]])
}

# Combine tables that span pages ------------------------------------------

# Code to combine tbls that are read as seperate tbls but need to be bound together
if(length(formatted_tbls) > 1) {
  
  remove_tbls <- c()
  
  for (i in 2:length(formatted_tbls)) {
    
    if ((length(formatted_tbls[[i-1]]) == length(formatted_tbls[[i]]))) {
      
      if((all(names(formatted_tbls[[i-1]]) == names(formatted_tbls[[i]])))) {
        formatted_tbls[[i]] <- rbind(formatted_tbls[[i-1]], formatted_tbls[[i]])
        remove_tbls <- c(remove_tbls, i-1)
      }
    }
  }
  structured_tables <- formatted_tbls[-remove_tbls]
} else {
  structured_tables <- formatted_tbls
}

# Write data to excel workbook ------------------------------------------

# Create name for excel workbook to be the same as the pdf name
file_name <- paste0(str_remove(pdfs[4], '.pdf'), '.xlsx') 

# Write first tbl to the first sheet of the excel workbook
write.xlsx(formatted_tbls[[1]], file = file_name, sheetName = 'Table 1')

# Append the rest of the tbls to the excel workbook in different sheets
if (length(structured_tables) > 1) {
  for (i in 2:length(structured_tables)) {
    sheet_name <- paste('Table', i)
    write.xlsx(structured_tables[[i]],
               file_name,
               sheetName = sheet_name,
               append = TRUE)
  }
}
