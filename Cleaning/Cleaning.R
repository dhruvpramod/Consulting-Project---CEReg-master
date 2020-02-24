
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(janitor)
library(lubridate)


# Load data ---------------------------------------------------------------

df <- read_csv('2019-06-30ncdntcmprhnsv-eng.csv') %>% clean_names()

df <- df %>% rename(approx_vol_released = approximate_volume_released_m_u_00b3)

# Exploration -------------------------------------------------------------

glimpse(df)

summary(df)

# incident_type(s) --------------------------------------------------------

# There are differences between the two vars
df %>%
  select(reported_date,incident_type, incident_types) %>%
  mutate(flag = ifelse(incident_type == incident_types, FALSE, TRUE)) %>%
  filter(flag)

# nearest_population ------------------------------------------------------

df <- df %>%
  mutate(nearest_populated_centre = case_when(
    str_detect(nearest_populated_centre, 'Merritt') ~ 'Merritt',
    str_detect(nearest_populated_centre, 'Prophet River') ~ 'Prophet River',
    str_detect(nearest_populated_centre, 'Blueberry River') ~ 'Blueberry River',
    str_detect(nearest_populated_centre, 'Halfway River') ~ 'Halfway River',
    str_detect(nearest_populated_centre, 'Buick Creek') ~ 'Buick Creek',
    str_detect(nearest_populated_centre, 'Bonanza ') ~ 'Bonanza ',
    str_detect(nearest_populated_centre, 'Sikanni Chief') ~ 'Sikanni Chief',
    str_detect(nearest_populated_centre, 'Wonowon') ~ 'Wonowon', 
    str_detect(nearest_populated_centre, 'Altona ') ~ 'Altona',
    str_detect(nearest_populated_centre, 'Bear Lake') ~ 'Bear Lake',
    str_detect(nearest_populated_centre, 'Tumbler Ridge') ~ 'Tumbler Ridge', 
    str_detect(nearest_populated_centre, 'Kerrobert') ~ 'Kerrobert',
    str_detect(nearest_populated_centre, 'Grande Prairie') ~ 'Grande Prairie',
    str_detect(nearest_populated_centre, 'Chetwynd') ~ 'Chetwynd',
    TRUE ~ as.character(nearest_populated_centre)
  ))

provinces <- c('ON', 'BC', 'AB', 'British Columbia', 'Alberta', 'QC', 'Saskatchewan', 'SK', 'MB')

test <- df$nearest_populated_centre

for (prov in provinces) {
  test <- str_remove(test, prov)
}

test <- test %>% str_remove(',') %>% str_trim()

nearest_pop_cent <- test

df <- df %>% mutate(nearest_populated_centre = nearest_pop_cent)

# company ----------------------------------------------------------------

df %>% count(company)

df <- df %>%
  mutate(company = case_when(
    str_detect(company, 'Enbridge') ~ 'Enbridge Pipelines Inc.',
    str_detect(company, 'Foothills') ~ 'Foothills Pipe Lines Ltd.',
    str_detect(company, 'Pembina') ~ 'Pembina Energy Services Inc.',
    str_detect(company, 'Trans Mountain Pipeline') ~ 'Trans Mountain Pipeline ULC',
    TRUE ~ as.character(company)
  ))


# approx_vol_released ----------------------------------------------------------------------

df <- df %>%
  mutate(approx_vol_released = case_when(
    str_detect(approx_vol_released, 'Not Applicable') ~ '0',
    str_detect(approx_vol_released, 'Not Provided') ~ '0',
    TRUE ~ as.character(approx_vol_released)
  )) %>%
  mutate(approx_vol_released = as.numeric(approx_vol_released))


# reported date -----------------------------------------------------------

df <- df %>%
  mutate(reported_date = mdy(reported_date))

df <- df %>%
  mutate(reported_year = year(reported_date),
         reported_month = month(reported_date),
         reported_day = day(reported_date))


# ex ----------------------------------------------------------------------

df %>% 
  filter(approx_vol_released > 0) %>%
  ggplot(aes(x = reported_date, y = approx_vol_released)) +
  geom_point() +
  scale_y_log10()

df %>% 
  filter(approx_vol_released > 0) %>%
  ggplot(aes(x = factor(reported_month), y = approx_vol_released)) +
  geom_boxplot() +
  scale_y_log10()
