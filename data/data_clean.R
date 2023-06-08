#data clean

librarian::shelf(tidyverse, tsibble, lubridate, glue, TimTeaFan/dplyover, zoo, TTR, fs, gt, openxlsx, 
                 snakecase, rlang, fredr, BrookingsInstitution/ggbrookings, ipumsr, here)

main_dir<- here()
data_raw<- glue("{main_dir}/data-raw")
data<- glue("{main_dir}/data")

# CPS data ----------------------------------------------------------------

#read data -------------------------
 setwd(data_raw)
# source("cps_00045.R")
cps<-read_rds("cps_raw.rds") %>% 
  rename_all(tolower) %>%
  filter(year>2015) %>% #restricting to 2016 onward for now 
  mutate(age = ifelse(age > 80, 80, age)) %>%
  filter(age >= 16, empstat != 0, empstat != 1)

cps_2019<- cps %>% filter(year == 2019) %>%
  mutate(agedum = as.numeric(cut(age, breaks = c(15, seq(16, 80, by = 1), Inf)))) %>%
  group_by(statefip, sex) %>% summarize(agedum = sum(agedum * wtfinl), age = age) %>% 
  pivot_longer(cols = c(agedum), names_to = "var", values_to = "val") %>%
  separate(var, c("var", "w"), sep = "\\*") %>% 
  mutate(w = as.numeric(w))
