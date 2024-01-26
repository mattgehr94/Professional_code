#sample fake data that shows how time series data can be pumped into power BI for visualizations

library(dplyr)
library(tidyverse)
library(bigrquery)
library(ggplot2)
library(dplyr)
library(stats)
library(gridExtra)
library(tseries)
library(forecast)

qf_cases_raw<- read.csv("C:/Users/Matt/Downloads/SF_contact_data.csv")


qf_cases<- qf_cases_raw %>%
  filter(!Case.Type %in% "Tech Assist",
         Case.Origin %in% c("Automation","Email","Phone","Web"),
         Closed == 1,
         !Subtype %in% NA)%>%
  mutate(multiple_cases = duplicated(Account.Name)|
           duplicated(Account.Name, fromLast = T),
         Date.Time.Closed = na_if(Date.Time.Closed, "")) %>%
  drop_na(Date.Time.Opened, Date.Time.Closed) %>%
  separate(Date.Time.Opened, into =c("date_opened","time_opened"), sep = " ", remove = F) %>%
  separate(Date.Time.Closed, into=c("date_closed","time_closed"), sep = " ", remove = F) %>%
  mutate(
    date_closed = mdy(date_closed),
    date_opened = mdy(date_opened),
    month_opened = month(date_opened, label = TRUE),
    year_opened = year(date_opened),
    month_closed = month(date_closed, label = TRUE),
    year_closed = year(date_closed),
    day_opened = day(date_opened),
    day_closed = day(date_closed),
    subtype_simplified = case_when(grepl("Buried Service", Subtype)~"Buried Service Issue",
                                   grepl("Due Date", Subtype)~"Due Date Change",
                                   grepl("Facilities|Facility", Subtype)~"Facility Issue",
                                   grepl("Programming", Subtype)~"Programming Issue",
                                   TRUE~Subtype)
  ) %>%
  replace_na(list(time_closed = "00:00:00", time_opened = "00:00:00")) %>%
  add_count(Account.Name, name = "case_count") %>%
  mutate(case_count_user = case_when(case_count <= 3 ~"Light_caller",
                                     case_count >=4 & case_count<= 10 ~"Medium_caller",
                                     case_count >= 11 & case_count <=15 ~"Heavy_caller",
                                     case_count >= 16 ~"Extreme_caller")) %>%
  arrange(Account.Name, Date.Time.Opened) %>%
  group_by(Account.Name)


