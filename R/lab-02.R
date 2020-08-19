#Liam Finn
#Geog 176A, Lab 02
#August 17th, 2020

install.packages("tidyverse")
install.packages("knitr")
install.packages("readxl")
install.packages("zoo")
install.packages("ggthemes")
install.packages("scales")

library(tidyverse)
library(knitr)
library(readxl)
library(zoo)
library(ggthemes)
library(scales)

#LOAD DATA
covid_url=("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
covid= read_csv(covid_url)  #covid= data from nytimes
  
#STEP2: subset Covid Data -> filters data to CA Counties
CA_covid= covid%>%
  filter(state=="California")%>%
  group_by(county) %>%
  mutate(newCases = c(cases[1], diff(cases))) %>%
  ungroup() %>%
  filter(date >=max(date)-13)

#STEP 3
#Most cumulative cases
CA_cumulative= CA_covid %>%
  filter(date==max(date)) %>%
  slice_max(cases, n=5) %>%
  select(county,cases)
#Most new cases
CA_new= CA_covid %>%
  filter(date==max(date)) %>%
  slice_max(newCases, n=5) %>%
  select(county, newCases)

#make table using Knitr::Kable
knitr::kable(CA_cumulative,
             caption = "Counties with Most Cumulative Cases",
             col.names = c("County", "Total Cases"))
knitr::kable(CA_new,
             caption = "Counties with Most New Cases",
             col.names = c("County", "Total Cases"))

#STEP4/5 : Load population data
library(readxl)
pop <- read_excel("data/PopulationEstimates.xls", skip = 2) %>%#changed from PopulationEstimates
  select(pop2019 = POP_ESTIMATE_2019, fips = FIPStxt) #change column names

#STEP 6 #examine data
names(pop)
dim(pop)
names(CA_covid)
dim(CA_covid)

#STEP 7
#join COVID & Population data

all_data= left_join(covid, pop, by='fips')

last_14_days= all_data %>%
  filter(state==c("California")) %>%
  group_by(county, date) %>%
  summarize(totalcases=sum(cases, na.rm = TRUE),
            pop2019= sum(pop2019, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(date>=max(date)-13)%>%
  group_by(county,pop2019)%>%
  mutate(newCases=totalcases-lag(totalcases))%>%
  summarize(totalcases= sum(newCases,na.rm = TRUE)) %>%
  mutate(per100= totalcases/ (pop2019/100000)) %>%
  filter(per100 <=100) %>%
  pull(county)
last_14_days

knitr::kable(last_14_days, caption = paste('Safe counties across State'),
             col.names = c('County'))
