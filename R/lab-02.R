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

pop <- read_excel("data/PopulationEstimates.xls", skip = 2) #changed from PopulationEstimates

pop%>%
  select(c(fips= "FIPStxt", state= "State", pop2019="POP_ESTIMATE_2019"))  #change column names
  
#STEP2: subset Covid Data -> filters data to CA Counties
CA_covid= covid%>%
  filter(state=="California")%>%
  group_by(county) %>%
  mutate(newCases= cases- lag(cases))%>%
  ungroup()%>%
  filter(date==max(date))%>%
  select(state, county, cases, newCases) 

#STEP 3
#Most cumulative cases
CA_cumulative= CA_covid[order(-CA_covid$cases),]
head(CA_cumulative, 5)
#Most new cases
CA_new=CA_covid[order(-CA_covid$newCases),]
head(CA_new, 5)

#make table using Knitr::Kable
knitr::kable(CA_cumulative,
             caption = "Counties with Most Cumulative Cases",
             col.names = c("State", "County", "Total Cases", "New Cases"))
knitr::kable(CA_new,
             caption = "Counties with Most New Cases",
             col.names = c("State", "County", "Total Cases", "New Cases"))



#STEP4/5 : Load population data