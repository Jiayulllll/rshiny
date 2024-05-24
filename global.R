library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(tidyr)
library(highcharter)
library(forcats)
library(magrittr)
library(networkD3)
library(shinydashboard)
library(colorspace)
# import dataset which is cleaned by last project
suicide_factor <- read.csv("suicide_rate.csv")
suicide_rate <- read.csv("original_suicide_rate.csv")
# get the sub-dataset to plot gender and age 
genderAge <- suicide_rate[, 2:8]
# get the country name
country <- c(unique(suicide_factor$Country))
all_country <- c("OECD", country)
age_group <- c("5-14 years", "15-24 years", "25-34 years", "35-54 years", "55-74 years", "75+ years")
gender <- c("female", "male")