library(shiny)
library(plotly)
library(jsonlite)
library(stringr)
library(RCurl)
library(magrittr)
library(purrr)
library(ggplot2)
library(tidyverse)
library(shinythemes)
library(leaflet)
#To access the API key in the .Renviron
source('.Renviron')
Sys.setenv("MAPBOX_TOKEN" = mapboxToken) # for Orca
#Access the data with the API keys
url_state <- paste("api.airvisual.com/v2/states?country=USA&key=", my_key, sep = '')
#Using Json to extract the data and unlist the data.
all_state <- httpGET(url_state)
all_state <- fromJSON(all_state)
all_state <- unlist(all_state$data)

#Create a dataframe of all data
all_state_list <- as.list(all_state)
#extract the names of each variables.
names(all_state_list) <- all_state