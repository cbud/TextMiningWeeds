#install.packages("dimensionsR")
library(dimensionsR)
library(tidyverse)
library(tictoc)
library(bib2df)
library(rscopus)
library(bibliometrix)
library(readxl)


#starts timer
tic()

#dimensions key
token <- dsAuth(key = "5D70B3712E044A41B40C922330029585")




#create list of weeds to search
#weed_list <- c("Cirsium vulgare", "Cuscuta campestris")


#creates the query string for dimensions

dimensions_query <- paste0("search publications in authors for \"Hossein Ghanizadeh\" return publications[type + basics + extras + authors + concepts + abstract]")

  query_result <- dsApiRequest(token = token, query = dimensions_query, step = 200, limit = 50000, verbose=TRUE)
  ds_temp <- dsApi2df(query_result)

  results <- biblioAnalysis(ds_temp, sep = ";")
  summary_stats<- summary(results)
  
  #plot(x = results, k = 10, pause = FALSE)
  
  toc()
  