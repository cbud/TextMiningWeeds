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


weed_list <- c("Alternanthera philoxeroides", "Telanthera philoxeroides", "Achyranthes philoxeroides") 

#create lists of abbreviated names
weed_list_abr <-  paste0(substr(weed_list, 1, 1), ". ",sub("^\\S+\\s+", '', weed_list))
weed_list_abr2 <-  paste0(substr(weed_list, 1, 1), " ",sub("^\\S+\\s+", '', weed_list))

#creates the query string for dimensions
dimensions_query <- paste0("search publications in title_abstract_only for \"(\\\"invasi*\\\"OR\\\"introduced species\\\"OR\\\"toxicity\\\"OR\\\"dispersal\\\"OR\\\"poisoning\\\"OR\\\"vector\\\"OR\\\"invasive species\\\"OR\\\"invasive organisms\\\"OR\\\"weed control\\\")AND(\\\"",weed_list,"\\\"OR\\\"",weed_list_abr,"\\\"OR\\\"",weed_list_abr2,"\\\"OR\\\"Alligatorweed\\\")\" where year in [ 1900 : 2022 ] and type in [ \"article\" ] return publications[type + basics + extras + authors + concepts + abstract]")

#queries each species in list and adds to data table
for (i in 1:length(weed_list)) {
  query_result <- dsApiRequest(token = token, query = dimensions_query[i], step = 200, limit = 50000)
  ds_temp <- dsApi2df(query_result)
  ds_temp$weed_searched<- weed_list[i]
  cat("Searched species ", i, " of ", length(weed_list), " (", weed_list[i], ").")
  if (i == 1) {
    #  if (exists("weed_papers") == F) {
    dimensions_raw <- ds_temp
  }
  else{
    dimensions_raw <- bind_rows(dimensions_raw, ds_temp)
  }
}


papers<- dimensions_raw


papers$title <- papers$TI %>% str_replace_all(., "[[:punct:]]", "") %>% tolower()

#adds lowercase TI, AB, journal, and keywords to the "combined" column                                  
papers$combined <- paste(papers$title, tolower(papers$AB), tolower(papers$DE), tolower(papers$SO))

#creates status & reason column
papers<- papers %>% mutate(status="neutral", reason="")


# Section 2a. Clean data -----------------------------------------

#adds duplicates to removed_papers
removed_papers<- 
  filter(papers,duplicated(title)) %>%
  mutate(status="bad", reason="duplicate")

#removes duplicates from master list
papers <- papers %>%
  distinct(title, .keep_all=TRUE) %>% 
  select(-"title")

#create function to keep papers with certain words in the TI
keep_words <- function(input) {
  for (i in 1:length(input)) {
    papers <-
      papers %>%
      mutate(status = ifelse((grepl(input[i], tolower(SO))) == TRUE, "good", status)) %>%
      mutate(reason = paste0(ifelse(
        status == "good",
        paste0(ifelse(reason=="", input[i], paste0(reason, ", ", input[i])), " journal"),
        reason
      )))
  }
  return(papers)
}



#create function to remove papers with certain words in the combined column
remove_words <- function(input) {
  for (i in 1:length(input)) {
    
    to_remove <-
      papers %>% 
      filter(.,grepl(input[i],combined) & status != "good") %>% 
      mutate(status = "bad", reason = paste(input[i]))
    
    removed_papers <<- bind_rows(removed_papers,to_remove)
    
    papers <- 
      papers %>% 
      filter(!(grepl(input[i],combined)& status != "good"))
    
  }
  return(papers)
}


# Section 2b. clean data cont. ----------------------------------------------------------

#runs function to keep papers with these words in the Journal name
papers<-keep_words(list("toxicology", "weed"))

#runs function to exclude papers with these words anywhere
papers<-remove_words(list("insect pest", "pests","carnivore", "domesticated", "folk", "herbal", "essential oils", "soil organic matter", "cover crop"))

# Section 3 Output -----------------------------------------

#creates summary of kept papers by counting them, grouped by status and reason
kept_summary <- papers %>% 
  group_by(status, reason) %>% 
  summarise(n()) 


#renames summary columns
names(kept_summary) <- c("kept papers", "reason", "count")

#workaround that removes quotation marks that appeared in the reason column
kept_summary <- kept_summary %>% mutate(reason = noquote(reason))

#creates summary of removed papers by counting them, grouped by status and reason
removed_summary <- removed_papers %>% 
  group_by(status, reason) %>% 
  summarise(n())

#renames summary columns
names(removed_summary) <- c("removed papers", "reason", "count")

removed_summary

count_weeds<- count(papers, weed_searched)

results <- biblioAnalysis(papers)
summary_stats<- summary(results)

papers<- papers %>% select(., title = TI, author = AF, source = SO, url = URL, year = PY, doi = DI, abstract = AB, type = DT, keywords = DE, funding = FU, country = AU_CO, organisation = AU_UN, database = DB, weed_searched = weed_searched)

write.csv(papers, "alligator_weed.csv")

#displays run time
toc()

