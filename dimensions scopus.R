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

#scopus key
set_api_key("d2dcc4a928a10f0d9184f6ced6473c3c")
hdr<-inst_token_header("272404b5b445f8a89b33cda259416973" )



weed_list <- read_excel("Small Data Files/gbif_all_synonyms_with_occurrence_counts_282_weeds_20220311.xlsx")
weed_list <- weed_list$`Row Labels`

#create list of weeds to search
#weed_list <- c("Cirsium vulgare", "Cuscuta campestris")


#create lists of abbreviated names
weed_list_abr <-  paste0(substr(weed_list, 1, 1), ". ",sub("^\\S+\\s+", '', weed_list))
weed_list_abr2 <-  paste0(substr(weed_list, 1, 1), " ",sub("^\\S+\\s+", '', weed_list))

#creates the query string for dimensions

dimensions_query <- paste0("search publications in title_abstract_only for \"(\\\"weed\\\"OR\\\"invasi*\\\"OR\\\"introduced species\\\"OR\\\"invasive species\\\"OR\\\"invasive organisms\\\"OR\\\"alien invasive species\\\"OR\\\"invasive alien species\\\"OR\\\"weed control\\\")AND(\\\"",weed_list,"\\\"OR\\\"",weed_list_abr,"\\\"OR\\\"",weed_list_abr2,"\\\")\" where year in [ 1900 : 2022 ] and type in [ \"article\" ] return publications[type + basics + extras + authors + concepts + abstract]")

#queries each species in list and adds to data table
for (i in 1:length(weed_list)) {
  query_result <- dsApiRequest(token = token, query = dimensions_query[i], step = 200, limit = 50000)
  ds_temp <- dsApi2df(query_result)
  ds_temp$weed_searched<- weed_list[i]
  paste0("Searched species ", i, " of ", length(weed_list), " (", weed_list[i], ").\n")
  
  if (i == 1) {
    #  if (exists("weed_papers") == F) {
    dimensions_raw <- ds_temp
  }
  else{
    dimensions_raw <- bind_rows(dimensions_raw, ds_temp)
  }
}


dimensions<- dimensions_raw %>% select(., title = TI, author = AF, source = SO, url = URL, year = PY, doi = DI, abstract = AB, type = DT, keywords = DE, funding = FU, country = AU_CO, organisation = AU_UN, database = DB, weed_searched = weed_searched)

scopus_query <-
  paste0("TITLE-ABS-KEY(weed*  OR  invasi* OR {introduced species}  OR  {invasive species}  OR  {invasive organisms}  OR  {alien invasive species}  OR  {invasive alien species}  OR  {weed control}) AND TITLE-ABS-KEY({", weed_list, "} OR {", weed_list_abr, "} OR {", weed_list_abr2, "})")


for (i in 1:length(weed_list)) {
  weed_query <-
    rscopus::scopus_search(scopus_query[i], view = "COMPLETE", headers = hdr)
  scopus_temp <- gen_entries_to_df(weed_query$entries)
  scopus_temp<-scopus_temp$df
  scopus_temp$weed_searched<- weed_list[i]
  if (i == 1) {
    #  if (exists("weed_papers") == F) {
    scopus_raw <- scopus_temp
  }
  else{
    scopus_raw <- bind_rows(scopus_raw, scopus_temp)
  }
}

#removes temp objects
rm("ds_temp", "scopus_temp")

#remove special characters from column names
names(scopus_raw) <- names(scopus_raw) %>% 
  {gsub("prism:", "", .)}%>%
  {gsub("dc:", "", .)}%>%
  {gsub("-", "_", .)}%>%
  {gsub("@", "_", .)}%>%
  {gsub("[.$]", "", .)}

scopus<- scopus_raw %>% select(., title = title, author = creator, source = publicationName, url = url, date = coverDate, doi = doi, abstract = description, type = subtypeDescription, keywords = authkeywords, funding = fund_sponsor, weed_searched = weed_searched)

scopus<- scopus %>% mutate(database="scopus")


papers <- bind_rows(scopus, dimensions)


papers$title <- papers$title %>% str_replace_all(., "[[:punct:]]", "") %>% tolower()

#adds lowercase title, abstract, journal, and keywords to the "combined" column                                  
papers$combined <- paste(papers$title, tolower(papers$abstract), tolower(papers$keywords), tolower(papers$source))

#creates status & reason column
papers<- papers %>% mutate(status="neutral", reason="")


# Section 2a. Clean data -----------------------------------------

#adds duplicates to removed_papers
removed_papers<- 
  filter(papers,duplicated(paste(tolower(title)," ",weed_searched))) %>%
  mutate(status="bad", reason="duplicate")

#removes duplicates from master list
papers <- papers %>%
  distinct(paste(title," ",weed_searched), .keep_all=TRUE) %>% 
  select(-"paste(title, \" \", weed_searched)")



#create function to keep papers with certain words in the title
keep_words <- function(input) {
  for (i in 1:length(input)) {
    papers <-
      papers %>%
      mutate(status = ifelse((grepl(input[i], tolower(source))) == TRUE, "good", status)) %>%
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
papers<-keep_words(list("weed"))

#runs function to exclude papers with these words anywhere

papers<-remove_words(list("cover crop performance", "insect pest", "pests","breeding","pests","medicin","medical", "cancer",  "carnivore", "domesticated", "folk", "herbal", "essential oils", "cm soil", "soil organic matter"))


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

#displays run time
toc()
