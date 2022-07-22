#this script queries the scopus database for papers mentioning the species in the weed list in section 1
#to try different cleaning words, change terms in section 2b then rerun sections 2b and 3


#rscopus API key 	d2dcc4a928a10f0d9184f6ced6473c3c

#https://aurelien-goutsmedt.com/post/extracting-biblio-data-1/
#
library(rscopus)
library(bibliometrix)
library(tidyverse)
library(tictoc)

# Section 1. Import and reformat Scopus data -----------------------------------------

set_api_key("d2dcc4a928a10f0d9184f6ced6473c3c")
hdr<-inst_token_header("272404b5b445f8a89b33cda259416973" )

#starts run time timer
tic()

#create list of weeds to search
weed_list <- c("Cirsium vulgare", "Chenopdium album")

#create lists of abbreviated names
weed_list_abr <-  paste0(substr(weed_list, 1, 1), ". ",sub("^\\S+\\s+", '', weed_list))
weed_list_abr2 <-  paste0(substr(weed_list, 1, 1), " ",sub("^\\S+\\s+", '', weed_list))

#makes the string for Scopus query
QueryList <-
  paste0("TITLE-ABS-KEY(weed*  OR  invasi* OR {introduced species}  OR  {invasive species}  OR  {invasive organisms}  OR  {alien invasive species}  OR  {invasive alien species}  OR  {weed control}) AND TITLE-ABS-KEY({", weed_list, "} OR {", weed_list_abr, "} OR {", weed_list_abr2, "})")

#queries one of the species in list
# weed_query <- rscopus::scopus_search(QueryList[2], view = "COMPLETE")
# weed_data_raw <- gen_entries_to_df(weed_query$entries)
# weed_papers <- weed_data_raw$df
# weed_affiliations <- weed_data_raw$affiliation
# weed_authors <- weed_data_raw$author

#queries each species in list and adds to data table
for (i in 1:length(weed_list)) {
  weed_query <-
    rscopus::scopus_search(QueryList[i], view = "COMPLETE", headers = hdr)
  weed_data_raw <- gen_entries_to_df(weed_query$entries)
  species_papers<-weed_data_raw$df
  species_papers$weed_searched<- weed_list[i]
  if (i == 1) {
    #  if (exists("weed_papers") == F) {
    weed_papers_raw <- species_papers
  }
  else{
    weed_papers_raw <- bind_rows(weed_papers_raw, species_papers)
  }
}

#removes temp object
rm("species_papers")


#remove special characters from column names
names(weed_papers_raw) <- names(weed_papers_raw) %>% 
  {gsub("prism:", "", .)}%>%
  {gsub("dc:", "", .)}%>%
  {gsub("-", "_", .)}%>%
  {gsub("@", "_", .)}%>%
  {gsub("[.$]", "", .)}

#will use weed papers from here to keep raw data intact
weed_papers<-weed_papers_raw


#makes titles lowercase and removes punctuation
weed_papers$title <- weed_papers$title %>% str_replace_all(., "[[:punct:]]", "") %>% tolower()

#adds lowercase title, abstract, journal, and keywords to the "combined" column                                  
weed_papers$combined <- paste(weed_papers$title, tolower(weed_papers$description), tolower(weed_papers$authkeywords), tolower(weed_papers$publicationName))

#creates status column
weed_papers<- weed_papers %>% mutate(status="neutral", reason="")


# Section 2a. Clean data -----------------------------------------

#adds duplicates to removed_papers
removed_papers<- 
  filter(weed_papers,duplicated(paste(tolower(title)," ",weed_searched))) %>%
  mutate(status="bad", reason="duplicate")

#removes duplicates from master list
weed_papers <- weed_papers %>%
  distinct(paste(title," ",weed_searched), .keep_all=TRUE) %>% 
  select(-"paste(title, \" \", weed_searched)")



#create function to keep papers with certain words in the title
keep_words <- function(input) {
  for (i in 1:length(input)) {
    weed_papers <-
      weed_papers %>%
      mutate(status = ifelse((grepl(input[i], tolower(publicationName))) == TRUE, "good", status)) %>%
      mutate(reason = paste0(ifelse(
        status == "good",
        paste0(ifelse(reason=="", input[i], paste0(reason, ", ", input[i])), " journal"),
        reason
      )))
  }
  return(weed_papers)
}



#create function to remove papers with certain words in the combined column
remove_word <- function(input) {
  for (i in 1:length(input)) {
    
    to_remove <-
      weed_papers %>% 
      filter(.,grepl(input[i],combined) & status != "good") %>% 
      mutate(status = "bad", reason = paste(input[i]))
    
    removed_papers <<- bind_rows(removed_papers,to_remove)
    
    weed_papers <- 
      weed_papers %>% 
      filter(!(grepl(input[i],combined)& status != "good"))
    
  }
  return(weed_papers)
}


# Section 2b. clean data cont. ----------------------------------------------------------

#runs function to keep papers with these words in the Journal name
weed_papers<-keep_words(list("weed"))

#runs function to exclude papers with these words anywhere
weed_papers<-remove_word(list("insect pest", "breeding","pests", "wildlife","medicin","medical", "cancer",  "carnivore", "domesticated", "folk", "herbal", "essential oils"))

# Section 3 Output -----------------------------------------

#creates summary of kept papers by counting them, grouped by status and reason
kept_summary<- weed_papers %>% 
  group_by(status, reason) %>% 
  summarise(n()) 


#renames summary columns
names(kept_summary) <- c("kept papers", "reason", "count")

#workaround that removes quotation marks that appeared in the reason column
kept_summary<-kept_summary %>% mutate(reason=noquote(reason))

#creates summary of removed papers by counting them, grouped by status and reason
removed_summary <- removed_papers %>% 
  group_by(status, reason) %>% 
  summarise(n())

#renames summary columns
names(removed_summary) <- c("removed papers", "reason", "count")


#display results
removed_summary
kept_summary

#displays run time
toc()

#res = author_df(last_name = "Buddenhagen", first_name = "C", verbose = TRUE, general = FALSE)

# weed_data_raw <- gen_entries_to_df(weed_query$entries)
# weed_papers <- weed_data_raw$df
# weed_affiliations <- weed_data_raw$affiliation
# weed_authors <- weed_data_raw$author

# https://johnmuschelli.com/rscopus/articles/multi_author.html

#maybe adapt this code to query the species one by one?