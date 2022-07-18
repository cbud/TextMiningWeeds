#rscopus API key 	d2dcc4a928a10f0d9184f6ced6473c3c

#https://aurelien-goutsmedt.com/post/extracting-biblio-data-1/
#
library(rscopus)
library(bibliometrix)
library(tidyverse)

# Import and reformat Scopus data -----------------------------------------

set_api_key("d2dcc4a928a10f0d9184f6ced6473c3c")
hdr<-inst_token_header("272404b5b445f8a89b33cda259416973" )  
                 
#create list of weeds to search
weed_list <- c("Lolium perenne", "Arundo donax")

#create lists of abbreviated names
weed_list_abr <-  paste0(substr(weed_list, 1, 1), ". ",sub("^\\S+\\s+", '', weed_list))
weed_list_abr2 <-  paste0(substr(weed_list, 1, 1), " ",sub("^\\S+\\s+", '', weed_list))

#makes the string for Scopus query
QueryList <-
  paste0("TITLE-ABS-KEY(weed* OR invasi*) AND TITLE-ABS-KEY({", weed_list, "} OR {", weed_list_abr, "} OR {", weed_list_abr2, "})")

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
  if (exists("weed_papers") == F) {
   weed_papers <- species_papers
  }
  else{
   weed_papers <- bind_rows(weed_papers, species_papers)
  }
}

#remove special characters from column names
names(weed_papers) <- names(weed_papers) %>% 
  {gsub("prism:", "", .)}%>%
  {gsub("dc:", "", .)}%>%
  {gsub("-", "_", .)}%>%
  {gsub("@", "_", .)}%>%
  {gsub("[.$]", "", .)}

#removes duplicates based on weeds searched + title
weed_papers<-weed_papers %>%
  distinct(paste(tolower(title)," ",weed_searched), .keep_all=TRUE) %>% 
  select(-"paste(tolower(title), \" \", weed_searched)")
 
#adds lowercase title, abstract, and keywords to the "combined" column                                  
weed_papers$combined <- paste(tolower(weed_papers$title), tolower(weed_papers$description), tolower(weed_papers$authkeywords))

#creates "keep" column
weed_papers<- weed_papers %>% mutate(keep=TRUE)


#counts papers included before cleaning
before_cleaning<- weed_papers %>% 
  group_by(weed_searched) %>% 
  summarise(n()) 


# Clean data -----------------------------------------

#cleaning goes here (by setting keep=FALSE for bad papers)





# Output -----------------------------------------


after_cleaning<- weed_papers %>% 
  filter(keep==TRUE) %>%
  group_by(weed_searched) %>% 
  summarise(n())

before_cleaning
after_cleaning

#res = author_df(last_name = "Buddenhagen", first_name = "C", verbose = TRUE, general = FALSE)

# weed_data_raw <- gen_entries_to_df(weed_query$entries)
# weed_papers <- weed_data_raw$df
# weed_affiliations <- weed_data_raw$affiliation
# weed_authors <- weed_data_raw$author

# https://johnmuschelli.com/rscopus/articles/multi_author.html

#maybe adapt this code to query the species one by one?