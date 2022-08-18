#install.packages("dimensionsR")
library(dimensionsR)
library(tidyverse)
library(tictoc)
library(bib2df)
library(rscopus)
library(bibliometrix)
library(readxl)


# Sections
# 1. create search list
# 2. dimensions search
# 4. scopus search
# 4. join scopus and dimensions
# 5. remove duplicates
# 6. create functions to clean data
# 7. run functions
# 8. output


#starts timer
tic()


# Section 1: create search list -------------------------------------------


#weed_list <- read_excel("Small Data Files/gbif_all_synonyms_with_occurrence_counts_282_weeds_20220311.xlsx")
#weed_list <- weed_list$`Row Labels`

#create list of weeds to search
weed_list <- c("Cirsium vulgare", "Cuscuta campestris")


#create lists of abbreviated names
weed_list_abr <-  paste0(substr(weed_list, 1, 1), ". ",sub("^\\S+\\s+", '', weed_list))
weed_list_abr2 <-  paste0(substr(weed_list, 1, 1), " ",sub("^\\S+\\s+", '', weed_list))






# Section 2: Dimensions Search -------------------------------------------

#dimensions key
token <- dsAuth(key = "5D70B3712E044A41B40C922330029585")


#creates the query string for dimensions (can use concepts_scores. also can change search to full_data)
dimensions_query <- paste0("search publications in title_abstract_only for \"(\\\"weed\\\"OR\\\"invasi*\\\"OR\\\"introduced species\\\"OR\\\"invasive species\\\"OR\\\"invasive organisms\\\"OR\\\"alien invasive species\\\"OR\\\"invasive alien species\\\"OR\\\"weed control\\\")AND(\\\"",weed_list,"\\\"OR\\\"",weed_list_abr,"\\\"OR\\\"",weed_list_abr2,"\\\")\" where year in [ 1900 : 2022 ] and type in [ \"article\" ] return publications[type + basics + extras + authors + concepts + abstract + dimensions_url]")

#can search full_data or title_abstract_only

#currently searching for articles only

#Available fields: abstract,acknowledgements,altmetric,altmetric_id,arxiv_id,authors,authors_count,book_doi,book_series_title,book_title,category_bra,category_for,category_hra,category_hrcs_hc,category_hrcs_rac,category_icrp_cso,category_icrp_ct,category_rcdc,category_sdg,category_uoa,clinical_trial_ids,concepts,concepts_scores,date,date_inserted,date_online,date_print,dimensions_url,doi,field_citation_ratio,funder_countries,funders,id,issn,issue,journal,journal_lists,journal_title_raw,linkout,mesh_terms,open_access,pages,pmcid,pmid,proceedings_title,publisher,recent_citations,reference_ids,referenced_pubs,relative_citation_ratio,research_org_cities,research_org_countries,research_org_country_names,research_org_names,research_org_state_codes,research_org_state_names,research_orgs,researchers,resulting_publication_doi,source_title,subtitles,supporting_grant_ids,times_cited,title,type,volume,year and available fieldsets: basics,book,categories,extras

#loops through each species in list
for (i in 1:length(weed_list)) {
  ds_temp <- dsApiRequest(token = token, query = dimensions_query[i], step = 200, limit = 50000) #scopus query
  ds_temp <- dsApi2df(ds_temp) #convert to df
  ds_temp$weed_searched<- weed_list[i] #add weed searched to new column
  cat("Completed Dimensions search ", i, " of ", length(weed_list), " (", weed_list[i], ").\n") #displays progress 
  
  #if 
  if (i == 1) {
    dimensions_raw <- ds_temp
  }
  else{
    dimensions_raw <- bind_rows(dimensions_raw, ds_temp)
  }
}

#removes temp object
rm("ds_temp")


#renames columns, but leave a raw file.
dimensions<- dimensions_raw %>% select(., title = TI, author = AF, source = SO, url = URL, year = PY, doi = DI, abstract = AB, type = DT, keywords = DE, funding = FU, country = AU_CO, organisation = AU_UN, database = DB, weed_searched = weed_searched)

# url=dimensions_url doesnt work


# Section 3: Scopus search -----------------------------------------------

#scopus key
set_api_key("d2dcc4a928a10f0d9184f6ced6473c3c")
hdr<-inst_token_header("272404b5b445f8a89b33cda259416973" )


scopus_query <-
  paste0("TITLE-ABS-KEY(weed*  OR  invasi* OR {introduced species}  OR  {invasive species}  OR  {invasive organisms}  OR  {alien invasive species}  OR  {invasive alien species}  OR  {weed control}) AND TITLE-ABS-KEY({", weed_list, "} OR {", weed_list_abr, "} OR {", weed_list_abr2, "})")


for (i in 1:length(weed_list)) {
  sink("NUL")
  scopus_temp <- rscopus::scopus_search(scopus_query[i], view = "COMPLETE", headers = hdr)
  
  scopus_temp <- gen_entries_to_df(scopus_temp$entries)
  
  scopus_temp<-scopus_temp$df
  
  scopus_temp$weed_searched<- weed_list[i]
  sink()
  cat("Completed Scopus search ", i, " of ", length(weed_list), " (", weed_list[i], ").\n")
   
  if (i == 1) {
    scopus_raw <- scopus_temp
  }
  else{
    scopus_raw <- bind_rows(scopus_raw, scopus_temp)
  }
}

#removes temp objects
rm("scopus_temp")

#remove special characters from column names
names(scopus_raw) <- names(scopus_raw) %>% 
  {gsub("prism:", "", .)}%>%
  {gsub("dc:", "", .)}%>%
  {gsub("-", "_", .)}%>%
  {gsub("@", "_", .)}%>%
  {gsub("[.$]", "", .)}

#renames columns, but leave a raw file.
scopus<- scopus_raw %>% select(., title = title, author = creator, source = publicationName, url = url, date = coverDate, doi = doi, abstract = description, type = subtypeDescription, keywords = authkeywords, funding = fund_sponsor, weed_searched = weed_searched)

#adds "scopus" to the database variable for scopus searches
scopus<- scopus %>% mutate(database="scopus")







# Section 4: Join scopus and Dimensions, create new columns --------------

#joins scopus and dimensions tables
papers <- bind_rows(scopus, dimensions)

#makes titles lowercase and removes punctuation (to increase ability to detect duplicates).
papers$title <- papers$title %>% str_replace_all(., "[[:punct:]]", "") %>% tolower()

#adds lowercase title, abstract, journal, and keywords to the "combined" column                                  
papers$combined <- paste(papers$title, tolower(papers$abstract), tolower(papers$keywords), tolower(papers$source))

#creates status & reason column
papers<- papers %>% mutate(status="neutral", keep_reason=" ", exclude_reason = " ")





# Section 5. remove duplicates -----------------------------------------

#adds duplicates to removed_papers (i tried using doi but there are too many NA's)
duplicate_papers<- 
  filter(papers, duplicated(paste(title," ",weed_searched))) %>%
  mutate(status="bad", exclude_reason="duplicate")

#removes duplicates from master list based on title & weed
papers <- papers %>%
  distinct(paste(title," ",weed_searched), .keep_all=TRUE) 

#remove the temporary column created in the last step
papers <- select(papers, 1:(length(papers)-1))

# Section 6: create loop functions to look for key words -----------------

#create function to keep papers with certain words in the keywords
keep_keywords <- function(input) {
  for (i in 1:length(input)) {
    papers <-
      papers %>%
      mutate(status = ifelse((grepl(input[i], tolower(keywords))) == TRUE, "good", status)) %>%
      mutate(keep_reason = paste0(ifelse(
        status == "good",
        paste0(ifelse(keep_reason==" ", input[i], paste0(keep_reason, ", ", input[i])), " <KW>"),
        keep_reason
      )))
  }
  return(papers)
}


#create function to keep papers with certain words in the journal name
keep_journal <- function(input) {
  for (i in 1:length(input)) {
    papers <-
      papers %>%
      mutate(status = ifelse((grepl(input[i], tolower(source))) == TRUE, "good", status)) %>%
      mutate(keep_reason = paste0(ifelse(
        status == "good",
        paste0(ifelse(keep_reason==" ", input[i], paste0(keep_reason, ", ", input[i])), " <JNL>"),
        keep_reason
      )))
  }
  return(papers)
}


#create function to remove papers with certain words in the combined column
remove_words <- function(input) {
  
  #because the loop below will add to this if its in your environment from the last run.
  rm("removed_papers")
  
  for (i in 1:length(input)) {
    if (exists("removed_papers") == T) {
     removed_papers_updated <- removed_papers %>%
       filter(.,grepl(input[i],combined)) %>% 
mutate(status = "bad", exclude_reason =  paste0(exclude_reason, ", ", input[i]))
     
     removed_papers <<- 
       removed_papers %>% 
       filter(!(grepl(input[i],combined)))
     
     removed_papers <<- bind_rows(removed_papers_updated,removed_papers)
}
     to_remove <-
      papers %>% 
      filter(.,grepl(input[i],combined) & status != "good") %>% 
      mutate(status = "bad", exclude_reason  =  paste(input[i]))
    
    papers <- 
      papers %>% 
      filter(!(grepl(input[i],combined)& status != "good"))
    
    if (i == 1) {
      removed_papers <<- to_remove
    }
    else{
      removed_papers <<- bind_rows(removed_papers,to_remove)
    
    }

    }
  
  return(papers)
}




# Section 7. run functions ----------------------------------------------------------

#runs function to keep papers with these words in the keywords
papers<-keep_keywords(list("herbicide resistance", "invasive plant"))

#runs function to keep papers with these words in the Journal name
papers<-keep_journal(list("weed", "invasions"))


#runs function to exclude papers with these words anywhere
papers<-remove_words(list("cover crop performance", "insect pest", "pests","breeding","pests","medicin","medical", "cancer",  "carnivore", "domesticated", "folk", "herbal", "essential oils", "cm soil", "soil organic matter", "cultivar", "honey"))
#ignore the "object 'removed_papers' not found" error message above

# Section 8 Output -----------------------------------------

removed_papers <- bind_rows(removed_papers,duplicate_papers)
rm("duplicate_papers")

#creates summary of kept papers by counting them, grouped by status and reason
kept_summary <- papers %>% 
  group_by(status, keep_reason) %>% 
  summarise(n()) 


#renames summary columns
names(kept_summary) <- c("kept papers", "reason", "count")

#workaround that removes quotation marks that appeared in the reason column
kept_summary <- kept_summary %>% mutate(reason = noquote(reason))


#creates summary of removed papers by counting them, grouped by status and reason
removed_summary <- removed_papers %>% 
  group_by(status, exclude_reason) %>% 
  summarise(n())

#renames summary columns
names(removed_summary) <- c("removed papers", "reason", "count")

#prints outputs
removed_summary
kept_summary
count_weeds<- count(papers, weed_searched)
count_weeds

#displays run time
toc()
