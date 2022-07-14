#rscopus API key 	d2dcc4a928a10f0d9184f6ced6473c3c 

#https://aurelien-goutsmedt.com/post/extracting-biblio-data-1/
#
library(rscopus)
library(bibliometrix)

set_api_key("d2dcc4a928a10f0d9184f6ced6473c3c" )
res = author_df(last_name = "Buddenhagen", first_name = "C", verbose = TRUE, general = FALSE)

Species_full<-"Arundo donax"

weed_query <- rscopus::scopus_search("TITLE-ABS-KEY(weed OR invasiv*) AND TITLE-ABS-KEY(\"Arundo donax\") OR TITLE-ABS-KEY(\"A donax\")", 
                                     view = "COMPLETE")
weed_data_raw <- gen_entries_to_df(weed_query$entries)
weed_papers <- weed_data_raw$df
weed_affiliations <- weed_data_raw$affiliation 
weed_authors <- weed_data_raw$author

# https://johnmuschelli.com/rscopus/articles/multi_author.html
#maybe adapt this code to query the 
if (have_api_key()) {
  make_query = function(subj_area) {
    paste0("AF-ID(60006514) AND SUBJAREA(", 
           subj_area,
           ") AND PUBYEAR = 2018 AND ACCESSTYPE(OA)")
  }
  
  i = 3
  subj_area = subject_areas()[i]
  print(subj_area)
  completeArticle <- scopus_search(
    query = make_query(subj_area), 
    view = "COMPLETE", 
    count = 200)
  print(names(completeArticle))
  total_results = completeArticle$total_results
  total_results = as.numeric(total_results)
} else {
  total_results = 0
}