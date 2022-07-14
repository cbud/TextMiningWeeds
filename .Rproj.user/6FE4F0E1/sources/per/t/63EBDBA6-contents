#rscopus API key 	d2dcc4a928a10f0d9184f6ced6473c3c 

#https://aurelien-goutsmedt.com/post/extracting-biblio-data-1/
#
library(rscopus)
library(bibliometrix)

set_api_key("d2dcc4a928a10f0d9184f6ced6473c3c" )
res = author_df(last_name = "Buddenhagen", first_name = "C", verbose = TRUE, general = FALSE)

Species_full<-"Arundo donax"

weed_query <- rscopus::scopus_search("TITLE-ABS-KEY(weed) AND TITLE-ABS-KEY(\"Arundo donax\") OR TITLE-ABS-KEY(\"A. donax\")", 
                                     view = "COMPLETE")
weed_data_raw <- gen_entries_to_df(weed_query$entries)
weed_papers <- weed_data_raw$df
weed_affiliations <- weed_data_raw$affiliation 
weed_authors <- weed_data_raw$author

convert2df(weed_papers, dbsource = "scopus")
results <- biblioAnalysis(weed_papers, sep = ";")
