#rscopus API key 	d2dcc4a928a10f0d9184f6ced6473c3c 

#https://aurelien-goutsmedt.com/post/extracting-biblio-data-1/
#
library(rscopus)
library(bibliometrix)

set_api_key("d2dcc4a928a10f0d9184f6ced6473c3c" )
#res = author_df(last_name = "Buddenhagen", first_name = "C", verbose = TRUE, general = FALSE)

# Species_full<-"Arundo donax"
# 
# weed_query <- rscopus::scopus_search("TITLE-ABS-KEY(weed OR invasiv*) AND TITLE-ABS-KEY(\"Arundo donax\") OR TITLE-ABS-KEY(\"A donax\")", 
#                                      view = "COMPLETE")
# 
# weed_data_raw <- gen_entries_to_df(weed_query$entries)
# weed_papers <- weed_data_raw$df
# weed_affiliations <- weed_data_raw$affiliation 
# weed_authors <- weed_data_raw$author

# https://johnmuschelli.com/rscopus/articles/multi_author.html

#maybe adapt this code to query the species one by one?



weed_list<-c("Arundo donax", "Ageratina riparia", "Avena fatua")

#Makes a list of queries for each species.

QueryList <- paste0("TITLE-ABS-KEY(weed* OR invasi*) AND TITLE-ABS-KEY(", 
                weed_list, 
                ")")
QueryList

#queries one of the species in list

weed_query <- rscopus::scopus_search(QueryList[2], 
                                     view = "COMPLETE")

weed_data_raw2 <- gen_entries_to_df(weed_query$entries)
weed_papers2 <- weed_data_raw2$df
weed_affiliations2 <- weed_data_raw2$affiliation 
weed_authors2 <- weed_data_raw2$author


