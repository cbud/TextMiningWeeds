#this script imports and collates data from the endnote files Good weed references.bib and Some problem reference example.bib 
#then attempts to clean out bad papers and checks the results against these files 

#Vignette source information: https://www.bibliometrix.org/vignettes/Introduction_to_bibliometrix.html #see also package help

# Load packages
library(bib2df)
library(bibliometrix)
library(tidyverse)
library(rscopus)
library(readr)

#Not synching large files on github
#dir()
# [1] "Large Data Files"      "Outputs"               "README.md"             "Small Data Files"     
# [5] "TextMiningWeeds.Rproj" "Weed_text_mining.r"  

#Note it looks like it is better to download from one recognized source through an API for bibliometrix package
#GW<-convert2df(path2file, dbsource = "scopus", format = "endnote")
#results <- biblioAnalysis(GW, sep = ";")
#Note it looks like it is better to download from one recognized source through an API for bibliometrix package
#GW<-convert2df(path2file, dbsource = "scopus", format = "endnote")
#path2file<-"Small Data Files/Good weed references.bib"
#GW<-bib2df(path2file, separate_names = TRUE)
#path2file<-"Small Data Files/Some problem reference examples.bib"
#BW<-bib2df(path2file, separate_names = TRUE)


#import data
good_papers<- bib2df("Small Data Files/Good weed references.bib", separate_names = TRUE)%>%
  mutate(status="good") 
bad_papers<- bib2df("Small Data Files/Some problem reference examples.bib", separate_names = TRUE)%>% 
  mutate(status="bad") 
before_clean <- bind_rows(good_papers, bad_papers)


#adds lowercase title, abstract, and keywords to new column 
before_clean$combined <- paste(tolower(before_clean$TITLE), tolower(before_clean$ABSTRACT), tolower(before_clean$KEYWORDS), tolower(before_clean$JOURNAL), (before_clean$BOOKTITLE), (before_clean$CHAPTER))
before_clean$TITLE <- before_clean$TITLE %>% str_replace_all(., "[[:punct:]]", "")
good_papers$TITLE <- good_papers$TITLE %>% str_replace_all(., "[[:punct:]]", "")

#remove duplicates
cleaning<-before_clean %>%
  distinct(paste(tolower(TITLE)), .keep_all=TRUE) %>% 
  select(-"paste(tolower(TITLE))") %>% #remove temporary column
  mutate(keep="good") #create keep column

#remove duplicates another way
#removed_weeds<- filter(before_clean,duplicated(paste(tolower(before_clean$TITLE)))) %>% mutate(keep="duplicate")

#add duplicates back in, with keep = duplicate
cleaning<- before_clean %>% left_join(cleaning, by = names(.))
cleaning$keep[is.na(cleaning$keep)] <- "duplicate"

#create function to remove papers with certain words in the combined column
remove_word <- function(input) {
  for (i in 1:length(input)) {
    removed <- cleaning[grep(input[i], cleaning$combined), ]  #subset rows based on input word
    
    removed <- removed %>% #filter(keep!="duplicate") %>%
      mutate(keep = paste0(ifelse(keep == "good", "", paste0(keep, ", ")), input[i])) #replace keep with input word + any other exclusion words
    
    cleaning <-
      cleaning %>% anti_join(removed, by = "TITLE") %>% full_join(removed, by = names(.)) #adds subset rows back onto main weed paper list
  }
  return(cleaning)
}

#create function to keep papers with certain words in the journal name
keep_words <- function(input) {
  for (i in 1:length(input)) {
    to_add <- cleaning[grep(input[i], tolower(cleaning$JOURNAL)), ]  #subset rows based on input word
    
    to_add <- to_add %>% filter(!(grepl("duplicate",keep))) %>%
      mutate(keep = "include") #replace keep with input word + any other exclusion words
    
    after_clean <-
      after_clean %>% anti_join(to_add, by = "TITLE") %>% full_join(to_add, by = names(.)) #adds subset rows back onto main weed paper list
  }
  return(after_clean)
}

#runs the two functions above. You can run this bit separately with different terms.
after_clean<-remove_word(list("insect pest","pests", "wildlife","medicin", "cover crop","medical", "cancer",  "carnivore", "domesticated", "folk", "herbal", "essential oils"))

after_clean<-keep_words(list("herbicide"))

#possible others: folk, chemistry, herbal, essential oils

#calculates erroneous removals
missed_bad <- after_clean %>% filter(keep=="good" | keep=="include") %>% anti_join(good_papers)
excluded_good <- after_clean %>% filter(keep!="good" & keep!="include") %>% merge(good_papers)

#shows summary of papers excluded
after_clean%>% 
  group_by(keep) %>%
  summarise(n()) %>% 
  arrange(-.[2]) #arrange by 2nd column, descending

#sum(a$`n()`) #sum of papers

paste(nrow(missed_bad), "missed bad papers")
paste(nrow(excluded_good), "excluded good papers")

