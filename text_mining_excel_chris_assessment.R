#this script imports and collates data from the csv file weeds_paper_chris_assess.csv 
#then attempts to clean out bad papers and checks the results against this file


library(tidyverse)
library(tictoc)

tic()

# import data -------------------------------------------------------------

weed_papers_raw <- read_csv("Small Data Files/weeds_paper_chris_assess.csv")

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


# Clean data -----------------------------------------

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
      mutate(reason = ifelse(
        status == "good",
        paste0(ifelse(reason=="", input[i], paste0(reason, ", ", input[i])), " journal"),
        reason
      ))
  }
  return(weed_papers)
}



#create function to remove papers with certain words in the combined column

remove_word <- function(input) { #creates function
  for (i in 1:length(input)) { #loops i times, where i is the number of input words
    
    to_remove <-
      weed_papers %>% 
      filter(.,grepl(input[i],combined) & status != "good") %>% #search for input word, but not in "good" papers
      mutate(status = "bad", reason = paste(input[i])) #make status bad and reason is input word
    
    removed_papers <<- bind_rows(removed_papers,to_remove) #adds to the removed paper table
    #<< makes it edit the global variable
    
    weed_papers <- 
      weed_papers %>% 
      filter(!(grepl(input[i],combined)& status != "good")) #removes these papers from main table
    
  }
  return(weed_papers)
}


#runs functions with input words as list of characters

weed_papers<-keep_words(list("weed"))

weed_papers<-remove_word(list("cover crop performance", "insect pest", "pests","breeding","pests","medicin","medical", "cancer",  "carnivore", "domesticated", "folk", "herbal", "essential oils", "cm soil", "soil organic matter"))


# Section 3 Output -----------------------------------------

#creates summary of kept papers by counting them, grouped by status and reason
kept_summary<- weed_papers %>% 
  group_by(GoodBad, reason) %>% 
  summarise(n()) 


#renames summary columns
names(kept_summary) <- c("kept papers", "reason", "count")

#workaround that removes quotation marks that appeared in the reason column
kept_summary<-kept_summary %>% mutate(reason=noquote(reason))

#creates summary of removed papers by counting them, grouped by status and reason
removed_summary <- removed_papers %>% 
  group_by(GoodBad, reason) %>% 
  summarise(n()) 

#renames summary columns
names(removed_summary) <- c("removed papers", "reason", "count")


#display results
removed_summary
kept_summary


#displays run time
toc()
