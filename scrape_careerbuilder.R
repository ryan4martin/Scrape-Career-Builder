#---
#title: "Function to scrape CareerBuilder job postings"
#author: "Ryan Martin"
#---

library(tidyverse)
library(rvest)
library(furrr)

#Search terms for Career Builder consists of a character string
search_term <- c('Data analyst', 'Data scientist', 'Business analyst')
locations <- c('Montreal', 'Toronto')


job_listings <- scrape_careerbuilder(search_term = search_term, locations = locations)

#Function to scrape careerbuilder website
scrape_careerbuilder <-  function(search_term, locations) {
  #Use furrr to map functions in parallel
  plan(multisession)
  
  #name search_term vector
  names(search_term) <- search_term
  
  #Map over search terms and row bind results 
  combined_listing <- future_map_dfr(.x = search_term, .id = 'search_term', .progress = T, .f = function(x){
    #Change spaces to + in search term
    search_term <- map_chr(x, function(a) gsub(" ", "+", a))
    
    #Number of pages to read for each location
    locations <- locations
    pages <- c()
    
    #Obtain header from first page of search
    for (i in seq_along(locations)) {
      heading <- paste0('https://www.careerbuilder.ca/search?q=', search_term, '&loc=', locations[i], '&loc=&pg=1') %>%
        read_html() %>%
        html_nodes('h1') %>%
        html_text()
      #The header contains the number of search results
      #20 results are stored per page, divide total by 20 and round up to nearest integer
      pages[i] <- ceiling(read.table(text = heading, fill = T)[[1]] / 20)
    }
    names(pages) <- locations
    
    for (i in seq_along(locations)) {
      #Create empty lists for each feature collected
      title <- list()
      description_clean <- list()
      company_clean <- list()
      location_clean <- list()
      #Iterate through each location 
      for (a in 1:pages[locations[i]]) {
        url <-  paste0('https://www.careerbuilder.ca/search?q=', search_term, '&loc=', locations[i], '&loc=&pg=', a)
        webpage <- read_html(url)
        #Obtain job title from listing
        title[[a]] <- html_nodes(webpage, '.job-title') %>%
          html_text()
        #Flatten list object and store job titles from this page V
        title[[a]] <- unlist(title[[a]])
        #Obtain company from listing
        company <- html_nodes(webpage, '.text-center .show-for-large-up') %>%
          html_text()
        #Company information is returned with an empty character string every other string
        #Remove every other character string
        company <- company[c(T, F)]
        #Store company name from this page in list of all company name
        company_clean[[a]] <- company
        #Obtain company location from listing
        location <- html_nodes(webpage, '.large li:nth-child(1)') %>%
          html_text()
        #Clean character string to obtain city name only
        location <- strsplit(location, " ")
        for (b in seq_along(location)) {
          location[[b]] <- location[[b]][75]
          location[[b]] <- sub("\r\n","", location[[b]])
        }
        #Store locations from this page in list of all locations
        location_clean[[a]] <- unlist(location)
        #Obtain job description from listing
        description <- list()
        for(c in seq_along(title[[a]])) {
          description[[c]] <- html_session(url) %>%
            follow_link(title[[a]][c]) %>%
            read_html() %>%
            html_nodes('p') %>%
            html_text()
          #Description is read in with each line a sw
          description[[c]] <- paste(description[[c]], sep = ' ', collapse = ' ')
        }
        #Store descriptions from this page in list of all descriptions
        description_clean[[a]] <- unlist(description)
      }
      #Put results from all the pages at a certain location into the same list
      job_title <- list()
      job_title[[i]] <- title
      job_company <- list()
      job_company[[i]] <- company_clean
      job_location <- list()
      job_location[[i]] <- location_clean
      job_description <- list()
      job_description[[i]] <- description_clean
    }
    #Flatten lists containing job information from all the searchs 
    job_title <- unlist(job_title, recursive = T)
    job_company <- unlist(job_company, recursive = T)
    job_location <- unlist(job_location, recursive = T)
    job_description <- unlist(job_description, recursive = T)
    
    #Combined the job listing information into a data.frame
    combined_listing <- list()
    combined_listing <- pmap_dfr(list(job_title, job_company, job_location, job_description), 
                                 function(job_title, job_company, job_location, job_description) {
                                   as.data.frame(cbind(job_title, job_company, job_location, job_description)) 
                                 })
    return(combined_listing)
  })
}

