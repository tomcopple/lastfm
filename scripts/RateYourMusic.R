## Is it possible to download track ratings from RateYourMusic?

library(tidyverse);library(httr);library(jsonlite);library(rvest)


rymURL <- "https://rateyourmusic.com/collection/tomcopple2/track_ratings"

rymRaw <- rvest::read_html(rymURL)

## Full table
rymTab <- rymRaw %>% html_elements(".mbgen")

rymTab %>% html_elements('tr')

## Separate out into individual album
rymTab %>% html_elements('.or_q_review') %>% html_text()

## Album artist/name
rymRaw %>% html_elements('.or_q_albumartist') %>% 
    html_text2()

## Artist name - NB sometimes multiple artists separated out
rymRaw %>% html_elements('.artist') %>% html_text()

## Album name 
rymRaw %>% html_elements('.album') %>% html_text()

rymTab %>% html_element('.trackratings') %>% html_text2()
