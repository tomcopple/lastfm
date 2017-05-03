# Send list of artists to lastfm, get popular tags, then collate to get summary of genres. 
# NB Needs lastfm to exist in the global environment, but doesn't check for it. 

getTopGenres <- function(howManyArtists = 100) {
    library(tidyverse);library(lubridate);library(httr);library(jsonlite);library(zoo)
    
    # Save any data in Dropbox
    dirData <- "~/Dropbox/R/lastfm"
    
    # Set user = username, api = api_key
    source("scripts/setLastfm.R")
    user <- setUser()
    api <- setAPI()
    
    # Need a list of artists to look up; decide how many in function call
    topArtists <- lastfm %>% 
        count(artist) %>% 
        top_n(howManyArtists) %>% 
        arrange(desc(n))
    
    # Initialise an empty dataframe
    responseTags <- data_frame()
    
    # Then send each artist to lastfm to get top tag. Only keeping the top. 
    for(i in topArtists$artist) {
        
        # Can't remember why I needed to do this, think it might some weird encoding. 
        getArtist <- gsub("\ ", "+", i)
        getArtist <- gsub("&", "%26", getArtist)
        
        # Put together GET url
        baseurl <- paste0(
            "http://ws.audioscrobbler.com/2.0/?method=artist.gettoptags&artist=", getArtist,
            "&api_key=", api, "&format=json"
        )
        
        # Just send a courtesy message to console
        print(paste0("Hang on, just getting info for ", i, " [", 
                     length(topArtists$artist) - match(i, topArtists$artist),
                     "]"))
        
        # Send GET request
        responseRaw <- httr::GET(baseurl)
        httr::warn_for_status(responseRaw)
        
        # Convert JSON to text then a dataframe
        responseClean <- responseRaw %>% 
            httr::content(., as = "text") %>% 
            jsonlite::fromJSON(., simplifyDataFrame = T, flatten = T)
        
        # Convert response to a dataframe, then rbind to responseTags
        if(!is.numeric(responseClean$error) &
           !is_empty(responseClean$toptags$tag)) {
            responseTags <- rbind(
                responseTags,
                data.frame(artist = i, 
                           tag = tolower(head(responseClean$toptags$tag$name, 1)))
            )
        }
    }
    
    # Merge scrobbles with tags
    lastTags <- full_join(
        x = lastfm, y = responseTags
    ) %>% 
        na.omit()
    
    # Then going to plot changing genres over time, say top 10
    top10tags <- lastTags %>% 
        # Seen live isn't very helpful
        filter(tag != "seen live") %>% 
        count(tag) %>% 
        top_n(10)
    
    # Then get daily plays of each tag since the beginning, with a rolling monthly average
    daily <- lastTags %>% 
        mutate(date = lubridate::date(date)) %>% 
        filter(tag %in% top10tags$tag) %>% 
        select(tag, date) %>% 
        count(tag, date) %>% 
        spread(key = tag, value = n, fill = 0) %>% 
        right_join(
            x = .,
            y = data.frame(
                date = seq.Date(from = date(min(lastTags$date)),
                                to = date(max(lastTags$date)),
                                by = 1)
            )
        ) %>% 
        gather(., -date, key = tag, value = count) %>% 
        mutate(count = ifelse(is.na(count), 0, count)) %>% 
        group_by(tag) %>% 
        # NB Changing to 6 months (180 days) as was too volatile otherwise
        mutate(monthlyPlays = rollsum(count, k = 180, fill = NA, align = "right")) %>% 
        filter(!is.na(monthlyPlays)) %>% 
        as_data_frame()
    
    # Draw a plotly graph
    library(plotly)
    top10graph <- ggplot(daily, aes(x = date, y = monthlyPlays, fill = tag)) +
        geom_area(alpha = 0.6, size = 1.2, position = "fill") +
        scale_fill_brewer("", palette = "Spectral") +
        scale_x_date(date_labels = "%Y") +
        labs(x = "", y = "Monthly count", 
             title = "Top 10 genres over time")
    top10plotly <- ggplotly(top10graph) %>% 
        layout(margin = list(l = 60))
    
    print(top10plotly)
    
}