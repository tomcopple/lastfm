# Enter an artist and see which albums you've listened to the most
# NB This function sends a query to lastfm to get the track numbers, so don't go crazy

getArtistPlays <- function(enterArtist, refresh = FALSE) {
    
    # Get lots of libraries
    library(tidyverse);library(lubridate);library(httr);library(jsonlite)
    library(forcats);library(scales);library(plotly);library(magrittr);library(stringr)
    
    # Set basedir; scripts are set relative to this
    basedir <- "~/R/lastfm"
    source(file.path(basedir, "scripts/setLastfm.R"))
    source(file.path(basedir, "scripts/getLastfm.R"))
    
    # Refresh lastfm dataframe if necessary
    if(refresh) {
        getLastfm(T)
    } else {
        if(!exists("lastfm")) {
            return("Make sure you either refresh or run getLastfm first")
        }
    }
    
    # Want to remove as many duplicates and weirdness as possible.
    # So create a function to convert every track name and album name to a lowercase slug, remove punctuation.
    getSlugs <- function(x) {
        x1 <- stringr::str_to_lower(x)
        x2 <- stringr::str_replace_all(x1, "[:punct:]", "")
        x3 <- stringr::str_replace_all(x2, "\\s{2,}", " ")
        x4 <- stringr::str_trim(x3, side = "both")
        return(x4)
    }
    
    # Get list of albums to lookup from lastfm
    albumList <- lastfm %>%
        filter(artist == enterArtist,
               album != "",
               # Don't want to include Pitchfok compilations
               # Probably some other compilations too?
               !(grepl("Pitchfork", album))) %>% 
        group_by(album) %>% 
        count() %>% 
        # How many plays to keep? ~10 is one whole play of most albums
        filter(n > 10) %>% 
        magrittr::extract2('album')
    
    # Count scrobbles now, easier than doing it later.
    countScrobbles <- lastfm %>% 
        mutate(album = getSlugs(album), track = getSlugs(track)) %>% 
        filter(artist == enterArtist, 
               # Keep blanks, try to match against lastfm database. 
               album %in% c(getSlugs(albumList), "")) %>% 
        group_by(album, track) %>% 
        mutate(scrobbles = n()) %>% 
        ungroup() %>% 
        distinct(album, track, scrobbles)
    
    # Function to submit album title and artist to lastfm try to download tracklist. 
    getAlbumInfo <- function(x) {
        baseurl <- "http://ws.audioscrobbler.com/2.0/?method=album.getinfo&api_key="
        
        # Some special characters cause problems; replace. Might not need %20
        y <- gsub(" ", "%20", x)
        y <- gsub("#", "%23", y)
        z <- gsub(" ", "%20", enterArtist)
        
        # Paste GET request
        url <- paste0(baseurl, setAPI(), 
                      "&artist=", z,
                      "&album=", y, "&format=json")
        
        # Send GET request for album
        responseRaw <- httr::GET(url)
        httr::stop_for_status
        
        # Convert from json to dataframe
        responseClean <- responseRaw %>% 
            httr::content(., as = "text") %>% 
            jsonlite::fromJSON(., simplifyDataFrame = T, flatten = T) 
        
        # If it returned tracks, separate into dataframe
        if(!is.null(responseClean$album$tracks)) {
            if(length(responseClean$album$tracks$track) > 0) {
                responseTracks <- responseClean$album$tracks$track %>% 
                    select(track = name, trackNum = `@attr.rank`) %>% 
                    mutate(album = responseClean$album$name,
                           trackNum = as.integer(trackNum)) 
            } else {
                responseTracks <- data_frame(
                    track = "", trackNum = NA, album = x
                )
            }
            # Otherwise just return null 
        } else {
            responseTracks <- data_frame(
                track = "", trackNum = NA, album = x
            )
        }
        
        return(responseTracks)
    }
    albumInfo <- map_df(albumList, getAlbumInfo)
    
    # Want to merge this list of tracks with scrobbles
    # First, try to fill in blank albums with other scrobbles.
    fillBlanks1 <- countScrobbles %>% 
        # Join to itself by track, will try to match album
        # Arrange so blank tracks are at the bottom. 
        left_join(., arrange(countScrobbles, desc(album)), 
                  by = "track") %>% 
        # Remove duplicates from original data
        distinct(album.x, scrobbles.x, track, .keep_all = TRUE) %>% 
        # If original was blank, fill with new (best guess)
        mutate(album = ifelse(album.x != "", album.x, album.y)) %>% 
        # NB scrobbles.x is the original count
        select(track, album, scrobbles = scrobbles.x) %>% 
        # Then add together scrobbles which have been matched. 
        group_by(track, album) %>% 
        summarise(scrobbles = sum(scrobbles)) %>% 
        ungroup()
    
    # Second, try matching any remaining blanks with info from lastfm
    fillBlanks2 <- left_join(
        x = fillBlanks1, 
        y = albumInfo %>% mutate(track = getSlugs(track)) %>% select(-trackNum), 
        by = 'track'
    ) %>% 
        mutate(album = ifelse(album.x != "", album.x, album.y)) %>% 
        select(-album.x, -album.y)
    
    # Now try to add track numbers in lastfm data
    getTrackNums <- left_join(
        x = fillBlanks2 %>% rename(matchTrack = track, matchAlbum = album),
        y = albumInfo %>% mutate(matchTrack = getSlugs(track), matchAlbum = getSlugs(album)),
        by = c('matchTrack', 'matchAlbum')
    ) %>% 
        filter(!is.na(matchAlbum)) %>% select(-track)
    
    # Some track numbers will fail, need to make a best guess, but avoid duplication. 
    # Function to apply to each album to guess the track number. 
    getBlankTracks <- function(dataframe) {
        if(anyNA(dataframe$trackNum)) {
            tempBlank <- filter(dataframe, is.na(trackNum))
            possNums = c()
            for(i in 1:tempBlank$trackCount) {
                if(i %in% dataframe$trackNum) {
                    next
                } else {
                    possNums <- c(possNums, i)
                }
            }
            possNums <- possNums[c(1:nrow(tempBlank))]
            for(i in 1:length(possNums)) {
                tempBlank$trackNum[i] <- possNums[i]
            }
            bind_rows(
                filter(dataframe, !is.na(trackNum)), 
                tempBlank)
            
        } else {
            dataframe
        }
    }
    
    getTrackNums2 <- getTrackNums %>%
        group_by(matchAlbum) %>% 
        mutate(trackCount = n()) %>% 
        # Nest into list, then apply function to each album individually
        nest() %>% 
        mutate(data = map(data, getBlankTracks)) %>% 
        unnest() %>% 
        # Also try to fill in missing album data
        arrange(matchAlbum, album) %>% 
        fill(album)
    
    
    # Should be ready to plot a graph
    getAlbums <- select(getTrackNums2, matchAlbum, matchTrack, scrobbles, trackNum) %>% 
        arrange(matchAlbum, trackNum)
    
    howManyAlbums <- length(unique(getAlbums$matchAlbum))
    howManyRows <- ifelse(howManyAlbums %% 3 == 0, howManyAlbums %/% 3, (howManyAlbums %/% 3) + 1)
    
    albums <- getAlbums %>% group_by(matchAlbum) %>% summarise(total = sum(scrobbles)) %>%
        arrange(desc(total)) %>% extract2('matchAlbum')
    
    # Only keep max 12 albums
    if(howManyAlbums > 12) {
        keep <- getAlbums %>% group_by(matchAlbum) %>% summarise(total = sum(scrobbles)) %>% 
            top_n(n = 12, wt = total) %>% extract2('matchAlbum')
        getAlbums <- filter(getAlbums, matchAlbum %in% keep)
    }
    
    plotlyCol <- brewer_pal(palette = 'Paired')(12)
    
    plots <- list()
    for(i in 1:howManyAlbums) {
        
        album <- str_to_title(albums[i])
        albumData <- 
            filter(getAlbums, matchAlbum == albums[i])
        plots[[i]] <- plot_ly(
            albumData, x = ~trackNum, y = ~scrobbles,
            text = ~paste0(matchTrack, ": ", scrobbles), hoverinfo = "text",
            type = "scatter", mode = "lines+markers", 
            marker = list(color = plotlyCol[i]),
            line = list(color = plotlyCol[i])
        ) %>%
            layout(title = paste("Album plays by", enterArtist),
                xaxis = list(title = "", showgrid=FALSE, zeroline=F,
                                  tickformat = ",d"),
                     yaxis = list(title = "", showgrid=FALSE, zeroline=F,
                                  tickformat = ",d",
                                  range = c(0, ~max(getAlbums$scrobbles)))

            )
    }
    
    finalP <- subplot(plots, nrows = howManyRows, shareY = FALSE, shareX = FALSE,
                      margin = c(0.02, 0.02, 0.08, 0.02)) %>% 
        layout(showlegend = FALSE)
    
   # Try to add annotations separately?
    for(i in 1:howManyAlbums) {
        finalP <- finalP %>% 
            layout(annotations = list(
                x = 1,
                xanchor = "left", xref = paste0("x", i),
                y = 1, yanchor ="auto", yref = paste0("y", i),
                text = albums[i], showarrow = F,
                bgcolor = "#FFF", bordercolor = "#D9D9D9"
            ))
    }
    
    print(finalP)
}


