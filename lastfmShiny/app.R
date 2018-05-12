library(shiny);library(shinymaterial);library(plotly);library(tidyverse);library(RColorBrewer);library(lubridate);library(rdrop2);library(zoo);library(magrittr)

# Define a function to get lastfm data. 
source('getLastfmShiny.R')

# Run the function
lastfm <- getLastfmShiny()
# lastfm <- read.csv("lastfm.csv", stringsAsFactors = F, fileEncoding = "UTF-16LE") %>% 
#     mutate(date = lubridate::ymd_hms(date)) %>% 
#     as_data_frame()

artistList <- unique((lastfm %>% distinct(artist, album, track) %>%
                          filter(album != "", !grepl("unknown", artist)) %>% 
                          group_by(artist) %>% 
                          filter(n() > 25) %>% 
                          arrange(artist))$artist)

# options(shiny.autoreload = TRUE)
# options(shiny.reactlog=TRUE) 

ui <- material_page(
    
    # Application title
    title = "Lastfm Dashboard",
    
    nav_bar_color = "indigo",
    
    # Want to try having a main tab here first?
    material_tabs(
        tabs = c(
            "Main page" = "main_page",
            "Artist page" = "artist_page"
        )
    ),
    
    material_tab_content(
        tab_id = "main_page",
        tags$h2("Summary of recent plays"),
        
        material_row(
            material_column(
                width = 3,
                material_card(
                    title = "",
                    material_radio_button(
                        input_id = "chooseType",
                        label = "Select top 10:",
                        color = "blue",
                        choices = c("Artists", "Albums", "Tracks")
                    )
                ),
                material_card(
                    title = "",
                    material_radio_button(
                        input_id = "chooseYear",
                        label = "Select year:",
                        color = "green",
                        choices = seq.int(from = year(max(lastfm$date)), 
                                          to = year(min(lastfm$date)))
                    )
                )
            ),
            material_column(
                width = 9, 
                material_card(title = "", plotlyOutput("top10plotly", height = "100%"))
                )
            )
        ),
    
    material_tab_content(
        tab_id = "artist_page",
        material_row(
            material_column(
                width = 3,
                material_card(
                    title = NULL,
                    material_dropdown(
                        input_id = "chooseArtist",
                        label = "Choose artist",
                        color = "red",
                        choices = artistList,
                        selected = first(lastfm$artist)
                    )
                ),
                material_card(
                    title = NULL,
                    material_button(
                        input_id = "refresh",
                        label = "Refresh data",
                        icon = "refresh"
                    )
                )
            ),
            material_column(
                width = 9,
                material_row(
                    material_card(
                        title = "", plotlyOutput("trackPlays", height = "100%")
                    )
                ),
                material_row(
                    material_card(title = "", plotlyOutput("albumPlays", height = "100%"))
                )
            )
        )
        
    )
    
    # # Put sidenav here if you want one
    # material_side_nav(
    #     material_dropdown(
    #         input_id = 'chooseArtist',
    #         label = 'Choose artist',
    #         color = 'red',
    #         choices = artistList,
    #         selected = first(lastfm$artist)
    #     ),
    #     fixed = TRUE
    # ),
    # 
    # # Define tabs
    # material_tabs(
    #     tabs = c(
    #         "Tracks over time" = "first_tab",
    #         "Album plays" = "second_tab"
    #     )
    # ),
    # 
    # # Define tab content
    # material_tab_content(
    #     tab_id = "first_tab",
    #     tags$h2("Tracks over time"),
    #     plotlyOutput('trackPlays')
    # ),
    # material_tab_content(
    #     tab_id = "second_tab",
    #     tags$h2("Album plays"),
    #     plotlyOutput("artistPlays")
    # )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    values <- reactiveValues()
    values$lastfm <- lastfm
    
    observeEvent(input$refresh, {
        if(input$refresh > 0) {
            material_spinner_show(session, "trackPlays")
            material_spinner_show(session, "albumPlays")
            shiny::showNotification("Refreshing data, may take some time...", type = "default")
            values$lastfm <- getLastfmShiny()
            shiny::showNotification("Done!", type = "message")
            material_spinner_hide(session, "trackPlays")
            material_spinner_hide(session, "albumPlays")
        }
        
    })
    
    output$top10plotly <- renderPlotly({
        # Filter for year
        chooseYear <- input$chooseYear
        chooseType <- input$chooseType
        top10data <- filter(values$lastfm, year(date) == chooseYear)
        
        # If it's tracks or albums, need to make sure it's the right one
        # Just do three functions for each one, too complicated otherwise
        if(chooseType == "Artists") {
            top10 <- top10data %>% 
                count(artist) %>% 
                top_n(10) %>% 
                magrittr::extract2('artist')
            
            ## Must be an easier way of doing this, but going to split into lists
            ## then rejoin together afterwards?
            top10plays <- top10data %>% 
                
                ## Filter for top 10 artists
                filter(artist %in% top10) %>% 
                select(artist, date) %>% 
                
                ## Get total count of plays for each day
                mutate(date = lubridate::as_date(date)) %>% 
                count(artist, date) %>% 
                
                ## Convert to list column
                nest(-artist) %>% 
                
                ## Merge with a dataframe containing a sequence of days for the whole year
                mutate(dateSeq = list(data_frame(
                    date = seq.Date(from = dmy(paste0("01-01-", chooseYear)), 
                                    to   = dmy(paste0("01-12-", chooseYear)), by = 1)))) %>% 
                mutate(fullJoin = map2(data, dateSeq, full_join, by = "date")) %>% 
                
                ## Get rolling 30 day count for graph (cum sum for first month)
                mutate(fullJoin = map(fullJoin, arrange, date)) %>% 
                mutate(fullJoin = map(fullJoin, mutate, n = ifelse(is.na(n), 0, n))) %>% 
                mutate(fullJoin = map(fullJoin, mutate, 
                                       plays = c(cumsum(n[1:29]),
                                                 zoo::rollsum(n, k = 30, align = "right")))) %>% 
                unnest(fullJoin) %>% 
                
                ## Just renaming to x for the graph, not important. 
                rename(x = artist)
            
            ## Seems to work
                
        } else if(chooseType == "Albums") {
                
            ## Just do the exact same thing but for albums. I'm sure this can be simplified. 
            ## Actually, keep artists to make sure there aren't 2 albums with the same name. 
            top10 <- top10data %>% 
                count(artist, album) %>% 
                top_n(10) %>% 
                unite(col = "album", artist, album) %>% 
                extract2('album')
            
            ## Must be an easier way of doing this, but going to split into lists
            ## then rejoin together afterwards?
            top10plays <- top10data %>% 
                unite(col = "album", artist, album) %>% 
                filter(album %in% top10) %>% 
                select(album, date) %>% 
                mutate(date = lubridate::as_date(date)) %>% 
                count(album, date) %>% 
                nest(-album) %>% 
                mutate(dateSeq = list(data_frame(
                    date = seq.Date(from = dmy(paste0("01-01-", chooseYear)), 
                                    to   = dmy(paste0("01-12-", chooseYear)), by = 1)))) %>% 
                mutate(fullJoin = map2(data, dateSeq, full_join, by = "date")) %>% 
                mutate(fullJoin = map(fullJoin, arrange, date)) %>% 
                mutate(fullJoin = map(fullJoin, mutate, n = ifelse(is.na(n), 0, n))) %>% 
                mutate(fullJoin = map(fullJoin, mutate, 
                                      plays = c(cumsum(n[1:29]),
                                                zoo::rollsum(n, k = 30, align = "right")))) %>% 
                unnest(fullJoin) %>% 
                separate(album, into = c('artist', 'album'), sep = "_") %>% 
                mutate(x = stringr::str_c(album, "<br>[", artist, "]"))
        } else {
            top10 <- top10data %>% 
                count(artist, track) %>% 
                top_n(10) %>% 
                unite(col = "track", artist, track) %>% 
                extract2('track')
            
            ## Must be an easier way of doing this, but going to split into lists
            ## then rejoin together afterwards?
            top10plays <- top10data %>% 
                unite(col = "track", artist, track) %>% 
                filter(track %in% top10) %>% 
                select(track, date) %>% 
                mutate(date = lubridate::as_date(date)) %>% 
                count(track, date) %>% 
                nest(-track) %>% 
                mutate(dateSeq = list(data_frame(
                    date = seq.Date(from = dmy(paste0("01-01-", chooseYear)), 
                                    to   = dmy(paste0("01-12-", chooseYear)), by = 1)))) %>% 
                mutate(fullJoin = map2(data, dateSeq, full_join, by = "date")) %>% 
                mutate(fullJoin = map(fullJoin, arrange, date)) %>% 
                mutate(fullJoin = map(fullJoin, mutate, n = ifelse(is.na(n), 0, n))) %>% 
                mutate(fullJoin = map(fullJoin, mutate, 
                                      plays = c(cumsum(n[1:29]),
                                                zoo::rollsum(n, k = 30, align = "right")))) %>% 
                unnest(fullJoin) %>% 
                separate(track, into = c('artist', 'track'), sep = "_") %>% 
                mutate(x = stringr::str_c(track, "<br>[", artist, "]")) 
        }
        
        top10plays %>% 
            group_by(x) %>% 
            plot_ly(x = ~date, y = ~plays, color = ~x, type = "scatter", mode = "lines",
                    fill = "tozeroy", colors = "Spectral", 
                    text = ~stringr::str_c(x, "<br>Plays: ", plays), hoverinfo = "text") %>% 
            layout(xaxis = list(title = ""),
                   yaxis = list(title = "Monthly plays"),
                   title = stringr::str_c("Top ten ", tolower(chooseType), " in ", chooseYear),
                   height = "100%")
    })
    
    
    # Want to remove as many duplicates and weirdness as possible.
    # Function to convert every track name and album name to a lowercase slug,
    # remove punctuation etc.
    getSlugs <- function(x) {
        x1 <- stringr::str_to_lower(x)
        x2 <- stringr::str_replace_all(x1, "&", "and")
        x3 <- stringr::str_replace_all(x2, "[:punct:]", "")
        x4 <- stringr::str_replace_all(x3, "\\s{2,}", " ")
        x5 <- stringr::str_trim(x4, side = "both")
        return(x5)
    }
    
    ## Filter scrobbles by artist for both graphs
    
    observeEvent(input$chooseArtist, {
        
        values$chooseArtist <- stringr::str_replace_all(
            input$chooseArtist, "_shinymaterialdropdownspace_", " "
        )
        
        values$plays <- values$lastfm %>% 
            filter(artist == values$chooseArtist) %>% 
            mutate(track = getSlugs(track), album = getSlugs(album)) %>%
            ## summarise album if < 10 plays
            add_count(album) %>% 
            mutate(albumGroup = ifelse(n < 10 | album == "", "others", album)) %>% 
            mutate(album = forcats::as_factor(album))
            
    })
    
    ## first tab: plotly of tracks over time (rough)
    output$trackPlays <- renderPlotly({
        
        trackPlays <- values$plays %>% 
            # Want to re-order albums by first time any song was played
            arrange(date) %>% 
            mutate(albumGroup = forcats::fct_inorder(albumGroup)) %>% 
            mutate(track = forcats::as_factor(track))
  
        # Plotly. Very basic. 
        plot_ly(trackPlays, x = ~date, y = ~track, color = ~albumGroup, 
                type = "scatter", colors = "Spectral",
                text = ~paste0("(", album, ") ", track), 
                hoverinfo = "text") %>% 
            layout(title = paste("Track plays by", values$chooseArtist),
                   yaxis = list(visible = FALSE),
                   # autosize = FALSE,
                   # height = 1000,
                   height = (450 + length(unique(trackPlays$album)) * 10),
                   # For artists with lots of albums, the legend goes crazy, so just ignore it. 
                   legend = list(orientation = "h"), 
                   xaxis = list(title = NA), 
                   margin = list(b = 20))
        
    })
    
    ## I think this is too fiddly, just going to have a bar chart of album plays
    output$albumPlays <- renderPlotly({
        
        albumPlays <- values$plays %>%
            count(albumGroup) %>%
            # distinct(albumGroup, n) %>% 
            mutate(albumGroup = forcats::fct_reorder(albumGroup, nn))
            
        plot_ly(data = albumPlays, x = ~nn, y = ~albumGroup, type = "bar",
                text = ~nn, textposition = "outside",
                orientation = "h", color = ~albumGroup, colors = "Spectral",
                hoverinfo = "none") %>% 
            layout(showlegend = FALSE, 
                   bargap = 0.5, barmode = "stack",
                   xaxis = list(title = "", zeroline = FALSE),
                   yaxis = list(title = ""),
                   # annotations = list(
                   #     x = ~n, y = ~album, text = ~n, showarrow = FALSE,
                   #     bgcolor = "white", xanchor = "left"),
                   margin = list(l = (max(nchar(as.character(albumPlays$albumGroup)))*6.7)),
                   height = "100%"
                   )
        
        })
    # Tab 2: More complicated - album plays by artist
    # output$artistPlays <- renderPlotly({
    #     
    #     chooseArtist <- stringr::str_replace_all(input$chooseArtist, "_shinymaterialdropdownspace_", " ")
    #     cat(file=stderr(), chooseArtist, "\n")
    #     
    #     # Want to remove as many duplicates and weirdness as possible.
    #     # So create a function to convert every track name and album name to a lowercase slug, remove punctuation.
    #     getSlugs <- function(x) {
    #         x1 <- stringr::str_to_lower(x)
    #         x2 <- stringr::str_replace_all(x1, "[:punct:]", "")
    #         x3 <- stringr::str_replace_all(x2, "\\s{2,}", " ")
    #         x4 <- stringr::str_trim(x3, side = "both")
    #         return(x4)
    #     }
    #     
    #     # Get list of albums to lookup from lastfm
    #     cat(file=stderr(), "Getting album list", "\n")
    #     
    #     albumList <- lastfm %>%
    #         filter(artist == chooseArtist,
    #                album != "",
    #                # Don't want to include Pitchfok compilations
    #                # Probably some other compilations too?
    #                !(grepl("Pitchfork", album))) %>% 
    #         group_by(album) %>% 
    #         count() %>% 
    #         ungroup() %>% 
    #         # How many plays to keep? ~12 is one whole play of most albums
    #         filter(n > 12) %>% 
    #         magrittr::extract2('album')
    #     
    #     # Count scrobbles now, easier than doing it later.
    #     cat(file=stderr(), "Defining countScrobbles", "\n")
    #     
    #     countScrobbles <- lastfm %>% 
    #         mutate(album = getSlugs(album), track = getSlugs(track)) %>% 
    #         filter(artist == chooseArtist, 
    #                # Keep blanks, try to match against lastfm database. 
    #                album %in% c(getSlugs(albumList), "")) %>% 
    #         group_by(album, track) %>% 
    #         mutate(scrobbles = n()) %>% 
    #         ungroup() %>% 
    #         distinct(album, track, scrobbles)
    #     
    #     # Function to submit album title and artist to lastfm try to download tracklist. 
    #     cat(file=stderr(), "Defining getAlbumInfo", "\n")
    #     
    #     getAlbumInfo <- function(x) {
    #         baseurl <- "http://ws.audioscrobbler.com/2.0/?method=album.getinfo&api_key="
    #         
    #         api <- Sys.getenv('LASTFM_APIKEY')
    #         
    #         # Some special characters cause problems; replace. Might not need %20
    #         y <- gsub(" ", "%20", x)
    #         y <- gsub("#", "%23", y)
    #         z <- gsub(" ", "%20", chooseArtist)
    #         
    #         # Paste GET request
    #         url <- paste0(baseurl, api, 
    #                       "&artist=", z,
    #                       "&album=", y, "&format=json")
    #         
    #         # Send GET request for album
    #         responseRaw <- httr::GET(url)
    #         httr::stop_for_status
    #         
    #         # Convert from json to dataframe
    #         responseClean <- responseRaw %>% 
    #             httr::content(., as = "text") %>% 
    #             jsonlite::fromJSON(., simplifyDataFrame = T, flatten = T) 
    #         
    #         # If it returned tracks, separate into dataframe
    #         if(!is.null(responseClean$album$tracks)) {
    #             if(length(responseClean$album$tracks$track) > 0) {
    #                 responseTracks <- responseClean$album$tracks$track %>% 
    #                     select(track = name, trackNum = `@attr.rank`) %>% 
    #                     mutate(album = responseClean$album$name,
    #                            trackNum = as.integer(trackNum)) 
    #             } else {
    #                 responseTracks <- data_frame(
    #                     track = "", trackNum = NA, album = x
    #                 )
    #             }
    #             # Otherwise just return null 
    #         } else {
    #             responseTracks <- data_frame(
    #                 track = "", trackNum = NA, album = x
    #             )
    #         }
    #         
    #         return(as_data_frame(responseTracks))
    #     }
    #     
    #     cat(file=stderr(), "Calling albumInfo on albumList", "\n")
    #     
    #     albumInfo <- purrr::map_df(albumList, getAlbumInfo)
    #     cat(file=stderr(), "Result from lastfm: ", length(albumInfo$track), " tracks", "\n")
    #     albumInfo$track <- getSlugs(albumInfo$track)
    #     
    #     # Want to merge this list of tracks with scrobbles
    #     # First, try to fill in blank albums with other scrobbles.
    #     cat(file=stderr(), "fillBlanks1", "\n")
    #     
    #     fillBlanks1 <- countScrobbles %>% 
    #         # Join to itself by track, will try to match album
    #         # Arrange so blank tracks are at the bottom. 
    #         left_join(., arrange(countScrobbles, desc(album)), 
    #                   by = "track") %>% 
    #         # Remove duplicates from original data
    #         distinct(album.x, scrobbles.x, track, .keep_all = TRUE) %>% 
    #         # If original was blank, fill with new (best guess)
    #         mutate(album = ifelse(album.x != "", album.x, album.y)) %>% 
    #         # NB scrobbles.x is the original count
    #         select(track, album, scrobbles = scrobbles.x) %>% 
    #         # Then add together scrobbles which have been matched. 
    #         group_by(track, album) %>% 
    #         summarise(scrobbles = sum(scrobbles)) %>% 
    #         ungroup()
    #     
    #     # Second, try matching any remaining blanks with info from lastfm
    #     cat(file=stderr(), "fillBlanks2", "\n")
    #     fillBlanks2 <- left_join(
    #         x = fillBlanks1, 
    #         y = select(albumInfo, -trackNum), 
    #         by = 'track'
    #     ) %>% 
    #         mutate(album = ifelse(album.x != "", album.x, album.y)) %>% 
    #         select(-album.x, -album.y)
    #     
    #     # Now try to add track numbers in lastfm data
    #     cat(file=stderr(), "getTrackNums", "\n")
    #     
    #     getTrackNums <- left_join(
    #         x = fillBlanks2 %>% rename(matchTrack = track, matchAlbum = album),
    #         y = albumInfo %>% mutate(matchTrack = getSlugs(track), matchAlbum = getSlugs(album)),
    #         by = c('matchTrack', 'matchAlbum')
    #     ) %>% 
    #         filter(!is.na(matchAlbum)) %>% select(-track)
    #     
    #     # Some track numbers will fail, need to make a best guess, but avoid duplication. 
    #     # Function to apply to each album to guess the track number.
    #     cat(file=stderr(), "getBlankTracks", "\n")
    #     
    #     getBlankTracks <- function(dataframe) {
    #         if(anyNA(dataframe$trackNum)) {
    #             tempBlank <- filter(dataframe, is.na(trackNum))
    #             possNums = c()
    #             for(i in 1:tempBlank$trackCount) {
    #                 if(i %in% dataframe$trackNum) {
    #                     next
    #                 } else {
    #                     possNums <- c(possNums, i)
    #                 }
    #             }
    #             possNums <- possNums[c(1:nrow(tempBlank))]
    #             for(i in 1:length(possNums)) {
    #                 tempBlank$trackNum[i] <- possNums[i]
    #             }
    #             bind_rows(
    #                 filter(dataframe, !is.na(trackNum)), 
    #                 tempBlank)
    #             
    #         } else {
    #             dataframe
    #         }
    #     }
    #     cat(file=stderr(), "getTrackNums2", "\n")
    #     
    #     getTrackNums2 <- getTrackNums %>%
    #         group_by(matchAlbum) %>% 
    #         mutate(trackCount = n()) %>% 
    #         # Nest into list, then apply function to each album individually
    #         nest() %>% 
    #         mutate(data = map(data, getBlankTracks)) %>% 
    #         unnest() %>% 
    #         # Also try to fill in missing album data
    #         arrange(matchAlbum, album) %>% 
    #         fill(album)
    #     
    #     
    #     # Should be ready to plot a graph
    #     cat(file=stderr(), "Start plotting the graphs", "\n")
    #     
    #     getAlbums <- select(getTrackNums2, matchAlbum, matchTrack, scrobbles, trackNum) %>% 
    #         arrange(matchAlbum, trackNum)
    #     
    #     howManyAlbums <- length(unique(getAlbums$matchAlbum))
    #     howManyRows <- ifelse(howManyAlbums %% 3 == 0, 
    #                           howManyAlbums %/% 3, (howManyAlbums %/% 3) + 1)
    #     
    #     albums <- getAlbums %>% 
    #         group_by(matchAlbum) %>% 
    #         summarise(total = sum(scrobbles)) %>%
    #         arrange(desc(total)) %>% 
    #         magrittr::extract2('matchAlbum')
    #     
    #     # Only keep max 12 albums
    #     if(howManyAlbums > 12) {
    #         keep <- getAlbums %>% 
    #             group_by(matchAlbum) %>% 
    #             
    #             summarise(total = sum(scrobbles)) %>% 
    #             top_n(n = 12, wt = total) %>% 
    #             magrittr::extract2('matchAlbum')
    #         getAlbums <- filter(getAlbums, matchAlbum %in% keep)
    #         
    #         howManyAlbums <- 12
    #     }
    #     
    #     plotlyCol <- brewer.pal(12, 'Paired')
    #     
    #     plots <- list()
    #     for(i in 1:howManyAlbums) {
    #         
    #         album <- stringr::str_to_title(albums[i])
    #         albumData <- 
    #             filter(getAlbums, matchAlbum == albums[i])
    #         plots[[i]] <- plot_ly(
    #             albumData, x = ~trackNum, y = ~scrobbles,
    #             text = ~paste0(matchTrack, ": ", scrobbles), hoverinfo = "text",
    #             type = "scatter", mode = "lines+markers", 
    #             marker = list(color = plotlyCol[i]),
    #             line = list(color = plotlyCol[i])
    #         ) %>%
    #             layout(title = paste("Album plays by", chooseArtist),
    #                    xaxis = list(title = "", showgrid=FALSE, zeroline=F,
    #                                 tickformat = ",d"),
    #                    yaxis = list(title = "", showgrid=FALSE, zeroline=F,
    #                                 tickformat = ",d",
    #                                 range = c(0, ~max(getAlbums$scrobbles)))
    #                    
    #             )
    #     }
    #     
    #     finalP <- subplot(plots, nrows = howManyRows, shareY = FALSE, shareX = FALSE,
    #                       margin = c(0.02, 0.02, 0.08, 0.02)) %>% 
    #         layout(showlegend = FALSE)
    #     
    #     # Try to add annotations separately?
    #     for(i in 1:howManyAlbums) {
    #         finalP <- finalP %>% 
    #             layout(annotations = list(
    #                 x = 0,
    #                 xanchor = "left", xref = paste0("x", i),
    #                 y = 0, 
    #                 yanchor ="auto", yref = paste0("y", i),
    #                 text = albums[i], showarrow = F,
    #                 bgcolor = "#FFFFFF00", bordercolor = "#D9D9D9"
    #             ))
    #     }
    #     
    #     finalP %>% layout(height = 150 * howManyRows)
    #     
    # })
}


# Run the application 
shinyApp(ui = ui, server = server)

