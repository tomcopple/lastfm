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
                        choices = c("All time", seq.int(from = year(max(lastfm$date)), 
                                          to = year(min(lastfm$date))))
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
        # Filter for year, unless it says all time
        chooseYear <- input$chooseYear
        chooseType <- input$chooseType
        
        if(chooseYear == "All time") {
            top10data <- values$lastfm
            minDate <- as_date(min(top10data$date))
            maxDate <- as_date(max(top10data$date))
        } else {
            top10data <- filter(values$lastfm, year(date) == chooseYear)
            minDate <- dmy(paste0("01-01-", chooseYear))
            maxDate <- dmy(paste0("31-12-", chooseYear))
        }
        
        # If it's tracks or albums, need to make sure it's the right one
        # Just do three functions for each one, too complicated otherwise
        if(chooseType == "Artists") {
            top10 <- top10data %>% 
                mutate(artist = stringr::str_to_title(artist)) %>% 
                count(artist, sort = T) %>% 
                top_n(10) %>% 
                ## If there are more than 10 results (e.g. tie), just force it to take 10 rows
                head(10) %>% 
                magrittr::extract2('artist')
            
            ## Must be an easier way of doing this, but going to split into lists
            ## then rejoin together afterwards?
            top10plays <- top10data %>% 
                
                mutate(artist = stringr::str_to_title(artist)) %>% 
                
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
                    date = seq.Date(from = minDate, to = maxDate, by = 1)))) %>% 
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
                mutate(artist = stringr::str_to_title(artist),
                       album  = stringr::str_to_title(album)) %>% 
                count(artist, album, sort = T) %>% 
                top_n(10) %>% 
                head(10) %>% 
                unite(col = "album", artist, album) %>% 
                extract2('album')
            
            ## Must be an easier way of doing this, but going to split into lists
            ## then rejoin together afterwards?
            top10plays <- top10data %>% 
                mutate(artist = stringr::str_to_title(artist),
                       album  = stringr::str_to_title(album)) %>% 
                unite(col = "album", artist, album) %>% 
                filter(album %in% top10) %>% 
                select(album, date) %>% 
                mutate(date = lubridate::as_date(date)) %>% 
                count(album, date) %>% 
                nest(-album) %>% 
                mutate(dateSeq = list(data_frame(
                    date = seq.Date(from = minDate, to = maxDate, by = 1)))) %>% 
                mutate(fullJoin = map2(data, dateSeq, full_join, by = "date")) %>% 
                mutate(fullJoin = map(fullJoin, arrange, date)) %>% 
                mutate(fullJoin = map(fullJoin, mutate, n = ifelse(is.na(n), 0, n))) %>% 
                mutate(fullJoin = map(fullJoin, mutate, 
                                      plays = c(cumsum(n[1:29]),
                                                zoo::rollsum(n, k = 30, align = "right")))) %>% 
                unnest(fullJoin) %>% 
                separate(album, into = c('artist', 'album'), sep = "_") %>% 
                mutate(x = stringr::str_c(album, " [", artist, "]"))
        } else {
            top10 <- top10data %>% 
                mutate(artist = stringr::str_to_title(artist),
                       track  = stringr::str_to_title(track)) %>% 
                count(artist, track, sort = T) %>% 
                top_n(10) %>% 
                head(10) %>% 
                unite(col = "track", artist, track) %>% 
                extract2('track')
            
            ## Must be an easier way of doing this, but going to split into lists
            ## then rejoin together afterwards?
            top10plays <- top10data %>% 
                mutate(artist = stringr::str_to_title(artist),
                       track  = stringr::str_to_title(track)) %>% 
                unite(col = "track", artist, track) %>% 
                filter(track %in% top10) %>% 
                select(track, date) %>% 
                mutate(date = lubridate::as_date(date)) %>% 
                count(track, date) %>% 
                nest(-track) %>% 
                mutate(dateSeq = list(data_frame(
                    date = seq.Date(from = minDate, to = maxDate, by = 1)))) %>% 
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
                   legend = list(orientation = "h", xanchor = "center", x = 0.5))
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
            mutate(albumGroup = forcats::fct_reorder(albumGroup, n))
            
        plot_ly(data = albumPlays, x = ~n, y = ~albumGroup, type = "bar",
                text = ~n, textposition = "outside",
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
}


# Run the application 
shinyApp(ui = ui, server = server)

