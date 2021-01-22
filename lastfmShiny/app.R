# setwd('lastfmShiny')
# getwd()
# Setup -------------------------------------------------------------------

library(shiny);library(shinymaterial);library(plotly);library(tidyverse);library(RColorBrewer)
library(lubridate);library(rdrop2);library(zoo);library(stringr);library(forcats)

## NB Need to create a new Renviron file in this folder containing Lastfm API credentials
## Won't be committed to github so need to recreate 
readRenviron(".Renviron")
print(Sys.getenv('LASTFM_USER'))

## Import local dropbox token
# token <- rdrop2::drop_auth()
# saveRDS(token, 'dropbox.rds')
rdrop2::drop_auth(rdstoken = 'dropbox.rds')


## Download plex ratings
rdrop2:::drop_download(path = 'R/lastfm/plexMasterRatings.csv', 
                       local_path = 'plexMasterRatings.csv', 
                       overwrite = T)
ratings <- read_csv('plexMasterRatings.csv')

rdrop2::drop_download(
    path = 'R/lastfm/plexDB.csv', 
    local_path = 'plexDB.csv', 
    overwrite = T
)

plexDB <- read_csv('plexDB.csv')
source('getLastfmShiny.R')

refresh = TRUE

# Run the function
if(refresh) {
    lastfm <- getLastfmShiny()
} else {
    lastfm <- read_csv("tracks.csv")
}

## Get list of artists with more than 25 plays, and ignore 'The' and 'A' at 
# beginning of name in sorting
artistList <- lastfm %>% 
    distinct(artist, track) %>% 
    count(artist) %>% 
    filter(n > 25, str_detect(artist, 'unknown', negate = TRUE)) %>% 
    mutate(arrangeArtist = str_remove_all(artist, '^The |^A ')) %>% 
    arrange(arrangeArtist) %>% 
    pull(artist)



# UI ----------------------------------------------------------------------

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
    
    # Summary page
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
                        choices = c("All time", "Last 30 days",
                                    seq.int(from = year(max(lastfm$date)), 
                                            to = year(min(lastfm$date))))
                    )
                )
            ),
            material_column(
                width = 9, 
                material_card(title = "", plotlyOutput("plotlyTime", height = "100%")),
                material_card(title = "", plotlyOutput("plotlyBar", height = "100%"))
                )
            )
        ),
    
    # Artist Page
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
                ),
                material_row(
                    material_card(title = "", plotlyOutput('albumRatings', height = "100%"))
                )
            )
        )
        
    )
    
)


# Server ------------------------------------------------------------------
server <- function(input, output, session) {

    values <- reactiveValues()
    values$lastfm <- lastfm
    
    # Want to remove as many duplicates and weirdness as possible.
    # Function to convert every track name and album name to a lowercase slug,
    # remove punctuation etc.
    getSlugs <- function(x) {
        x1 <- stringr::str_to_lower(x)
        x2 <- stringr::str_replace_all(x1, "&", "and")
        x3 <- stringr::str_remove_all(x2, "[:punct:]")
        x4 <- stringr::str_replace_all(x3, "\\s{2,}", " ")
        x5 <- stringr::str_trim(x4, side = "both")
        ## Remove anything in brackets, e.g. (remastered) etc
        x6 <- stringr::str_remove_all(x5, "\\s\\(.*")
        return(x6)
    }
    
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
    
    observeEvent({
        input$chooseYear
        input$chooseType
    }, {
        chooseYear <- input$chooseYear
        chooseType <- input$chooseType
        
        if (chooseYear == "All time") {
            top10data <- values$lastfm
            values$minDate <- as_date(min(top10data$date))
            values$maxDate <- as_date(max(top10data$date))
        } else if (chooseYear == "Last 30 days") {
            values$minDate <- as_date(today() - days(30))
            values$maxDate <- as_date(today())
            top10data <- filter(values$lastfm,
                                date >= values$minDate)
            
        } else {
            top10data <- filter(values$lastfm, year(date) == chooseYear)
            values$minDate <- dmy(paste0("01-01-", chooseYear))
            values$maxDate <- dmy(paste0("31-12-", chooseYear))
        }
        
        ## For tracks and albums, combine with artists first (as newCol). 
        if (chooseType == "Artists") {
            top10data <- mutate(top10data, newCol = str_to_title(artist))
        } else {
            top10data <- unite(top10data, col = "newCol", sep = " - ",
                               c("artist", str_to_lower(str_sub(chooseType, 0, -2)))) %>% 
                mutate(newCol = str_to_title(newCol))
        }
        
        ## Now get top 10 of newCol and filter data
        top10 <- top10data %>% count(newCol, sort = T) %>% 
            head(10) %>% pull('newCol')
        values$top10data <- top10data %>% 
            filter(newCol %in% top10 ) %>% 
            mutate(date = as_date(date)) %>% 
            select(newCol, date)
    })
    

# Plotly: Top 10 time series ----------------------------------------------

    output$plotlyTime <- renderPlotly({
        
        ## Use the values$top10data, with newCol as the column of interest
        top10plays <- values$top10data %>% 
            mutate(newCol = ifelse(str_detect(newCol, " - "),
                                   str_c(str_replace_all(newCol, " - ", "<br><em>"), "</em>"),
                                   newCol)) %>% 
            ## Create a count per day
            count(newCol, date) %>% 
            ## Then combine with a sequence of days for the whole year to get rolling
            spread(key = newCol, value = n) %>% 
            full_join(., data.frame(
                date = seq.Date(from = values$minDate, to = values$maxDate, by = 1)
            ), by = "date") %>% 
            gather(-date, key = newCol, value = n) %>% 
            ## And replace all the NAs with 0
            mutate(n = replace_na(n, 0)) %>% 
            arrange(newCol, date) %>% 
            ## And add rolling 30 day
            group_by(newCol) %>% 
            mutate(plays = c(cumsum(n[1:29]), 
                             zoo::rollsum(n, k = 30, align = "right")))
        
        top10plays %>% 
            group_by(newCol) %>% 
            plot_ly(x = ~date, y = ~plays, color = ~newCol, type = "scatter", mode ="lines",
                    fill = "tozeroy", colors = "Spectral", 
                    text = ~str_c(newCol, "<br>Plays: ", plays), hoverinfo = "text") %>% 
            plotly::layout(xaxis = list(title = ""),
                   yaxis = list(title = "Monthly plays"),
                   title = str_c("Top ten ", tolower(input$chooseType), " in ", input$chooseYear),
                   legend = list(orientation = "h", xanchor = "center", x = 0.5,
                                                    yanchor = "top", y = 0))
            
    })
    

# Plotly: Top 10 bar ------------------------------------------------------

    output$plotlyBar <- renderPlotly({
        
        ## Just a horizontal bar chart showing totals
        values$top10data %>% 
            mutate(newCol = ifelse(str_detect(newCol, " - "),
                                   str_c(str_replace_all(newCol, " - ", "<br><em>"), "</em>"),
                                   newCol)) %>% 
            count(newCol, sort = T) %>% 
            mutate(newCol = forcats::fct_inorder(newCol)) %>% 
            group_by(newCol) %>% 
            plot_ly(x = ~n, y = ~newCol, color = ~newCol, type = "bar",
                    text = ~n, textposition = "outside",
                    orientation = "h", colors = "Spectral",
                    hovertext = ~str_c(newCol, n, sep = ": <br>"), hoverinfo = "text"
                    ) %>% 
            plotly::layout(showlegend = FALSE, bargap = 0.5,
                   xaxis = list(title = "", zeroline = FALSE),
                   yaxis = list(title = ""),
                   margin = list(l = 120, r = 40))
        
    })
    
    
    

# Get Artist Dataframe ----------------------------------------------------

    ## Filter scrobbles by artist for both graphs
    
    observeEvent(input$chooseArtist, {
        
        values$chooseArtist <- stringr::str_replace_all(
            input$chooseArtist, "_shinymaterialdropdownspace_", " "
        )
        
        artistTracks <- values$lastfm %>% 
          filter(artist == values$chooseArtist) %>% 
          mutate(track = getSlugs(track),
                 album = getSlugs(album))
        
        ## Want to limit each track to one album, just use the most popular 
        ## (i.e. the one most tracks appear on, excluding NA)
        albumList <- artistTracks %>% 
          distinct(track, artist, album) %>% 
          na.omit() %>% 
          group_by(album) %>% 
          mutate(n = n()) %>% 
          group_by(track) %>% 
          arrange(desc(n)) %>% 
          slice(1) %>% 
          distinct(track, album) %>% ungroup()
        
        values$plays <- full_join(artistTracks %>% select(-album),
                                albumList, by = 'track') %>% 
          mutate(album = ifelse(is.na(album) | album == '',
                                'others', album)) %>% 
          add_count(album) %>%
          mutate(album = ifelse(n < 9, 'others', album)) %>% 
          arrange(date) %>% 
          mutate(album = forcats::fct_inorder(album)) %>% 
          arrange(album, date) %>% 
          mutate(track = forcats::fct_inorder(track))
        
    })
        

# Plotly: Artist time series ----------------------------------------------

    ## first tab: plotly of tracks over time (rough)
    output$trackPlays <- renderPlotly({
        
        trackPlays <- values$plays 
        
        # Plotly. Very basic. 
        plot_ly(trackPlays, x = ~date, y = ~track, color = ~album, 
                type = "scatter", colors = "Spectral",
                text = ~paste0("(", album, ") ", track), 
                hoverinfo = "text") %>% 
            layout(title = paste("Track plays by", values$chooseArtist),
                   yaxis = list(visible = FALSE),
                   ## Fudge to get chart size to adapt to number of albums
                   height = (450 + length(unique(trackPlays$album)) * 10),
                   # For artists with lots of albums, the legend goes crazy, so just ignore it. 
                   legend = list(orientation = "h"), 
                   xaxis = list(title = NA), 
                   margin = list(b = 20))
        
    })
    

# Plotly: Artist bar ------------------------------------------------------

    ## Just going to have a bar chart of album plays
    output$albumPlays <- renderPlotly({
        
        albumPlays <- values$plays %>%
            count(album) %>%
            mutate(albumGroup = forcats::fct_reorder(album, n))
            
        plot_ly(data = albumPlays, x = ~n, y = ~albumGroup, type = "bar",
                text = ~n, textposition = "outside",
                orientation = "h", color = ~albumGroup, colors = "Spectral",
                hoverinfo = "none") %>% 
            layout(showlegend = FALSE, 
                   bargap = 0.5, barmode = "stack",
                   xaxis = list(title = "", zeroline = FALSE),
                   yaxis = list(title = ""),
                   ## Another fudge to get margin to adapt to album title length
                   margin = list(l = (max(nchar(as.character(albumPlays$albumGroup)))*6.7)),
                   height = "100%"
                   )
        
        })
    

# Plotly: Album Ratings --------------------------------------------------

output$albumRatings <- renderPlotly({
    
    if (values$chooseArtist %in% plexDB$artist) {
        ratingsPlot <- plexDB %>% 
            filter(artist == values$chooseArtist) %>% 
            mutate(rating = replace_na(rating, 0),
                   rating = as.factor(rating/2),
                   album = forcats::fct_rev(album))
        
        plot_ly(ratingsPlot, y = ~album, x = ~trackNum, color = ~rating, 
                type = 'scatter', mode = 'markers', 
                marker = list(size = 20, 
                              line = list(color = 'lightgrey', width = 1)),
                colors = c('#FFFFFF', '#DDE2F9','#B5BEE7',
                           '#8C99D6','#6475C4','#3B50B2'), 
                text = ~str_c(track, rating, sep = ": "), 
                hoverinfo = 'text') %>% 
            layout(showlegend = FALSE, 
                   yaxis = list(title = NA), 
                   xaxis = list(title = NA, zeroline = FALSE))
    
    }
})
    
   
    
    
}


# Run the application 
shinyApp(ui = ui, server = server)

