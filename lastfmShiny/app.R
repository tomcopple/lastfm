
# getwd()
# if (str_detect(getwd(), "Shiny", negate = T)) {
    # setwd('lastfmShiny')
# }


# Setup -------------------------------------------------------------------

suppressMessages({
    library(shiny)
    library(shinymaterial)
    library(plotly)
    library(tidyverse)
    library(RColorBrewer)
    library(lubridate)
    library(zoo)
    library(stringr)
    library(forcats)
    library(dropboxr)
})


## NB Need to create a new Renviron file in this folder containing Lastfm API credentials
## usethis::edit_r_environ('project') then copy to lastfmShiny folder
## Won't be committed to github so need to recreate
readRenviron(".Renviron")
lastfm_user <- Sys.getenv('LASTFM_USER')
lastfm_key <- Sys.getenv('LASTFM_APIKEY')
dropbox_key <- Sys.getenv('DROPBOX_KEY')
dropbox_secret <- Sys.getenv('DROPBOX_SECRET')

print(str_c('Username: ', Sys.getenv('LASTFM_USER')))

## Try dropbox authentication (run this locally first) ----
# dropboxr::dropbox_auth(dropbox_key, dropbox_secret, cache_path = "dropbox.rds")
dtoken <- readRDS('dropbox.rds')

print(str_glue("Access token: {dtoken$credentials$access_token}"))

## Download plex ratings ----

access_token <- dtoken$credentials$access_token
print("Downloading plex")
plexDB <- dropboxr::download_dropbox_file('/R/lastfm/plexMasterRatings.csv', token = access_token)

## Get lastfm scrobbles for Shiny App ----
getLastfmShiny <- function(refresh = TRUE) {
    
    print("Getting Lastfm Shiny")
    lastfmDownload <- dropboxr::download_dropbox_file("/R/lastfm/tracks.csv", token = dtoken)
    maxDate = max(lastfmDownload$date)
    
    if (refresh) {
        ## Start a response DF for new data
        responseDF <- tibble()
        pageNum <- 1
        
        # Keep getting data from lastfm until it's caught up with local data. 
        while(min(responseDF$date) >= maxDate) {
            
            # 1. Build the request
            req <- request("http://ws.audioscrobbler.com/2.0/") %>%
                req_url_query(
                    method = "user.getrecenttracks",
                    user = lastfm_user,
                    api_key = lastfm_key,
                    format = "json",
                    limit = 200,
                    page = pageNum
                )
            
            # 2. Perform request and process data
            responseRaw <- req %>%
                req_retry(max_tries = 3) %>% 
                req_perform() %>%
                resp_body_json(simplifyVector = TRUE, flatten = TRUE) %>%
                purrr::pluck("recenttracks", "track") %>% 
                select(
                    track = name, 
                    artist = `artist.#text`,
                    album = `album.#text`, 
                    date = `date.#text`
                ) %>%
                mutate(date = dmy_hm(date)) %>%
                na.omit()
            
            responseDF <- bind_rows(responseDF, responseRaw) %>% 
                arrange(desc(date))
            pageNum <- pageNum + 1
        }
        
        # Then filter response for new tracks, and add to localData
        lastfmUpload <- dplyr::bind_rows(
            dplyr::filter(responseDF, date > maxDate),
            lastfmDownload
        )
        
        ## Then save updated data as a local csv and upload back to Dropbox. 
        dropboxr::upload_df_to_dropbox(lastfmUpload, "/R/lastfm/tracks.csv", token = dtoken)
        
        return(lastfmUpload)
    } else {
        return(lastfmDownload)
    }
    
}

getSlugs <- function(text) {
    text1 <- text %>% 
        str_to_lower() %>% 
        str_remove_all(pattern = "[[:punct:]]") %>% 
        str_replace_all(pattern = "&", "and") %>% 
        str_replace_all(pattern = "\\s{2,}", replacement = " ") %>% 
        str_trim(side = 'both') %>% 
        str_remove_all(pattern = "\\s\\(.*") %>% 
        str_replace_all(pattern = " ", replacement = "-")
    return(text1)
}

removeEve <- function(lastfmDF) {
    lastfmDF <- lastfmDF %>% 
        filter(
            str_detect(artist, "Cocomelon", negate = T),
            str_detect(artist, "Super Simple", negate = T),
            str_detect(artist, "Duggee", negate = T),
            str_detect(artist, "Lullaby", negate = T),
            str_detect(artist, "Night Garden", negate = T),
            str_detect(artist, 'Toddler Tunes', negate = T),
            str_detect(artist, 'Pinkfong', negate = T)
        )
    return(lastfmDF)
}

# refresh = TRUE
refresh = FALSE

# Run the function
lastfm <- getLastfmShiny(refresh = refresh) %>% 
    removeEve()

## Get list of artists with more than 25 plays, and ignore 'The' and 'A' at
# beginning of name in sorting
artistList <- lastfm %>%
  distinct(artist, track) %>%
  mutate(artist = str_to_title(artist)) %>%
  count(artist) %>%
  filter(n > 25, str_detect(artist, '[Uu]nknown', negate = TRUE)) %>%
  mutate(arrangeArtist = str_remove_all(artist, '^The |^A ')) %>%
  arrange(arrangeArtist) %>%
  pull(artist)

# UI ----------------------------------------------------------------------

ui <- material_page(
  # Application title
  title = "Lastfm Dashboard",
  
  nav_bar_color = "indigo",
  
  # Want to try having a main tab here first?
  material_tabs(tabs = c(
    "Main page" = "main_page",
    "Artist page" = "artist_page"
  )),
  
  # * Main page ----
  material_tab_content(
    tab_id = "main_page",
    tags$h2("Summary of recent plays"),
    
    material_row(
      
      # * * Input column ---- 
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
                        seq.int(
                          from = year(max(lastfm$date)),
                          to = year(min(lastfm$date))
                        ))
          )
        )
      ),
      # * * Plotly graphs ---- 
      material_column(
        width = 9,
        material_card(
          title = "", plotlyOutput("plotlyTime", height = "100%")
          ),
        material_card(
          title = "", plotlyOutput("plotlyBar", height = "100%")
          ),
        h3("Plex Summary"),
        material_card(
          title = "", plotlyOutput('yearRatings', height = "100%")
        )
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
        material_row(material_card(
          title = "", plotlyOutput("trackPlays", height = "100%")
        )),
        material_row(material_card(
          title = "", plotlyOutput("albumPlays", height = "100%")
        )),
        material_row(
          material_column(material_card(
            title = "", plotOutput('albumRatings')
          )),
          material_column(material_card(
            title = "", plotlyOutput('albumRatings2', height = '100%')
          ))
          
        )
      )
    ))
  
)


# Server ------------------------------------------------------------------
server <- function(input, output, session) {
  values <- reactiveValues()
  values$lastfm <- lastfm
  
  observeEvent(input$refresh, {
    if (input$refresh > 0) {
      material_spinner_show(session, "trackPlays")
      material_spinner_show(session, "albumPlays")
      shiny::showNotification("Refreshing data, may take some time...", type = "default")
      values$lastfm <- getLastfmShiny(T)
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
    } else if (chooseType == 'Albums') {
      top10data <- top10data %>%
        na.omit() %>%
        unite(col = 'newCol',
              sep = ' - ',
              c('artist', 'album')) %>%
        mutate(newCol = str_to_title(newCol))
    } else {
      top10data <- unite(top10data,
                         col = "newCol",
                         sep = " - ",
                         c("artist", 'track')) %>%
        mutate(newCol = str_to_title(newCol))
    }
    
    ## Now get top 10 of newCol and filter data
    top10 <- top10data %>% count(newCol, sort = T) %>%
      head(10) %>% pull('newCol')
    values$top10data <- top10data %>%
      filter(newCol %in% top10) %>%
      mutate(date = as_date(date)) %>%
      select(newCol, date)
  })
  
  
  # Plotly: Top 10 time series ----------------------------------------------
  
  output$plotlyTime <- renderPlotly({
    ## Use the values$top10data, with newCol as the column of interest
    top10plays <- values$top10data %>%
      mutate(newCol = ifelse(
        str_detect(newCol, " - "),
        str_c(str_replace_all(newCol, " - ", "<br><em>"), "</em>"),
        newCol
      )) %>%
      ## Create a count per day
      count(newCol, date) %>%
      ## Then combine with a sequence of days for the whole year to get rolling
      spread(key = newCol, value = n) %>%
      full_join(., data.frame(date = seq.Date(
        from = values$minDate,
        to = values$maxDate,
        by = 1
      )), by = "date") %>%
      gather(-date, key = newCol, value = n) %>%
      ## And replace all the NAs with 0
      mutate(n = replace_na(n, 0)) %>%
      arrange(newCol, date) %>%
      ## And add rolling 30 day
      group_by(newCol) %>%
      mutate(plays = c(cumsum(n[1:29]),
                       zoo::rollsum(
                         n, k = 30, align = "right"
                       )))
    
    print("Top 10 plays chart")
    top10plays %>%
      group_by(newCol) %>%
      plot_ly(
        x = ~ date,
        y = ~ plays,
        color = ~ newCol,
        type = "scatter",
        mode = "lines",
        fill = "tozeroy",
        colors = "Spectral",
        text = ~ str_c(newCol, "<br>Plays: ", plays),
        hoverinfo = "text"
      ) %>%
      plotly::layout(
        xaxis = list(title = ""),
        yaxis = list(title = "Monthly plays"),
        title = str_c(
          "Top ten ",
          tolower(input$chooseType),
          " in ",
          input$chooseYear
        ),
        legend = list(
          orientation = "h",
          xanchor = "center",
          x = 0.5,
          yanchor = "top",
          y = 0
        )
      )
    
  })
  
  
  # Plotly: Top 10 bar ------------------------------------------------------
  
  print("Plotly bar chart")
  output$plotlyBar <- renderPlotly({
    ## Just a horizontal bar chart showing totals
    values$top10data %>%
      mutate(newCol = ifelse(
        str_detect(newCol, " - "),
        str_c(str_replace_all(newCol, " - ", "<br><em>"), "</em>"),
        newCol
      )) %>%
      count(newCol, sort = T) %>%
      mutate(newCol = forcats::fct_inorder(newCol)) %>%
      group_by(newCol) %>%
      plot_ly(
        x = ~ n,
        y = ~ newCol,
        color = ~ newCol,
        type = "bar",
        text = ~ n,
        textposition = "outside",
        orientation = "h",
        colors = "Spectral",
        hovertext = ~ str_c(newCol, n, sep = ": <br>"),
        hoverinfo = "text"
      ) %>%
      plotly::layout(
        showlegend = FALSE,
        bargap = 0.5,
        xaxis = list(title = "", zeroline = FALSE),
        yaxis = list(title = ""),
        margin = list(l = 120, r = 40)
      )
    
  })
  
  # Plotly: Album Ratings ------------------------------------------------------
  
  output$yearRatings <- renderPlotly({

    ## Join plexDB and lastfm to get plays/ratings
    message('input$chooseType ', input$chooseType)
    ratings <- inner_join(
      plexDB %>% mutate(
        newCol = case_when(
          input$chooseType == 'Artists' ~ getSlugs(artist),
          input$chooseType == 'Albums' ~ getSlugs(str_c(artist, " - ", album)),
          input$chooseType == 'Tracks' ~ getSlugs(str_c(artist, " - ", track))
        )),
      values$top10data %>% mutate(newCol = getSlugs(newCol)),
      by = 'newCol'
    ) %>%
      distinct(artist, track, album, rating, trackNum, discNum, year, newCol)

    # print(head(ratings))
    ratings %>%
      mutate(newColName = ifelse(nchar(newCol) > 25, str_c(str_sub(newCol, 0, 25), "..."), newCol)) %>%
      mutate(rating0 = replace_na(rating, 0)/2) %>%
      mutate(rating = rating/2) %>%
      mutate(text = str_c(artist, " - ", track, "<br>", album)) %>%
      ggplot(aes(x = trackNum, y = rating0, group = newColName, color = newCol, text = text)) +
      theme(legend.title = element_blank()) +
      # scale_y_continuous(expand = c(0,0), limits = c(0, 5)) +
      xlab(NULL) + ylab(NULL) +
      geom_jitter(alpha = 0.5) +
      geom_smooth(se = FALSE, span = 3, aes(y = rating))
    plotly::ggplotly(tooltip = 'text')
  })
  
  
  
  
  # Get Artist Dataframe ----------------------------------------------------
  
  ## Filter scrobbles by artist for both graphs
  
  observeEvent(input$chooseArtist, {
    values$chooseArtist <- stringr::str_replace_all(input$chooseArtist,
                                                    "_shinymaterialdropdownspace_",
                                                    " ")
    
    artistTracks <- values$lastfm %>%
      filter(str_to_lower(artist) == str_to_lower(values$chooseArtist)) %>%
      mutate(track = getSlugs(track),
             album = getSlugs(album))
    
    ## Want to limit each track to one album, just use the most popular
    ## Actually maybe don't want to do this?
    ## (i.e. the one most tracks appear on, excluding NA)
    albumList <- artistTracks %>%
      distinct(track, artist, album) %>%
      na.omit() %>%
      group_by(album) %>%
      mutate(n = n()) %>%
      group_by(track) %>%
      arrange(desc(n)) %>%
      slice(1) %>%
      distinct(track, album) %>% ungroup() %>%
      rename(album2 = album)
    
    values$plays <- full_join(artistTracks,
                              albumList, by = 'track') %>%
      mutate(album = ifelse(is.na(album) |
                              album == '', album2, album)) %>%
      mutate(album = ifelse(is.na(album) | album == '',
                            'others', album)) %>%
      add_count(album) %>%
      mutate(album = ifelse(n < 9, 'others', album)) %>%
      arrange(date) %>%
      mutate(album = forcats::fct_inorder(album)) %>%
      arrange(album, date) %>%
      mutate(track = forcats::fct_inorder(track))
    
    print('Getting plex plays')
    print(values$chooseArtist)
    ## Also get Plex DB dataframe for album ratings (x2)
    ## But only if the artist is in Plex?
    if (str_to_lower(values$chooseArtist) %in% str_to_lower(plexDB$artist)) {
      values$ratings <- plexDB %>%
        # filter(artist == 'Sault') %>%
        filter(str_to_lower(artist) == str_to_lower(values$chooseArtist)) %>%
        filter(albumArtist != "Various Artists") %>%
        group_by(album) %>%
        add_count() %>%
        filter(n > 1) %>%
        ungroup() %>%
        select(-n) %>%
        mutate(discNum = replace_na(discNum, 1)) %>% 
        mutate(album = ifelse(discNum == 1, 
                              album, 
                              str_c(album, " (", discNum, ")"))) %>%
        mutate(
          rating = replace_na(rating, 0) / 2,
          album = forcats::fct_rev(album)
        )
      # print(unique(values$ratings$album))
    }
    
  })
  
  
  # Plotly: Artist time series ----------------------------------------------
  
  ## first tab: plotly of tracks over time (rough)
  output$trackPlays <- renderPlotly({
    trackPlays <- values$plays
    
    numberOfAlbums <- length(unique(trackPlays$album))
    
    # Plotly. Very basic.
    print("Plotly artist time series")
    plot_ly(
      trackPlays,
      x = ~ date,
      y = ~ track,
      color = ~ album,
      type = "scatter", mode = 'markers',
      colors = colorRampPalette(brewer.pal(11, "Spectral"))(numberOfAlbums),
      text = ~ paste0("(", album, ") ", track),
      ## Fudge to get chart size to adapt to number of albums
      height = (450 + length(unique(
          trackPlays$album
      )) * 10),
      hoverinfo = "text"
    ) %>%
      plotly::layout(
        title = paste("Track plays by", values$chooseArtist),
        yaxis = list(visible = FALSE),
        # For artists with lots of albums, the legend goes crazy, so just ignore it.
        legend = list(orientation = "h"),
        xaxis = list(title = NA),
        margin = list(b = 20)
      )
    
  })
  
  
  # Plotly: Artist bar ------------------------------------------------------
  
  ## Just going to have a bar chart of album plays
  output$albumPlays <- renderPlotly({
    albumPlays <- values$plays %>%
      count(album) %>%
      mutate(albumGroup = forcats::fct_reorder(album, n))
    
    numberOfAlbums <- length(unique(albumPlays$album))
    
    
    print("Plotly album plays bar chart")
    plot_ly(
      data = albumPlays,
      x = ~ n,
      y = ~ albumGroup,
      type = "bar",
      text = ~ n,
      textposition = "outside",
      orientation = "h",
      color = ~ albumGroup,
      colors = colorRampPalette(brewer.pal(11, "Spectral"))(numberOfAlbums),
      hoverinfo = "none"
    ) %>%
      plotly::layout(
        showlegend = FALSE,
        bargap = 0.5,
        barmode = "stack",
        xaxis = list(title = "", zeroline = FALSE),
        yaxis = list(title = ""),
        ## Another fudge to get margin to adapt to album title length
        margin = list(l = (max(
          nchar(as.character(albumPlays$albumGroup))
        ) * 6.7)),
        height = "100%"
      )
    
  })
  
  
  # Plotly: Album Ratings --------------------------------------------------
  
  output$albumRatings <- renderPlot(
    if (str_to_lower(values$chooseArtist) %in% str_to_lower(plexDB$artist)) {
     
      colors <- c('#FFFFFF',
                  '#DDE2F9',
                  '#B5BEE7',
                  '#8C99D6',
                  '#6475C4',
                  '#3B50B2')
      
      print(unique(values$ratings$album))
      values$ratings %>% 
        mutate(rating = ifelse(rating == 0, NA, rating)) %>% 
        group_by(album) %>% 
        mutate(y = n()) %>% 
        na.omit() %>% 
        mutate(x = n()) %>% 
        group_by(album, x, y) %>% summarise(avRat = mean(rating), .groups = 'drop') %>% 
        mutate(albumName = as.character(album)) %>% 
        mutate(album = ifelse(nchar(albumName) > 15, 
                              str_c(str_sub(albumName, 0, 13), "..."),
                              albumName)) %>% 
        arrange(avRat) %>% 
        mutate(albumName = str_c(albumName, "\n", "(", round(avRat, 1), ")")) %>% 
        mutate(album = forcats::fct_inorder(albumName)) %>% 
        mutate(compRat = (x/y)*avRat) %>% 
        select(-x, -y, -albumName) %>% 
        gather(-album, key = series, value = Rating) %>% 
        ggplot(aes(x = album, y = Rating, group = series, fill = series)) +
        geom_col(position = 'identity', color = colors[6], size = 1) +
        scale_fill_manual(values = c(colors[1], colors[5])) + 
        coord_flip() +
        labs(x = NULL) +
        theme(legend.position = 'none')
      
    }
  )
  
  output$albumRatings2 <- renderPlotly({
    if (str_to_lower(values$chooseArtist) %in% str_to_lower(plexDB$artist)) {
      ratingsPlot2 <- values$ratings %>%
        mutate(rating = ifelse(rating == 0, NA, rating)) %>% 
        na.omit() %>%
        group_by(album) %>% 
        mutate(avRat = mean(rating, na.rm = TRUE)) %>%
        mutate(albumName = as.character(album)) %>% 
        mutate(albumName = ifelse(nchar(albumName) > 15, 
                              str_c(str_sub(albumName, 0, 13), "..."),
                              albumName)) %>% 
        mutate(rating = ifelse(is.na(rating), 0, rating)) %>% 
        mutate(albumLegend = str_c(albumName, ' (', round(avRat, 1), ')')) %>%
        add_count() %>%
        mutate(smooth = ifelse(n > 3,
                               predict(
                                 loess(rating ~ trackNum, span = 3)
                               ),
                               avRat))
     
      numberOfAlbums <- length(unique(ratingsPlot2$albumLegend))
      plot_ly() %>% 
          plotly::add_lines(data = ratingsPlot2,
            x = ~trackNum,y = ~smooth, line = list(shape = 'spline'),
            text = ~albumLegend, hoverinfo = 'text',
            color = ~albumLegend, legendgroup = ~albumLegend,
            colors = colorRampPalette(brewer.pal(11, "Spectral"))(numberOfAlbums)
          ) %>% 
          plotly::add_markers(data = ratingsPlot2,
              x = ~trackNum, y = ~ rating, showlegend = FALSE,
              legendgroup = ~ albumLegend, color = ~albumLegend,
              text = ~ str_c(track, ' (', rating, ')<br>', albumLegend),
              opacity = 0.5, hoverinfo = 'text'
        ) %>%
        plotly::layout(
          xaxis = list(title = NA, zeroline = FALSE),
          yaxis = list(title = NA, zeroline = FALSE),
          ## Another fudge to get margin to adapt to album title length
          legend = list(
            orientation = "h",
            xanchor = "center",
            x = 0.5,
            yanchor = "top",
            y = 0
          )
        )
    }
  })
  
  
  
  
}


# Run the application
shinyApp(ui = ui, server = server)
