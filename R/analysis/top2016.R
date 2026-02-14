# Top whatevers of 2016

top2016 <- function(whatever = "album", top = 50, year = "2018") {
    
    require(tidyverse);require(zoo);require(lubridate);require(plotly)
    if(whatever == "artist") {
        top1 <- lastfm %>% 
            filter(year(date) == year) %>% 
            group_by_(whatever) %>% 
            summarise(count = n()) %>% 
            arrange(desc(count)) %>% 
            rename_("name" = whatever) %>% 
            head(top)
        
        # Do album/track differently to make sure artist is the same
    } else if(whatever %in% c("album", "track")) {
        top1 <- lastfm %>% 
            filter(year(date) == year) %>% 
            group_by_(whatever, "artist") %>% 
            summarise(count = n()) %>% ungroup() %>% 
            arrange(desc(count)) %>% 
            head(top) %>% 
            mutate_("name" = whatever) %>% 
            select(name, count) %>% 
            filter(name != "") %>% 
            arrange(desc(count))
    } else return("Type must be either artist, album or track.")
    
    topPlot <- lastfm %>% 
        mutate(date = date(date)) %>% 
        filter(year(date) == year) %>% 
        filter_(paste(whatever, "%in%", top1[1])) %>% 
        select_(whatever, "date") %>% 
        # Get daily play counts
        group_by_(whatever, "date") %>% 
        summarise(count = n()) %>% 
        ungroup() %>% 
        # Rename column of interest to "name"
        rename_("name" = whatever) %>% 
        # Start getting monthly play counts; separate into columns
        spread(key = name, value = count, fill = 0) %>% 
        # Add a sequence of all days in the year
        right_join(
            x = .,
            y = data.frame(
                date = seq.Date(from = as.Date(paste0(year, "-01-01")),
                                to = as.Date(paste0(year, "-12-31")),
                                by = 1)
            ),
            by = "date"
        ) %>% 
        # Gather variables back together
        gather(., -date, key = name, value = count) %>% 
        # Replace NAs with 0
        mutate(count = ifelse(is.na(count), 0, count)) %>% 
        # Group by variable and get rolling 30-day count
        group_by(name) %>% 
        mutate(monthlyPlays = rollsum(count, k = 30, fill = NA, align = "right")) %>% 
        filter(!is.na(monthlyPlays)) %>% 
        as_data_frame()
    
    topPlotGraph <- ggplot(topPlot, 
                           aes(x = date, y = monthlyPlays, fill = name)) +
        geom_area(alpha = 0.5, position = "dodge", size = 1.2) +
        scale_fill_brewer("", palette = "Spectral") +
        scale_x_date(date_labels = "%b %Y") +
        labs(x = "", y = "Monthly count",
             title = paste0("Top ", top, " ", whatever, "s in ", year))
    
    plotlyIt <- ggplotly(topPlotGraph)
    print(plotlyIt)
    print(top1)
}
