# Script to take top ten artist/artists from last 12 months, then chart plays
# Group by last 7 days? Try different options. 

goTop10 <- function(choosetype = "artist", howlong = 1) {
    
    library(tidyverse);library(lubridate);library(plotly);library(zoo)
    
    # Cbeck that lastm data exists first
    if(!exists("lastfm")) {return("Need to run getLastfm.R first")}
    
    # Get data for last twelve months
    last12months <- lastfm %>% 
        filter(date >= today() - years(howlong)) %>% 
        mutate(date = date(date)) %>% 
        as_data_frame()
    
    # Get top ten of name
    # If track or album, need to make sure it's the right one
    if(choosetype %in% c("track", "album")){
        top10 <- last12months %>% 
            group_by_(choosetype, "artist") %>% 
            summarise(count = n()) %>% ungroup() %>% 
            arrange(desc(count)) %>% head(10) %>% 
            mutate_("name" = choosetype) %>% 
            select(name, count) %>% 
            filter(name != "") %>% 
            arrange(desc(count))
    } else {
        top10 <- last12months %>% 
            group_by_(choosetype) %>% 
            summarise(count = n()) %>% 
            arrange(desc(count)) %>% head(10) %>% 
            mutate_("name" = choosetype) %>% 
            select(name, count) %>% 
            filter(name != "") %>% 
            arrange(desc(count))
    }
    
    # Get play counts over the last month
    # Start with daily play counts
    daily <- lastfm %>% 
        filter(date >= today() - months((howlong*12)+1)) %>% 
        mutate(date = date(date)) %>% 
        # Need to include an extra month because of lag
        filter_(paste(choosetype, "%in%", top10[1])) %>% 
        select_(choosetype, "date") %>% 
        group_by_(choosetype, "date") %>% 
        summarise(count = n()) %>% 
        ungroup() %>% 
        mutate_("name" = choosetype) %>% 
        select(date, name, count) %>% 
        as_data_frame()
    
    daily2 <- daily %>% 
        spread_(key = "name", value = "count", fill = 0) %>% 
        merge(
            x = ., 
            y = data.frame(
                date = seq.Date(from = today() - months((howlong*12)+1),
                                to = today(),
                                by = 1)),
            by = "date",
            all = TRUE
        ) %>% 
        gather(., -date, key = name, value = count) %>%
        mutate(count = ifelse(is.na(count), 0, count)) %>% 
        group_by(name) %>% 
        mutate(monthlyPlays = rollsum(count, k = 30, fill = NA, align = "right")) %>% 
        filter(!is.na(monthlyPlays)) %>% 
        as_data_frame()
    
    top10graph <- daily2 %>% 
        group_by(name) %>% 
        plot_ly(x = ~date, y = ~monthlyPlays, color = ~name, type = "scatter", mode = "lines", fill = "tozeroy", colors = "Spectral") %>% 
        layout(
            xaxis = list(title = ""),
            yaxis = list(title = "Monthly plays"),
            title = stringr::str_c("Top ten ", choosetype, "s over the last ", howlong * 12, " months")
        )
    
    print(top10graph)
}
