# Want to plot a listening chart for a specific album/artist
# NB Spectral has 11 colours so max number of series must be <= 11

plotScrobbles <- function(x, type = "artist", period = 5) {
    library(tidyverse);library(plotly);library(lubridate);library(zoo)
    
    if(type == "artist") {
        scrob1 <- lastfm %>% 
            filter(artist == x,
                   date >= today() - years(period)) %>% 
            mutate(date = date(date)) %>% 
            select(artist, album, date) %>% 
            group_by(album, date) %>% 
            summarise(count = n()) %>% 
            ungroup() %>%
            spread(key = album, value = count, fill = 0) %>% 
            right_join(
                x = .,
                y = data_frame(
                    date = seq.Date(from = today() - years(period),
                                    to = today(),
                                    by = 1)),
                by = "date"
            ) %>%
            gather(., -date, key = album, value = count) %>% 
            mutate(count = ifelse(is.na(count), 0, count)) %>% 
            group_by(album) %>% 
            mutate(monthlyPlays = rollsum(count, k = 30, fill = NA, align = "right")) %>% 
            filter(!is.na(monthlyPlays)) %>% 
            ungroup() %>% 
            mutate(album = ifelse(is.na(album), "Other", album),
                   album = ifelse(album == "<NA>", "Other", album)) 
        
        top11 <- scrob1 %>% 
            group_by(album) %>% 
            summarise(total = sum(count)) %>% 
            ungroup() %>%
            arrange(desc(total)) %>% # Don't use top_n in case of a tie. 
            head(11)
        
        scrob2 <- filter(scrob1, album %in% top11$album)
        
        scrobPlot <- ggplot(scrob2, 
                            aes(x = date, y = monthlyPlays, fill = album)) +
            geom_area(alpha = 0.6, position = "dodge", size = 1.2) +
            scale_fill_brewer("", palette = "Spectral") +
            scale_x_date(date_labels = "%b %Y") +
            labs(x = "", y = "Monthly count",
                 title = paste0("Monthly plays by ", x, " over the last ", period, " years"))
        
    }
    
    ggplotly(scrobPlot)
}
