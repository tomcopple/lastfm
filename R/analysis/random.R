
library(tidyverse);library(gganimate);require(gifski)
dat <- data.frame(n = 1)

for(i in 1:25) { dat[[as.character(i)]] <- 0}

addOne <- function(x) {
  
  colNum <- floor(runif(1, 2, 27))
  print(colNum)
  
  newRow <- slice(dat, nrow(dat))
  
  newRow[1] <- x + 1
  newRow[colNum] <- newRow[colNum] + 1
  
  return(newRow)
}

for(i in 1:99) {
  
  dat[i+1,] <- addOne(i)
  
}

plotDat <- dat %>% as_tibble() %>% 
  gather(-n, key = episode, value = plays) %>%
  mutate(episode = as.numeric(episode)) %>% 
  arrange(episode) %>% 
  mutate(episode = as.character(episode)) %>% 
  mutate(episode = forcats::fct_inorder(episode))

p1 <- ggplot(plotDat, aes(x = episode, y = plays)) + geom_col()

animate(p1 + transition_states(
  states = n
), nframes = 300, end_pause = 100)

