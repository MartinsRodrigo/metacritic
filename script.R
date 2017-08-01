library(rvest)
library(Rcrawler)
library(tidyverse)
library(lubridate)
library(xlsx)


data <- data.frame(matrix(NA, ncol = 4, nrow = 3700))
link <- vector()


for (i in 0:36) {
  
  pageinfo <- LinkExtractor(paste0("http://www.metacritic.com/browse/games/score/metascore/all/pc/filtered?sort=desc&page=", i), 
                            Timeout = 10,
                            Useragent = "Mozilla/5.0 (Windows; U; Windows NT 5.1; en-GB; rv:1.8.1.6) Gecko/20070725 Firefox/2.0.0.6")
  
  for (j in 1:100) {
    
    
    data[j+(i*100),] <- unlist(
                          ContentScraper(
                            pageinfo[[1]][[10]], 
                              c(paste0("//*[@id='main']/div[2]/div[1]/div[2]/div[3]/div/div/div[",j,"]/div[2]/div"),
                                paste0("//*[@id='main']/div[2]/div[1]/div[2]/div[3]/div/div/div[",j,"]/div[3]/a"),
                                paste0("//*[@id='main']/div[2]/div[1]/div[2]/div[3]/div/div/div[",j,"]/div[4]/span[2]"),
                                paste0('//*[@id="main"]/div[2]/div[1]/div[2]/div[3]/div/div/div[',j,']/div[5]/text()'))))
    
    link <- c(link, read_html(pageinfo[[1]][[10]]) %>% 
                html_nodes(xpath = paste0('//*[@id="main"]/div[2]/div[1]/div[2]/div[3]/div/div/div[',j,']/div[3]/a')) %>%
                html_attr("href")) 
    
  }
  
}

data$X4 <- gsub("Feb", "02", data$X4)
data$X4 <- gsub("Apr", "04", data$X4)
data$X4 <- gsub("May", "05", data$X4)
data$X4 <- gsub("Aug", "08", data$X4)
data$X4 <- gsub("Sep", "09", data$X4)
data$X4 <- gsub("Oct", "10", data$X4)
data$X4 <- gsub("Dec", "12", data$X4)

games <- tibble(title = data$X2[1:length(link)],
                link = paste0("http://www.metacritic.com", unlist(link)),
                critics = as.double(data$X1[1:length(link)]),
                user = as.double(data$X3[1:length(link)])*10,
                date = mdy(data$X4[1:length(link)]))

games$normalized_critics <-(games$critics-min(games$critics))/(max(games$critics)-min(games$critics))
games$normalized_user <- (games$user-min(games$user, na.rm = TRUE))/(max(games$user, na.rm = TRUE)-min(games$user, na.rm = TRUE))

games$scaled_critics <- scale(games$critics)
games$scaled_user <- scale(games$user)


for (k in sort(seq(from = 0.1, to = 1, by = 0.1), decreasing = TRUE)) {
  
  
  games$decil_critics[games$normalized_critics <= k & games$normalized_critics > (k - 0.1)] <- k
  games$decil_user[games$normalized_user <= k & games$normalized_user > (k - 0.1) ] <- k
  
  
}

games$sum_scaled <- games$scaled_critics + games$scaled_user
games$sum_score <- games$normalized_critics + games$normalized_user
games$sum_decil <- games$decil_critics + games$decil_user
games$sum_decil[is.na(games$user)] <- NA

write.xlsx(games, "games.xlsx")





