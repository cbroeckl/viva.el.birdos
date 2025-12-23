library(baseballr)
library(tidyverse)

# This function requires both a start date and an end date, written in the format "YYYY-MM-DD". 
# By default, it only returns Triple-A data. To get data from other levels, set the level 
# argument to one of the following: "aaa", "aa", "higha", "a", "lowa"
# from: https://github.com/BillPetti/baseballr/issues/257

bref_daily_batter_milb <- function(t1, t2, level = "aaa") {
  payload <- xml2::read_html(paste0("http://www.baseball-reference.com/leagues/daily.cgi?user_team=&bust_cache=&type=b&lastndays=7&dates=fromandto&fromandto=", t1, ".", t2, "&level=", level, "&franch=&stat=&stat_value=0"))
  
  df <- payload %>%
    rvest::html_elements(xpath = '//*[@id="daily"]') %>%
    rvest::html_table(fill = TRUE) %>% 
    as.data.frame() %>% 
    dplyr::filter(Rk != "Rk") %>% # Remove headers
    dplyr::select(-c(1, 3, 5))
  
  names(df)[1:5] <- c("Name", "Age", "Level", "Team", "Affiliate")
  
  suppressWarnings(
    df[,c(2,6:26)] <- lapply(df[,c(2,6:26)],as.numeric)
  )
  
  df$X1B <- with(df, H-(X2B+X3B+HR))
  df$season <- as.integer(substr(t1, 1, 4))
  df$uBB <- with(df, BB-IBB)
  
  # df <- df[,c(29, 1:10, 28, 11:16, 30, 17:27)]  # this seems to break things, sometimes.
  df$Team <- gsub(" $", "", df$Team, perl=T)
  df <- df %>% 
    dplyr::filter(.data$Name != "Name")
  
  playerids <- payload %>%
    rvest::html_elements("table") %>%
    rvest::html_elements("a") %>%
    rvest::html_attr("href") %>%
    as.data.frame() %>%
    dplyr::filter(stringr::str_detect(., pattern = "mlb_ID=\\d{6}")) %>%
    dplyr::mutate(playerid = as.numeric(str_extract(string = ., pattern = "\\d{6}")))
  
  df <- df %>%
    dplyr::mutate(bbref_id = playerids$playerid) %>%
    dplyr::select("bbref_id", tidyr::everything()) %>% dplyr::arrange(desc(.data$PA), desc(.data$OPS))
  
  return(df)
}

yrs <- c(2000:2019, 2021:2025)
levs <- c("aaa", "aa", "higha", "a", "lowa", "rk")
stats.list <- list(length = 0)
for(yr in yrs) {
  for(lev in levs) {
    dt <- paste0(yr, c("-1-01", "-12-31"))
    d <- tryCatch(
      #this is the chunk of code we want to run
      {bref_daily_batter_milb(t1 = dt[1], t2 = dt[2], level = lev)
        #when it throws an error, the following block catches the error
      }, error = function(msg){
        return(NA)
      }
    )
    
    if(is.data.frame(d)) {
      d <- data.frame(
        "year" = rep(yr, nrow(d)),
        "level" = rep(lev, nrow(d)),
        d
      )
      stats.list[[length(stats.list)+1]] <- d
    }
    rm(d)
  }
}

save(stats.list, file = "C:/Users/cbroeckl/Documents/GitHub/viva.el.birdos/cdb/minor.league.comp.batter/minor.league.batters.Rdata")

