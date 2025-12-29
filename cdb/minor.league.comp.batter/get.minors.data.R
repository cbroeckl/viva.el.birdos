library(baseballr)
library(tidyverse)

# This function requires both a start date and an end date, written in the format "YYYY-MM-DD". 
# By default, it only returns Triple-A data. To get data from other levels, set the level 
# argument to one of the following: "aaa", "aa", "higha", "a", "lowa"
# from: https://github.com/BillPetti/baseballr/issues/257

bref_daily_batter_milb <- function(t1, t2, level) {
  
  # t1 = "2019-01-01"; t2 = "2019-12-25"; level = "aaa"
  payload <- xml2::read_html(
    paste0("http://www.baseball-reference.com/leagues/daily.cgi?user_team=&bust_cache=&type=b&lastndays=7&dates=fromandto&fromandto=", 
           t1, ".", t2, "&level=", level, "&franch=&stat=&stat_value=0")
  )
  
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

yrs <- c(2008:2019, 2021:2025)
# yrs <- c(2000, 2005, 2010)
levs <- c("aaa", "aa", "higha", "a", "lowa","rk", "rookie")
# levs <- c("aaa", "a")
stats.list <- as.list(vector(length = 0))
for(yr in yrs) {
  for(lev in levs) {
    # yr <- 2025
    # lev <- "rk"
    
    Sys.sleep(10)
    closeAllConnections()
    dt <- paste0(yr, c("-01-01", "-12-31"))
    cat(paste(dt[1], dt[2], lev, '\n'))
    # if(yr >= 2019 & lev == 'rk') {
    #   lev == "rookie"
    # }
    d <- tryCatch({
      bref_daily_batter_milb(t1 = dt[1], t2 = dt[2], level = lev)
      }, error = function(msg){
        return(NA)
      }
    )
    
    # if(!is.data.frame(d)) {
    #   Sys.sleep(81)
    #   cat(" -- attempt 2", '\n')
    #   d <- tryCatch({
    #     bref_daily_batter_milb(t1 = dt[1], t2 = dt[2], level = lev)
    #     }, error = function(msg){
    #       return(NA)
    #     }
    #   )
    # }
    # 
    # if(!is.data.frame(d)) {
    #   Sys.sleep(55)
    #   cat(" -- attempt 3", '\n')
    #   d <- tryCatch({
    #     bref_daily_batter_milb(t1 = dt[1], t2 = dt[2], level = lev)
    #     }, error = function(msg){
    #       return(NA)
    #     }
    #   )
    # }
    
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

save(stats.list, file = "C:/Users/cbroe/OneDrive/Documents/GitHub/viva.el.birdos/cdb/minor.league.comp.batter/minor.league.batters.Rdata")
years.collected <- vector()
levels.collected <- vector()
actual.levels.collected <- vector()
nrows.collected <- vector()
for(i in 1:length(stats.list)){
  years.collected <- c(years.collected, stats.list[[i]][1,1])
  levels.collected <- c(levels.collected, stats.list[[i]][1,2])
  actual.levels.collected <- c(actual.levels.collected, stats.list[[i]][1,6])
  nrows.collected <- c(nrows.collected, nrow(stats.list[[i]]))
}
table(years.collected)
cbind(years.collected, levels.collected)

# load("C:/Users/cbroe/OneDrive/Documents/GitHub/viva.el.birdos/cdb/minor.league.comp.batter/minor.league.batters.Rdata")

library(dplyr)
col.names <- unique(unlist(sapply(1:length(stats.list), FUN = function(x) names(stats.list[[x]]))))
milb.batting <- data.frame(matrix(nrow = 0, ncol = length(col.names)))
names(milb.batting) <- col.names
for(i in 1:length(stats.list)) {
  if(!grepl("Maj", stats.list[[i]][1,6])) {
    tmp <- milb.batting[0,]
    tmp[1:nrow(stats.list[[i]]),names(stats.list[[i]])] <- stats.list[[i]][,names(stats.list[[i]])]
    milb.batting <- rbind(milb.batting, stats.list[[i]])
  }
}

## change all lowa to rookie to enable alignment
milb.batting$level[which(milb.batting$level == "lowa")] <- 'rookie'


save(milb.batting, 
     file = "C:/Users/cbroe/OneDrive/Documents/GitHub/viva.el.birdos/cdb/minor.league.comp.batter/minor.league.batters.raw.Rdata"
)

# load("C:/Users/cbroe/OneDrive/Documents/GitHub/viva.el.birdos/cdb/minor.league.comp.batter/minor.league.batters.raw.Rdata")

milb.batting$group.by <- paste(milb.batting$level, milb.batting$bbref_id)

milb.batting.2 <- milb.batting %>%
  group_by(group.by) %>%
  summarise(
    bbref_id = bbref_id[1],
    level = level[1],
    name = Name[1],
    age.min = min(Age),
    age.max = max(Age),
    G = sum(G),
    PA = sum(PA),
    AB = sum(AB),
    H = sum(H),
    R = sum(R),
    X2B = sum(X2B),
    X3B = sum(X3B),
    HR = sum(HR),
    RBI = sum(RBI),
    BB = sum(BB),
    IBB = sum(IBB),
    SO = sum(SO),
    HBP = sum(HBP),
    SH = sum(SH),
    SF = sum(SF),
    GDP = sum(GDP),
    SB = sum(SB),
    CS = sum(CS),
    season.min = min(season),
    season.max = max(season)
  )
dim(milb.batting.2)
head(milb.batting.2, 20)
n.levels.per.player <- table(milb.batting.2$bbref_id)
hist(n.levels.per.player)
table(milb.batting.2$level)
milb.batting.2$BA <- (milb.batting.2$H/milb.batting.2$AB)
milb.batting.2$OBP <- (milb.batting.2$H + milb.batting.2$BB + milb.batting.2$HBP)/(milb.batting.2$AB + milb.batting.2$BB + milb.batting.2$HBP + milb.batting.2$SF)
milb.batting.2$X1B <- milb.batting.2$H - milb.batting.2$X2B - milb.batting.2$X3B - milb.batting.2$HR
milb.batting.2$SLG <- (milb.batting.2$X1B + (2*milb.batting.2$X2B) + (3*milb.batting.2$X3B) + (4*milb.batting.2$HR))/milb.batting.2$AB
milb.batting.2$OPS <- milb.batting.2$OBP + milb.batting.2$SLG
milb.batting.2$ISO <- milb.batting.2$SLG - milb.batting.2$BA
milb.batting.2$ISOP <- milb.batting.2$OBP - milb.batting.2$BA
milb.batting.2$BJM <- (milb.batting.2$X2B + milb.batting.2$X3B + milb.batting.2$HR)/milb.batting.2$AB
milb.batting.2$'BB%' <- milb.batting.2$BB/milb.batting.2$PA
milb.batting.2$'K%' <- milb.batting.2$SO/milb.batting.2$PA
milb.batting$lv.yr <- paste(milb.batting$level, milb.batting$season, sep = "_") 

G <- aggregate(milb.batting$G, by = list(milb.batting$lv.yr), FUN = "sum")
lg <- sapply(1:nrow(G), FUN = function(x) unlist(strsplit(G[x,1], "_"))[1])
yr <- as.numeric(sapply(1:nrow(G), FUN = function(x) unlist(strsplit(G[x,1], "_"))[2]))
G <- aggregate(milb.batting$G, by = list(milb.batting$lv.yr), FUN = "sum")[,2]
PA <- aggregate(milb.batting$PA, by = list(milb.batting$lv.yr), FUN = "sum")[,2]
AB <- aggregate(milb.batting$AB, by = list(milb.batting$lv.yr), FUN = "sum", na.rm = TRUE)[,2]
H <- aggregate(milb.batting$H, by = list(milb.batting$lv.yr), FUN = "sum")[,2]
R <- aggregate(milb.batting$R, by = list(milb.batting$lv.yr), FUN = "sum")[,2]
X2B <- aggregate(milb.batting$X2B, by = list(milb.batting$lv.yr), FUN = "sum")[,2]
X3B <- aggregate(milb.batting$X3B, by = list(milb.batting$lv.yr), FUN = "sum")[,2]
HR <- aggregate(milb.batting$HR, by = list(milb.batting$lv.yr), FUN = "sum")[,2]
RBI <- aggregate(milb.batting$RBI, by = list(milb.batting$lv.yr), FUN = "sum")[,2]
BB <- aggregate(milb.batting$BB, by = list(milb.batting$lv.yr), FUN = "sum", na.rm = TRUE)[,2]
IBB <- aggregate(milb.batting$IBB, by = list(milb.batting$lv.yr), FUN = "sum", na.rm = TRUE)[,2]
SO <- aggregate(milb.batting$SO, by = list(milb.batting$lv.yr), FUN = "sum", na.rm = TRUE)[,2]
HPB <- aggregate(milb.batting$HBP, by = list(milb.batting$lv.yr), FUN = "sum", na.rm = TRUE)[,2]
SH <- aggregate(milb.batting$SH, by = list(milb.batting$lv.yr), FUN = "sum", na.rm = TRUE)[,2]
SF <- aggregate(milb.batting$SF, by = list(milb.batting$lv.yr), FUN = "sum", na.rm = TRUE)[,2]
SB <- aggregate(milb.batting$SB, by = list(milb.batting$lv.yr), FUN = "sum", na.rm = TRUE)[,2]
CS <- aggregate(milb.batting$CS, by = list(milb.batting$lv.yr), FUN = "sum", na.rm = TRUE)[,2]

test <- data.frame(lg, yr, BA = H/AB)
ggplot(data = test, aes(x = yr, y = BA, color = lg)) + 
  geom_line() 

lev.average <- data.frame(
  year = yr, 
  level = lg,
  PA, 
  AB,
  H,
  R,
  X2B,
  X3B,
  HR,
  RBI,
  BB,
  IBB,
  SO,
  HPB,
  SH,
  SF,
  SB,
  CS
)
save(lev.average, 
     file = "C:/Users/cbroe/OneDrive/Documents/GitHub/viva.el.birdos/cdb/minor.league.comp.batter/lev.average.Rdata"
)


ggplot(data = lev.average, aes(x = yr, y = SB/(SB+CS), color = lg)) + 
  geom_line() 






G <- aggregate(milb.batting$G, by = list(milb.batting$Level), FUN = "sum")
lg <- sapply(1:nrow(G), FUN = function(x) unlist(strsplit(G[x,1], "_"))[1])
yr <- as.numeric(sapply(1:nrow(G), FUN = function(x) unlist(strsplit(G[x,1], "_"))[2]))
G <- aggregate(milb.batting$G, by = list(milb.batting$Level), FUN = "sum")[,2]
PA <- aggregate(milb.batting$PA, by = list(milb.batting$Level), FUN = "sum")[,2]
AB <- aggregate(milb.batting$AB, by = list(milb.batting$Level), FUN = "sum", na.rm = TRUE)[,2]
H <- aggregate(milb.batting$H, by = list(milb.batting$Level), FUN = "sum")[,2]
R <- aggregate(milb.batting$R, by = list(milb.batting$Level), FUN = "sum")[,2]
X2B <- aggregate(milb.batting$X2B, by = list(milb.batting$Level), FUN = "sum")[,2]
X3B <- aggregate(milb.batting$X3B, by = list(milb.batting$Level), FUN = "sum")[,2]
HR <- aggregate(milb.batting$HR, by = list(milb.batting$Level), FUN = "sum")[,2]
RBI <- aggregate(milb.batting$RBI, by = list(milb.batting$Level), FUN = "sum")[,2]
BB <- aggregate(milb.batting$BB, by = list(milb.batting$Level), FUN = "sum", na.rm = TRUE)[,2]
IBB <- aggregate(milb.batting$IBB, by = list(milb.batting$Level), FUN = "sum", na.rm = TRUE)[,2]
SO <- aggregate(milb.batting$SO, by = list(milb.batting$Level), FUN = "sum", na.rm = TRUE)[,2]
HPB <- aggregate(milb.batting$HBP, by = list(milb.batting$Level), FUN = "sum", na.rm = TRUE)[,2]
SH <- aggregate(milb.batting$SH, by = list(milb.batting$Level), FUN = "sum", na.rm = TRUE)[,2]
SF <- aggregate(milb.batting$SF, by = list(milb.batting$Level), FUN = "sum", na.rm = TRUE)[,2]
SB <- aggregate(milb.batting$SB, by = list(milb.batting$Level), FUN = "sum", na.rm = TRUE)[,2]
CS <- aggregate(milb.batting$CS, by = list(milb.batting$Level), FUN = "sum", na.rm = TRUE)[,2]

lg.average <- data.frame(
  year = yr, 
  level = lg,
  PA, 
  AB,
  H,
  R,
  X2B,
  X3B,
  HR,
  RBI,
  BB,
  IBB,
  SO,
  HPB,
  SH,
  SF,
  SB,
  CS
)
save(lg.average, 
     file = "C:/Users/cbroe/OneDrive/Documents/GitHub/viva.el.birdos/cdb/minor.league.comp.batter/lg.average.Rdata"
)
ggplot(data = lg.average, aes(x = yr, y = SB/(SB+CS), color = level)) + 
  geom_line() 
## this isn't useful, unless i get rid of players who played at multiple levels/leagues in a given year.  
## once i do that, i am not confident the league correction is accurate.  better to just not use it.  instead, correct by level only 
## that is: rookie, A, highA, AA, and AAA


## now, normalize each stat to level.yr average
## create league/year average table, same length of milb.batting.2
denom <- milb.batting.2[,1:6]

milb.batting.3$BA <- (milb.batting.2$H/milb.batting.2$AB)
milb.batting.3$OBP <- (milb.batting.2$H + milb.batting.2$BB + milb.batting.2$HBP)/(milb.batting.2$AB + milb.batting.2$BB + milb.batting.2$HBP + milb.batting.2$SF)
milb.batting.3$X1B <- milb.batting.2$H - milb.batting.2$X2B - milb.batting.2$X3B - milb.batting.2$HR
milb.batting.3$SLG <- (milb.batting.2$X1B + (2*milb.batting.2$X2B) + (3*milb.batting.2$X3B) + (4*milb.batting.2$HR))/milb.batting.2$AB
milb.batting.3$OPS <- milb.batting.2$OBP + milb.batting.2$SLG
milb.batting.3$ISO <- milb.batting.2$SLG - milb.batting.2$BA
milb.batting.3$ISOP <- milb.batting.2$OBP - milb.batting.2$BA
milb.batting.3$BJM <- (milb.batting.2$X2B + milb.batting.2$X3B + milb.batting.2$HR)/milb.batting.2$AB
milb.batting.3$'BB%' <- milb.batting.2$BB/milb.batting.2$PA
milb.batting.3$'K%' <- milb.batting.2$SO/milb.batting.2$PA


