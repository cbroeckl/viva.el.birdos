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
HBP <- aggregate(milb.batting$HBP, by = list(milb.batting$lv.yr), FUN = "sum", na.rm = TRUE)[,2]
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
  HBP,
  SH,
  SF,
  SB,
  CS
)

lev.average$year.level <- paste(lev.average$year, lev.average$level)

save(lev.average, 
     file = "C:/Users/cbroe/OneDrive/Documents/GitHub/viva.el.birdos/cdb/minor.league.comp.batter/lev.average.Rdata"
)

load("C:/Users/cbroe/OneDrive/Documents/GitHub/viva.el.birdos/cdb/minor.league.comp.batter/lev.average.Rdata")

ggplot(data = lev.average, aes(x = yr, y = SB/(SB+CS), color = lg)) + 
  geom_line() 


## league average is more difficult given source data. I would have to get rid of players who played at multiple levels/leagues in a given year.  
## once i do that, i am not confident the league correction is accurate.  better to just not use it.  instead, correct by level only 
## that is: rookie, A, highA, AA, and AAA
## would have to go back, maybe to invidual player summaries, or get day-by-day data. 

## now, normalize each stat to level.yr average
## create league/year average table, same length of milb.batting.2
head(lev.average)
head(milb.batting.2)
milb.batting.3 <- milb.batting.2
milb.batting.3$year.level <- paste(milb.batting.3$season.min, milb.batting.3$level)
milb.batting.3 <- merge(milb.batting.3, lev.average, by = "year.level", all.x = TRUE, all.y = FALSE)
names(milb.batting.3) <- gsub(".x", "", names(milb.batting.3), fixed = TRUE)

milb.batting.3$BA <- (milb.batting.3$H/milb.batting.3$AB)
milb.batting.3$OBP <- (milb.batting.3$H + milb.batting.3$BB + milb.batting.3$HBP)/(milb.batting.3$AB + milb.batting.3$BB + milb.batting.3$HBP + milb.batting.3$SF)
milb.batting.3$X1B <- milb.batting.3$H - milb.batting.3$X2B - milb.batting.3$X3B - milb.batting.3$HR
milb.batting.3$SLG <- (milb.batting.3$X1B + (2*milb.batting.3$X2B) + (3*milb.batting.3$X3B) + (4*milb.batting.3$HR))/milb.batting.3$AB
milb.batting.3$OPS <- milb.batting.3$OBP + milb.batting.3$SLG
milb.batting.3$ISO <- milb.batting.3$SLG - milb.batting.3$BA
milb.batting.3$ISOP <- milb.batting.3$OBP - milb.batting.3$BA
milb.batting.3$BJM <- (milb.batting.3$X2B + milb.batting.3$X3B + milb.batting.3$HR)/milb.batting.3$AB
milb.batting.3$'BB%' <- milb.batting.3$BB/milb.batting.3$PA
milb.batting.3$'K%' <- milb.batting.3$SO/milb.batting.3$PA

milb.batting.3$BA.y <- (milb.batting.3$H.y/milb.batting.3$AB.y)
milb.batting.3$OBP.y <- (milb.batting.3$H.y + milb.batting.3$BB.y + milb.batting.3$HBP.y)/(milb.batting.3$AB.y + milb.batting.3$BB.y + milb.batting.3$HBP.y + milb.batting.3$SF.y)
milb.batting.3$X1B.y <- milb.batting.3$H.y - milb.batting.3$X2B.y - milb.batting.3$X3B.y - milb.batting.3$HR.y
milb.batting.3$SLG.y <- (milb.batting.3$X1B.y + (2*milb.batting.3$X2B.y) + (3*milb.batting.3$X3B.y) + (4*milb.batting.3$HR.y))/milb.batting.3$AB.y
milb.batting.3$OPS.y <- milb.batting.3$OBP.y + milb.batting.3$SLG.y
milb.batting.3$ISO.y <- milb.batting.3$SLG.y - milb.batting.3$BA.y
milb.batting.3$ISOP.y <- milb.batting.3$OBP.y - milb.batting.3$BA.y
milb.batting.3$BJM.y <- (milb.batting.3$X2B.y + milb.batting.3$X3B.y + milb.batting.3$HR.y)/milb.batting.3$AB.y
milb.batting.3$'BB%.y' <- milb.batting.3$BB.y/milb.batting.3$PA.y
milb.batting.3$'K%.y' <- milb.batting.3$SO.y/milb.batting.3$PA.y

## plus calcs
milb.batting.3$'BA+' <- 100*((milb.batting.3$BA/milb.batting.3$BA.y))
hist(milb.batting.3$'BA+')
milb.batting.3$'OBP+' <- 100*((milb.batting.3$OBP/milb.batting.3$OBP.y))
hist(milb.batting.3$'OBP+')
milb.batting.3$'SLG+' <- 100*((milb.batting.3$SLG/milb.batting.3$SLG.y))
hist(milb.batting.3$'SLG+')
milb.batting.3$'OPS+' <- 100*((milb.batting.3$OPS/milb.batting.3$OPS.y))
hist(milb.batting.3$'OPS+')
milb.batting.3$'ISO+' <- 100*((milb.batting.3$ISO/milb.batting.3$ISO.y))
hist(milb.batting.3$'ISO+')
milb.batting.3$'ISOP+' <- 100*((milb.batting.3$ISOP/milb.batting.3$ISOP.y))
hist(milb.batting.3$'ISOP+')
milb.batting.3$'BJM+' <- 100*((milb.batting.3$BJM/milb.batting.3$BJM.y))
hist(milb.batting.3$'BJM+')
milb.batting.3$'BB%+' <- 100*((milb.batting.3$'BB%'/milb.batting.3$'BB%.y'))
hist(milb.batting.3$'BB%+')
milb.batting.3$'K%+' <- 100*((milb.batting.3$'K%.y'/milb.batting.3$'K%'))
hist(milb.batting.3$'K%+')
save(milb.batting.3, file = "C:/Users/cbroe/OneDrive/Documents/GitHub/viva.el.birdos/cdb/minor.league.comp.batter/milb.batting.3.Rdata")

# load("C:/Users/cbroe/OneDrive/Documents/GitHub/viva.el.birdos/cdb/minor.league.comp.batter/milb.batting.3.Rdata")
# load("C:/Users/cbroeckl/Documents/GitHub/viva.el.birdos/cdb/minor.league.comp.batter/milb.batting.3.Rdata")
# will now need to reshape
## example code: 
library(dplyr)
library(stringr)
library(tidyr)
# df <- tibble(PIN = c(1001, 1001, 1002, 1002), time = c(1, 2, 1, 2), age = c(84, 86, 22, 24), height = c(58, 58, 60, 62))
# df_wide <- reshape(df, 
#                    timevar=c("time"),
#                    idvar=c("PIN"),
#                    dir="wide")
# df %>% 
#   mutate(time = str_c('t', time)) %>%
#   pivot_wider(names_from = time, values_from = c(age, height))

## and for my dataset
milb.batting.4 <- milb.batting.3[c("bbref_id", "name", "age.min", "season.min", "season.max", "level", "PA", "AB", 
                                   # "BA", "OBP", "SLG", "OPS", "ISO", "ISOP", "BJM", "BB%", "K%",
                                   "BA+", "OBP+", "SLG+", "OPS+", "ISO+", "ISOP+", "BJM+", "BB%+", "K%+")]
milb.batting.4$level <- factor(milb.batting.4$level, levels = c("rookie", "a", "higha", "aa", "aaa"))
id.name <- milb.batting.4[,c("bbref_id", "name")]
levels(milb.batting.4$level)

## remove players with fewer than 300 total PA
pa <- aggregate(milb.batting.4$PA, by = list(milb.batting.4$bbref_id), FUN = sum)
keep <- pa[which(pa[,2]>=300),1]
milb.batting.4 <- milb.batting.4[milb.batting.4$bbref_id %in% keep,]
milb.batting.4 <- reshape(milb.batting.4,
                          timevar = "level",
                          idvar = "bbref_id",
                          dir = "wide")
## clean up names
milb.batting.4$name <- sapply(1:nrow(milb.batting.4), FUN = function(x){
  tmp <- unlist(milb.batting.4[x, grep("name.", names(milb.batting.4))])
  tmp[which(!is.na(tmp))[1]]
})
milb.batting.4 <- milb.batting.4[,-grep("name.", names(milb.batting.4))]

rookie <- which(substring(names(milb.batting.4), nchar(names(milb.batting.4))-1, nchar(names(milb.batting.4))) == "ie")
a <- which(substring(names(milb.batting.4), nchar(names(milb.batting.4))-1, nchar(names(milb.batting.4))) == ".a")
higha <- which(substring(names(milb.batting.4), nchar(names(milb.batting.4))-1, nchar(names(milb.batting.4))) == "ha")
aa <- which(substring(names(milb.batting.4), nchar(names(milb.batting.4))-2, nchar(names(milb.batting.4))) == ".aa")
aaa <- which(substring(names(milb.batting.4), nchar(names(milb.batting.4))-3, nchar(names(milb.batting.4))) == ".aaa")

final.data <- data.frame(
  milb.batting.4[,c("bbref_id", "name")],
  milb.batting.4[,rookie],
  milb.batting.4[,a],
  milb.batting.4[,higha],
  milb.batting.4[,aa],
  milb.batting.4[,aaa], 
  check.names = FALSE
)
save(final.data,  file = "C:/Users/cbroe/OneDrive/Documents/GitHub/viva.el.birdos/cdb/minor.league.comp.batter/final.data.Rdata")
# save(final.data,  file = "C:/Users/cbroeckl/Documents/GitHub/viva.el.birdos/cdb/minor.league.comp.batter/final.data.Rdata")
# load("C:/Users/cbroe/OneDrive/Documents/GitHub/viva.el.birdos/cdb/minor.league.comp.batter/final.data.Rdata")
# load("C:/Users/cbroeckl/Documents/GitHub/viva.el.birdos/cdb/minor.league.comp.batter/final.data.Rdata")

## test distance metrics. try euclidian first
## find JJ weatherholt
JJ <- grep("JJ Wetherholt", final.data$name)
final.data[JJ,]

RR <- grep("Rainiel", final.data$name)
final.data[RR,]

# d <- dist(final.data[(JJ-1):(JJ+1),], method = 'manhattan', diag = TRUE)
# d
# 
# # Calculate squared differences, sum, then sqrt
# # For M where rows are vectors:
# 
# # test data
# v <- matrix(c(100, NA, 120, 200, 200, NA, 220, NA, 400, NA), nrow = 2, ncol = 5, byrow = TRUE)
# v
# d.test <- dist(v, diag = TRUE, upper = TRUE)
# d.test

## the above demonstrates that any pairwise non-NA will return a euclidian distance,
## when there are only NA values pairwise, NA is returned 
## this is fine behavior to use. 

# M <- matrix(1.01*c(100, 110, 120), nrow = 1, ncol = 3)
# 
# # euclidian distance
# sq_diffs <- (M - v)^2
# sum_sq <- rowSums(sq_diffs)
# distances <- sqrt(sum_sq)
# print(distances)
# 
# v <- final.data[JJ, 3:ncol(final.data)]
# use <- which(!is.na(v))
# v <- v[,use]
# M <- final.data[(JJ+3), 3:ncol(final.data)]
# M <- M[,use]
# if(any(is.na(M))) {
#   M[which (is.na(M))] <- 0
# }
# 
# ## this works, but is kinda slow.  move subsetting and replacement of NA to DF level
euclid <- function(x, y) {
  use <- which(!is.na(x))
  if(any(is.na(y))) {
    y[is.na(y)] <- 0
  }
  sq_diffs <- (y[use] - x[use])^2
  sum_sq <- rowSums(sq_diffs)
  distance <- sqrt(sum_sq)
  distance <- distance/length(use)
  distance
}


## this function demonstrates the behavior i want.  only considers the non-NA levels for the player of interest. 
## the default dist function is way faster.  to take advantage of that, prefilter by NA pattern
na.pat <- data.frame(
  'rookie' = as.integer(!is.na(final.data$PA.rookie)),
  'a' = as.integer(!is.na(final.data$PA.a)),
  'higha'= as.integer(!is.na(final.data$PA.higha)),
  'aa' = as.integer(!is.na(final.data$PA.aa)),
  'aaa' =as.integer(!is.na(final.data$PA.aaa))
)

na.pat$na.pat.st <- sapply(1:nrow(na.pat), FUN = function(x) paste0(na.pat[x,], collapse = ""))
na.pats <- unique(na.pat$na.pat.st)

## create dataset for distance metric
use.pa <- TRUE
use.ab <- FALSE
use.ops <- FALSE
transform.pa.ab <- 'plus' ## 'sq.rt' or 'plus'
use.age <- TRUE
transform.age <- TRUE
standardize <- TRUE
age.weight <- 0.33


for.sim <- grep("+", names(final.data), fixed = TRUE)
if(use.pa) {for.sim <- c(for.sim,   grep("PA.", names(final.data), fixed = TRUE))}
if(use.ab) {for.sim <- c(for.sim, grep("AB.", names(final.data), fixed = TRUE))}
if(use.age) {for.sim <- c(for.sim, grep("age.min.", names(final.data), fixed = TRUE))}
if(!use.ops) {for.sim <- for.sim[!(for.sim %in% grep("OPS+", names(final.data), fixed = TRUE))]}
for.sim <- final.data[,for.sim]
if(use.age & transform.age) {
  do <- grep("age.min.", names(for.sim), fixed = TRUE)
  for(i in 1:length(do)) {
    for.sim[,do[i]] <- 100*for.sim[,do[i]]/mean(for.sim[,do[i]], na.rm = TRUE)
  }
}
if(use.pa & (transform.pa.ab == 'sq.rt')) {
  do <- grep("PA.", names(for.sim), fixed = TRUE)
  for(i in 1:length(do)) {
    for.sim[,do[i]] <- for.sim[,do[i]]^0.5
  }
}
if(use.pa & (transform.pa.ab == 'plus')) {
  do <- grep("PA.", names(for.sim), fixed = TRUE)
  for(i in 1:length(do)) {
    for.sim[,do[i]] <- 100*for.sim[,do[i]]/mean(for.sim[,do[i]], na.rm = TRUE)
  }
}
if(use.ab & (transform.pa.ab == 'sq.rt')) {
  do <- grep("AB.", names(for.sim), fixed = TRUE)
  for(i in 1:length(do)) {
    for.sim[,do[i]] <- 100*for.sim[,do[i]]/mean(for.sim[,do[i]], na.rm = TRUE)
  }
}
if(standardize) {
  for.sim <- data.frame(scale(for.sim), check.names = FALSE)
}

if(use.age & transform.age) {
  do <- grep("age.min.", names(for.sim), fixed = TRUE)
  for(i in 1:length(do)) {
    for.sim[,do[i]] <- age.weight*for.sim[,do[i]]
  }
}

cols.level <- list(
  rookie = which(substring(names(for.sim), nchar(names(for.sim))-1, nchar(names(for.sim))) == "ie"),
  a = which(substring(names(for.sim), nchar(names(for.sim))-1, nchar(names(for.sim))) == ".a"),
  higha = which(substring(names(for.sim), nchar(names(for.sim))-1, nchar(names(for.sim))) == "ha"),
  aa = which(substring(names(for.sim), nchar(names(for.sim))-2, nchar(names(for.sim))) == ".aa"),
  aaa = which(substring(names(for.sim), nchar(names(for.sim))-3, nchar(names(for.sim))) == ".aaa")
)



comps <- as.list(rep(NA, nrow(final.data)))
names(comps) <- final.data$bbref_id

for(i in 1:length(na.pats)){
  use <- which(as.logical(as.integer(as.numeric(unlist(strsplit(na.pats[i], ""))))))
  cols.selected <- as.vector(unlist(cols.level[use]))
  row.use <- which(rowSums(na.pat[,use, drop = FALSE]) == length(use))
  row.bbref_id <- final.data$bbref_id[row.use]
  record.comps <- final.data$bbref_id[which(na.pat$na.pat.st == na.pats[i])]
  row.name <- final.data$name[row.use]
  fin.sub <- for.sim[row.use ,cols.selected]
  d <- as.matrix(dist(fin.sub))
  d[is.infinite(d)] <- NA
  d.norm <- d/max(d, na.rm = TRUE)
  ## j <- which(row.name == "JJ Wetherholt")
  for(j in 1:nrow(d)) {
    if(row.bbref_id[j] %in% record.comps) {
      if(is.data.frame(comps[[as.character(row.bbref_id[j])]])) {
        stop('position occupied', "i =", i, " j =", j)
      }
      comp.v <- order(d[j,], decreasing = FALSE)[2:101]
      comps[[as.character(row.bbref_id[j])]] <- data.frame(
        'bbref_id' = row.bbref_id[comp.v],
        'name'= row.name[comp.v],
        'similarity' = round(1 - d.norm[j, comp.v],3)
      )
    }
    
  }
}

# RR <- grep("Rainiel", final.data$name)
# comps[[as.character(final.data[RR,"bbref_id"])]][1:20,]
# 
# 
# JJ.comp <- final.data[final.data$bbref_id %in% c(final.data[JJ,"bbref_id"], comps[[as.character(final.data[JJ,"bbref_id"])]]$bbref_id[1:10]),]
# JJ.comp <- final.data[c(JJ, match(comps[[as.character(final.data[JJ,"bbref_id"])]]$bbref_id, final.data$bbref_id)),]
# JJ.comp$name
# 
# JW <- grep("Jordan Walker", final.data$name)
# comps[[as.character(final.data[JW,"bbref_id"])]][1:10,]
# JJ.comp <- final.data[final.data$bbref_id %in% c(final.data[JJ,"bbref_id"], comps[[as.character(final.data[JJ,"bbref_id"])]]$bbref_id[1:10]),]
# JJ.comp <- final.data[c(JJ, match(comps[[as.character(final.data[JJ,"bbref_id"])]]$bbref_id, final.data$bbref_id)),]
# JJ.comp$name
# 
# JJ <- grep("JJ Wetherholt", final.data$name)
# comps[[as.character(final.data[JJ,"bbref_id"])]][1:10,]
# JW <- grep("Jordan Walker", final.data$name)
# comps[[as.character(final.data[JW,"bbref_id"])]][1:10,]
# VS <- grep("Victor Scott", final.data$name)
# comps[[as.character(final.data[VS,"bbref_id"])]][1:10,]
# RR <- grep("Rainiel", final.data$name)[1]
# comps[[as.character(final.data[JC,"bbref_id"])]][1:10,]
# 
# 
# comps[[as.character(802139)]][1:10,]
# 
# 
# table(comps[[as.character(final.data[JW,"bbref_id"])]][,"bbref_id"] %in% comps[[as.character(final.data[JJ,"bbref_id"])]][,"bbref_id"])

save(comps, file = "C:/Users/cbroe/OneDrive/Documents/GitHub/viva.el.birdos/cdb/minor.league.comp.batter/comps.Rdata")
# save(comps, file = "C:/Users/cbroeckl/Documents/GitHub/viva.el.birdos/cdb/minor.league.comp.batter/comps.Rdata")

# dists <- rep(NA, nrow(final.data))
# for(i in 1:length(dists)) {
#   dists[i] <- euclid(x = final.data[JJ,3:ncol(final.data)], y = final.data[i,3:ncol(final.data)])
# }

# 
# ## try again.
# metadata <- c("bbref_id", "name")
# metadata <- c(metadata, names(final.data)[grep('PA.', names(final.data))], names(final.data)[grep('AB.', names(final.data))], names(final.data)[grep('season', names(final.data))])
# tmp <- final.data[,!(names(final.data) %in% metadata)]
# tmp <- as.matrix(tmp)
# tmp[which(is.nan((tmp)))] <- NA
# tmp[which(is.na(tmp))] <- 0
# ## select only "+" stats
# # plus <- grep("+", names(tmp), fixed = TRUE)
# # tmp <- tmp[,plus]
# 
# ## calculate full matrix, extract top 100 similar for each player, excluding self
# tar <- grep('JJ Wetherholt', final.data$name )
# d <- dist(tmp, diag = TRUE, upper = FALSE)
# object.size(d)
# d <- as.matrix(d)
# object.size(d)
# range(d, na.rm = TRUE)
# d.norm <- d/max(d, na.rm = TRUE)
# 
# use <- order(d.norm[tar,], decreasing = FALSE)[1:11]
# final.data[tar,1:4]
# final.data[use,1:4]
# d.norm[tar,use]
# final.data[use[1:3],]
# tmp[use[1:3],]
# 
# dist(tmp[use[1:2],], diag = TRUE, upper = FALSE)
# 
# 
# 
# final.data[tar, 1:4]
# tar.set <- tmp[tar, ]
# use <- which(!is.na(tar.set))
# tar.set <- tar.set[,use]
# tmp <- tmp[,use]
# tmp[(is.na(tmp))] <- 0
# 
# euclid <- function(x, y) {
#   sq_diffs <- (y - x)^2
#   sum_sq <- rowSums(sq_diffs)
#   distance <- sqrt(sum_sq)
#   distance
# }
# 
# dists <- rep(NA, nrow(final.data))
# for(i in 1:length(dists)) {
#   dists[i] <- euclid(x = tar.set, y = tmp[i,])
# }
# 
# use <- order(dists, decreasing = FALSE)[1:11]
# cat(paste(final.data[use,'name'], collapse = '\n'))
# display <- final.data[use,]
# row.names(display) <- display$name
# display <- display[,-which(names(display) == "name")] 
# display <- display[,grep("+", names(display), fixed = TRUE)]
# t(round(display))[,]
# cat(paste(final.data[use,'name'], collapse = '\n'))


## get MLB level data for same time frame
# baseballr::bref_daily_batter()

yrs <- c(2008:2025)
# levs <- c("aaa", "a")
mlb.list <- as.list(vector(length = 0))
for(yr in yrs) {
  
  Sys.sleep(2)
  closeAllConnections()
  dt <- paste0(yr, c("-01-01", "-12-31"))
  # if(yr >= 2019 & lev == 'rk') {
  #   lev == "rookie"
  # }
  d <- tryCatch({
    baseballr::fg_batter_leaders(startseason = as.character(yr), endseason = as.character(yr))
  }, error = function(msg){
    return(NA)
  }
  )
  
  
  if(is.data.frame(d)) {
    d <- d[,c("Season", "PlayerName", "xMLBAMID", "Offense", "wRC_plus", "PA")]
    mlb.list[[length(mlb.list)+1]] <- d
  }
  rm(d)
}

mlb <- dplyr::bind_rows(mlb.list)
all.mlb.ids <- unique(mlb$xMLBAMID)
length(all.mlb.ids)
mlb.6yr <- data.frame(
  bbref_id = vector(length = 0, mode = 'character'),
  Offense = vector(length = 0, mode = 'numeric'),
  wRC_plus = vector(length = 0, mode = 'numeric'),
  PA = vector(length = 0, mode = 'integer'),
  n.season = vector(length = 0, mode = 'integer'),
  most.recent.mlb.season = vector(length = 0, mode = 'integer')
)
for(i in 1:length(all.mlb.ids)) {
  use <- which(mlb$xMLBAMID == all.mlb.ids[i])
  if(length(use) > 6) use <- use[1:6]
  n.years <- length(use)
  
  
  tmp <- data.frame(
    bbref_id = all.mlb.ids[i],
    Offense = sum(mlb$Offense[use], na.rm = TRUE),
    wRC_plus = weighted.mean(mlb$wRC_plus[use], weights = mlb$PA[use], na.rm = TRUE),
    PA = sum(mlb$PA[use]),
    n.season = n.years,
    most.recent.mlb.season = max(mlb$Season[use], na.rm = TRUE)
  )
  mlb.6yr <- rbind(mlb.6yr, tmp)
}
head(mlb.6yr)
mlb.6yr$Offense.600PA <- 600*mlb.6yr$Offense/mlb.6yr$PA

# final.data <- final.data[,-c(which(names(final.data) == 'Offense'): ncol(final.data))]

final.data <- merge(final.data, mlb.6yr, by = 'bbref_id', all.x = TRUE, all.y = FALSE)
head(final.data)

most.recent.milb.season <- sapply(1:nrow(final.data), FUN = function(x) {
  max(final.data[x, grep('season.max.', names(final.data))], na.rm = TRUE)
}
)
final.data$most.recent.milb.season <- most.recent.milb.season
final.data$most.recent.season <- pmax(final.data$most.recent.milb.season, final.data$most.recent.mlb.season, na.rm = TRUE)
current.year <- max(final.data$most.recent.mlb.season, na.rm = TRUE)
final.data$out.of.baseball <- sapply(final.data$most.recent.season, FUN = function(x) {(current.year - x) >=2})
head(final.data)
save(comps, file = "C:/Users/cbroeckl/Documents/GitHub/viva.el.birdos/cdb/minor.league.comp.batter/mlb.Rdata")
save(comps, file = "C:/Users/cbroeckl/Documents/GitHub/viva.el.birdos/cdb/minor.league.comp.batter/final.data.Rdata")


JJ <- grep("JJ Wetherholt", final.data$name)
JW <- grep("Jordan Walker", final.data$name)
VS <- grep("Victor Scott", final.data$name)
BT <- grep("Bryan Torres", final.data$name)
# RR <- grep("Rodriguez", final.data$name)

library(tidyverse)

JJ.comp <- comps[[as.character(final.data$bbref_id[JJ])]]$bbref_id
JJ.comp <- final.data[final.data$bbref_id %in% JJ.comp,]
JW.comp <- comps[[as.character(final.data$bbref_id[JW])]]$bbref_id
JW.comp <- final.data[final.data$bbref_id %in% JW.comp,]
VS.comp <- comps[[as.character(final.data$bbref_id[VS])]]$bbref_id
VS.comp <- final.data[final.data$bbref_id %in% VS.comp,]
BT.comp <- comps[[as.character(final.data$bbref_id[BT])]]$bbref_id
BT.comp <- final.data[final.data$bbref_id %in% BT.comp,]
RR.comp <- comps[[as.character(823787)]]$bbref_id
RR.comp <- final.data[final.data$bbref_id %in% RR.comp,]


comp.comp <- rbind(
  data.frame(player = "JJ", JJ.comp, check.names = FALSE), 
  data.frame(player = "JW", JW.comp, check.names = FALSE), 
  data.frame(player = "VS", VS.comp, check.names = FALSE),
  data.frame(player = "RR", RR.comp, check.names = FALSE)
)

library(plotly)
give.n <- function(x) {
  return(c(y = max(x), label = paste0("n = ", length(x))))
}

players <- unique(comp.comp$player)
failure.rate <- sapply(players, FUN = function(x) {
  tmp <- comp.comp[which(comp.comp$player == x),]
  round(100*length(which(is.na(tmp$wRC_plus)))/nrow(tmp))
})
failure.rate <- data.frame(
  player = names(failure.rate),
  failure.rate
)


gg <- comp.comp %>%
  ggplot(aes(x = player, y=wRC_plus, fill = player))+
  geom_violin()+
  geom_jitter()+
  theme(legend.position = "none") +
  geom_text(data = failure.rate,
            aes(label = paste0(failure.rate, "%"),
                y = 1.05*max(comp.comp$wRC_plus, na.rm = TRUE),
                hjust = 0.5, vjust = 0, size = 5)
  )
gg
gg <- ggplotly(gg)
str(gg)

# df <- read.csv("https://raw.githubusercontent.com/plotly/datasets/master/violin_data.csv")

library(plotly)
fig <- comp.comp %>%
  plot_ly(
    y = ~wRC_plus,
    x = ~player,
    type = 'violin',
    box = list(
      visible = T
    ),
    scatter = list(
      visible = T
    ),
    meanline = list(
      visible = T
    )
  ) 


failure.rate <- sapply(players, FUN = function(x) {
  tmp <- comp.comp[which(comp.comp$player == x),]
  round(100*length(which(is.na(tmp$wRC_plus)))/nrow(tmp))
})

fig <- comp.comp %>% 
  plot_ly(y = ~wRC_plus, x = ~player, type = "box", boxpoints = "all",
          text = ~name,
          size = ~n.season,
          jitter = 1, pointpos = 0,
          # hovertemplate = "%{name}: <br>Popularity: %{wRC_plus} </br> %{PA}"
          hoverinfo = 'text'
  )
for (i in 1:length(failure.rate)) {
  fig <- fig %>% add_annotations(
    x = names(failure.rate)[i],         # X position based on Species name
    y = 1.05*max(comp.comp$wRC_plus, na.rm = TRUE),    # Y position (adjust as needed for placement)
    text = paste0(failure.rate[i], "%"), # The annotation text
    xref = "x",                   # Reference the x-axis
    yref = "y",                   # Reference the y-axis
    showarrow = FALSE,            # Do not show an arrow pointing to the value
    yshift = 0                   # Slightly shift up from the max value
  )
}

fig 


table(d$organizationReferenceId)
d.sub <- d[which(d$organizationReferenceId == "ARBIICIT"),]
aggregate(d.sub$Quantity, by = list(d.sub$Price.Type), FUN = 'sum')
