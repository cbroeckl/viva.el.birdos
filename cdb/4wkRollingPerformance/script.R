getAllMondays <- function(year) {
  days <- as.POSIXlt(paste(year, 1:366, sep="-"), format="%Y-%j")
  Ms <- days[days$wday==1]
  Ms[!is.na(Ms)]  # Needed to remove NA from day 366 in non-leap years
  as.character(Ms)
}
mondays <- getAllMondays(2024)
getAllSundays <- function(year) {
  days <- as.POSIXlt(paste(year, 1:366, sep="-"), format="%Y-%j")
  Ms <- days[days$wday==0]
  Ms[!is.na(Ms)]  # Needed to remove NA from day 366 in non-leap years
  as.character(Ms)
}
sundays <- getAllSundays(2024)
sundays
mondays
opening.day <- 2024-03-28
closing.day <- 2024-09-29

first.monday <- 13
first.sunday <- 13
last.monday <- 39
last.sunday <- 39

## example
## for some reason, you have to put month = '1000' as an option to get startdate to work.  https://github.com/BillPetti/baseballr/issues/358
d <- baseballr::fg_batter_leaders(startseason = '2024', endseason='2024', month = '1000',
                                  startdate = "2024-07-01", enddate = "2024-07-14", qual = '0')
## above does not give stats within timeframe for wRC+
weeks <- last.monday - first.monday - 3

for(i in 1:weeks) {
  tmp <- baseballr::fg_batter_leaders(startdate = mondays[first.monday + i - 1], 
                                      enddate = sundays[first.sunday + i +3], 
                                      qual = '0', month = '1000')
  if(i == 1) {
    out <- cbind(
      week = rep(i, nrow(tmp)),
      tmp
    )
  } else {
    tmp <- cbind(
      week = rep(i, nrow(tmp)),
      tmp
    )
    out <- rbind(out, tmp, fill = TRUE)
  }
}
four.week.rolling.average.2024.batters <- out
plot(four.week.rolling.average.2024.batters$wRC_plus[grep("Judge", four.week.rolling.average.2024.batters$PlayerName)], type = 'l')
save(four.week.rolling.average.2024.batters, file = 'C:/Users/cbroe/OneDrive/Documents/GitHub/viva.el.birdos/cdb/4wkRollingPerformance/four.week.rolling.average.2024.batters.rda')
write.csv(four.week.rolling.average.2024.batters, file = 'C:/Users/cbroe/OneDrive/Documents/GitHub/viva.el.birdos/cdb/4wkRollingPerformance/four.week.rolling.average.2024.batters.csv', row.names = FALSE)
