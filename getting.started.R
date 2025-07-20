## originally published here: 
##  https://www.vivaelbirdos.com/f/2024/2/8/24066704/r-rstudio-baseballr-data-access-and-visualization
## slightly modified below, to use stable release of baseballr

## START


## you will want to download and install R
## https://cran.r-project.org/bin/windows/base/
## and Rstudio
## https://posit.co/download/rstudio-desktop/

## install the latest stable version of the baseballr package: 
install.packages('gtools')

## install gtools - we use a function within this to align data across years
install.packages('gtools')


## load the packages
library(baseballr)
library(gtools)

## set for the most recent completed season.  simply change to 2025 next year! 
current.year <- 2024

## get all draft data from current.year and all years prior to 1990
draft <- data.frame(baseballr::get_draft_mlb(current.year))
for(i in (current.year-1):1980) {
  tmp <- data.frame(baseballr::get_draft_mlb(i))
  draft <- gtools::smartbind(
    draft,
    tmp
  )
  cat(i, nrow(draft), '\n')
}

## get all fg batter leaderboard data from current.year and all years prior to 2000
fg.batter <- data.frame(baseballr::fg_batter_leaders(startseason = current.year, endseason = current.year, qual = 0))
for(i in (current.year-1):1990) {
  tmp <- data.frame(baseballr::fg_batter_leaders(startseason = i, endseason = i))
  fg.batter <- gtools::smartbind(
    fg.batter,
    tmp
  )
  cat(i, nrow(fg.batter), '\n')
}

## get all fg pitcher leaderboard data from 2023 to 2000
fg.pitcher <- data.frame(baseballr::fg_pitcher_leaders(startseason = current.year, endseason = current.year, qual = 0))
for(i in (current.year-1):1990) {
  tmp <- data.frame(baseballr::fg_pitcher_leaders(startseason = i, endseason = i))
  fg.pitcher <- gtools::smartbind(
    fg.pitcher,
    tmp
  )
  cat(i, nrow(fg.pitcher), '\n')
}

fg.field <- data.frame(baseballr::fg_fielder_leaders(startseason = current.year, endseason = current.year, qual = 0))
for(i in (current.year-1):1990) {
  tmp <- data.frame(baseballr::fg_fielder_leaders(startseason = i, endseason = i))
  fg.field <- gtools::smartbind(
    fg.field,
    tmp
  )
  cat(i, nrow(fg.field), '\n')
}
rm(tmp); rm(i)

## generate aggregate WAR table
fg.war <- aggregate(fg.batter$WAR, by = list(fg.batter$xMLBAMID), FUN = 'sum')
names(fg.war) <- c("xMLBAMID", "career.WAR")

## to align draft data and other datasets, we need a constant identifier.  Fortunately we are using MLB draft data
## which has a "person_id" column representing the MLBID for that specific person
## aslo, fangraphs has kindly listed the MLB ID for their data, and baseballr has returned it
## in the fg datasets, this is listed as "xMLBAMID"

draft.war <- merge(draft, fg.war, by.x = 'person_id', by.y = 'xMLBAMID', all.x = TRUE, all.y = FALSE)

## lets check to make sure this worked
draft.war[grep("Barry Lamar Bonds", draft.war$person_full_fml_name),]

## we can now start looking at the relationship between draft position and career WAR (for hitters)
plot(draft.war$pick_number, draft.war$career.WAR, bty = "L", pch = 19)

## to save a version of this plot, you can 'export' in the Rstudio plot window, 
## or write out a file.  I generally do it this way as you have more control
## note that this will save to your 'working directory' - run this line of code: getwd()
jpeg(file = paste0("draft.position.vs.WAR.jpg"), width = 1200, height = 900, res = 300)
plot(draft.war$pick_number, draft.war$career.WAR, bty = "L", pch = 19)
dev.off()
getwd()  ## by default, this is saved in the working directory, which you can find by the getwd() command

## to subset the table, selecting only, for example the first 500 picks
draft.war.sub <- draft.war[which(draft.war$pick_number <= 500),]
## and we can make a boxplot based on pick 'bins' by taking the ceiling of the pick number over the bin size
boxplot(career.WAR~ceiling(pick_number/50), data = draft.war.sub)


## STOP