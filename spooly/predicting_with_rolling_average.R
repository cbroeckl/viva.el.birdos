library(baseballr)
library(tidyverse)

## setwd("~/Documents/veb/viva.el.birdos/cdb/4wkRollingPerformance")

## NOTE: it looks like fangraphs 2025 wrc+ calculation won't be finalized until after
##       the world series sometime, maybe december. So the 2025 numbers that I am trying to
##       predict bounce around from day to day, which changes my results from day to day.

##############################################################################
## code stolen from cdb to grab and pre-process data from baseballr,
##  with some modification (thanks!)
##############################################################################
getAllMondays = function(year) {
  days = as.POSIXlt(paste(year, 1:366, sep="-"), format="%Y-%j")
  Ms = days[days$wday==1]
  Ms[!is.na(Ms)]  # Needed to remove NA from day 366 in non-leap years
  as.character(Ms)
}
mondays = getAllMondays(2024)
getAllSundays = function(year) {
  days = as.POSIXlt(paste(year, 1:366, sep="-"), format="%Y-%j")
  Ms = days[days$wday==0]
  Ms[!is.na(Ms)]  # Needed to remove NA from day 366 in non-leap years
  as.character(Ms)
}
sundays = getAllSundays(2024)
sundays
mondays
opening.day = 2024-03-28
closing.day = 2024-09-29

first.monday = 13
first.sunday = 13
last.monday = 39
last.sunday = 39


batters.2024 = fg_batter_leaders(startseason = '2024', endseason='2024', qual = 'y')
batters.2025 = fg_batter_leaders(startseason = '2025', endseason='2025', qual = 'y')

## get 4 week rolling averages
weeks = last.monday - first.monday - 3
for(i in 1:weeks) {
    print(i)
    tmp = fg_batter_leaders(startdate = mondays[first.monday + i - 1],
                            enddate = sundays[first.sunday + i +3],
                            qual = 'y', month = '1000')
    if(i == 1) {
        out = cbind(
            week = rep(i, nrow(tmp)),
            tmp
        )
    } else {
        tmp = cbind(
            week = rep(i, nrow(tmp)),
            tmp
        )
        out = rbind(out, tmp, fill = TRUE)
    }
}
## only take periods with at least 20 PAs
batters.2024.wk4 = out %>% filter(PA >= 20)

## every player's rolling 4 week wrc+ including only periods with at least 20 at bats
ggplot(aes(x=week, y=wRC_plus, group=playerid, alpha=I(0.2)), data=batters.2024.wk4) + geom_line()


## summarize the 4 rolling week averages data
batters.2024.wk4.summary = batters.2024.wk4 %>%
    group_by(PlayerName, playerid) %>%           ## group by player
    summarise(n = n(),                           ## number of qualifying periods per player
              mean = mean(wRC_plus),             ## mean 4 week rolling wrc+
              sd = sd(wRC_plus),                 ## sd of 4 week rolling wrc+
              min = min(wRC_plus),
              max = max(wRC_plus),
              as_tibble_row(quantile(wRC_plus, probs=seq(0.1, 0.9, by=0.1)),
                            ## deciles of 4 week rolling wrc+
                            .name_repair = \(x) paste0('q', parse_number(x)))) %>%
    ungroup() %>%
    filter(n >= 10)                              ## only include players with 10+ 4 week periods

## select only the columns we care about for 2024 full season data
batters.2024.wrc = batters.2024 %>%
    mutate(wRC_plus_2024 = wRC_plus) %>%
    select(PlayerName, playerid, wRC_plus_2024)

## select only the columns we care about for 2025 full season data
batters.2025.wrc = batters.2025 %>%
    mutate(wRC_plus_2025 = wRC_plus) %>%
    select(PlayerName, playerid, wRC_plus_2025)

## combine datasets
##  use inner joins because we only want players who are in all of the datasets
batters.combined = batters.2024.wk4.summary %>%
    inner_join(batters.2024.wrc, by=c('PlayerName', 'playerid')) %>%
    inner_join(batters.2025.wrc, by=c('PlayerName', 'playerid'))

## basic model: predict future wrc+ with past wrc+:
o = lm(wRC_plus_2025 ~ wRC_plus_2024, data=batters.combined)
summary(o)

## add in mean and sd of wrc+
o.meansd = lm(wRC_plus_2025 ~ wRC_plus_2024 + mean + sd, data=batters.combined)
summary(o.meansd)
## note: none of the coefficients are significant

## just adding sd now
o.sd = lm(wRC_plus_2025 ~ wRC_plus_2024 + sd, data=batters.combined)
summary(o.sd)
## Call:
## lm(formula = wRC_plus_2025 ~ wRC_plus_2024 + sd, data = batters.combined)

## Residuals:
##     Min      1Q  Median      3Q     Max
## -41.555 -13.936  -0.537  10.924  61.389

## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)
## (Intercept)   44.98736   11.15956   4.031  0.00012 ***
## wRC_plus_2024  0.54396    0.08764   6.207  1.9e-08 ***
## sd             0.20103    0.23810   0.844  0.40087
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

## Residual standard error: 18.44 on 85 degrees of freedom
## Multiple R-squared:  0.3491,	Adjusted R-squared:  0.3338
## F-statistic: 22.79 on 2 and 85 DF,  p-value: 1.189e-08
## note: sd is significant: variation in rolling 4 week average wrc+ does seem
##       to predict future wrc+. But it's positively related

## add in all quantiles of rolling wrc+
o.quant = lm(wRC_plus_2025 ~ wRC_plus_2024 + q10 + q20 + q30 + q40 + q50 + q60 + q70 + q80 + q90,
             data=batters.combined)
summary(o.quant)
## note: note 10th percentile is significant. Negatively related with figure wrc+

## add in just the 10th and 90th percentiles
o.quant.10.90 = lm(wRC_plus_2025 ~ wRC_plus_2024 + q10 + q90,
                  data=batters.combined)
summary(o.quant.10.90)
## note: quantiles are not significant

## add in only 20th and 80th percentiles
o.quant.20.80 = lm(wRC_plus_2025 ~ wRC_plus_2024 + q20 + q80,
                  data=batters.combined)
summary(o.quant.20.80)
## note: quantiles not significant, and neither is wRC_plus_2024

## add in only 10th percentile
o.quant.10 = lm(wRC_plus_2025 ~ wRC_plus_2024 + q10,
                  data=batters.combined)
summary(o.quant.10)
## note: 10th percentile is not significant

## try min/max
o.min.max = lm(wRC_plus_2025 ~ wRC_plus_2024 + min + max,
                  data=batters.combined)
summary(o.min.max)
## note: min is significant, but negatively related with future wrc+

## just min
o.min = lm(wRC_plus_2025 ~ wRC_plus_2024 + min,
           data=batters.combined)
summary(o.min)
## note: min is significant, but negatively related with future wrc+

## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)
## (Intercept)    38.4316    10.8215   3.551 0.000628 ***
## wRC_plus_2024   0.8026     0.1292   6.211 1.87e-08 ***
## min            -0.2797     0.1173  -2.384 0.019362 *
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

## Residual standard error: 17.93 on 85 degrees of freedom
## Multiple R-squared:  0.3847,	Adjusted R-squared:  0.3703
## F-statistic: 26.58 on 2 and 85 DF,  p-value: 1.084e-09



#################
## try comparing all possible models using AIC/BIC
features = batters.combined %>%
    select(-PlayerName, - playerid, -n, -wRC_plus_2025)

## loop through all possible models, fit them, and collect aic/bic scores
## this will take awhile because we are fitting over 16,000 models
out = NULL
maxfeatures = ncol(features)
for(i in 1:maxfeatures){
    print(paste(i, " of ", maxfeatures, sep = ""))
    combos = combn(names(features), i)
    for(j in 1:ncol(combos)){
        cols = combos[,j]
        myformulastring = paste("wRC_plus_2025", "~", paste(cols, collapse = " + "))
        myformula = as.formula(myformulastring)
        o.my = lm(myformula, data= batters.combined)
        myaic = AIC(o.my)
        mybic = BIC(o.my)
        out = rbind(out, data.frame(model=myformulastring, aic=myaic, bic=mybic))
    }
}

modelscores = out %>%
    as_tibble()

## increase the number of significant digits printed for tibbles
options(pillar.sigfig = 7)

## using AIC we favor models that min along with 2024 wrc+, and possibly ohter variables like sd.
## (lower AIC is better)
modelscores %>% arrange(aic)

## using BIC we favor similar models, but with fewer variables. Doesn't like sd as much.
## (lower BIC is better)
## This is common: BIC tends to favor parsimony more than AIC does.
modelscores %>% arrange(bic)


## sd + min model
o.sd.min = lm(wRC_plus_2025 ~ wRC_plus_2024 + sd + min,
              data=batters.combined)
summary(o.sd.min)
## note: min is significant, but negatively related with future wrc+
##       sd is also negatively related to future wrc+

## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)
## (Intercept)    40.9637    10.8722   3.768 0.000305 ***
## wRC_plus_2024   1.0359     0.2011   5.152 1.68e-06 ***
## sd             -0.5394     0.3580  -1.507 0.135662
## min            -0.4893     0.1815  -2.697 0.008462 **
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

## Residual standard error: 17.8 on 84 degrees of freedom
## Multiple R-squared:  0.4009,	Adjusted R-squared:  0.3795
## F-statistic: 18.74 on 3 and 84 DF,  p-value: 2.142e-09


## What to make of this? Min and arguably SD help predict future wrc+.
## SD is positively related with future wrc+ in the model with only it and full 2024 season wrc+.
## Min is negatively related with future wrc+ in the model with only it and full 2024 season wrc+.
## Both of these might just be capturing a "he wasn't injured" signal.

## When min is high, then the player arguably wasn't playing through an injury. So the model
##  can't guarantee there won't be an injury next year, so it has to moderate predictions.
## But it can also increase the weight placed on full season wrc+.

## Similarly, when min is low, that tends to imply a higher SD of wrc+,
## and the same basic argument goes through.
## But SD can be high for other reasons (namely a larger max!) so it doesn't capture the injury
##  signal as well. Which explains why models with min tend to be favored over models with sd.

## But it is hard to interpret these coefficients because the covariates are highly correlated
## see e.g.
cor(features)
## so the above should be treated as speculation.
## I don't really know why min and sd are somewhat helpful in predicting future wrc+.
##  This is just a guess.
