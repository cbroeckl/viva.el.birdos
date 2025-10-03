d <- readxl::read_xlsx("team.offense.2025.xlsx")
head(d)
cor(d$R, d$Inn)
cor(d$R, d$PA)
cor(d$R, d$'wRC+')

library(ggplot2)
library(ggpmisc)
library(patchwork)

innings <- ggplot(d, aes(x = R, y = Inn)) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "R2"))) +
  geom_point()

plate.attempts <- ggplot(d, aes(x = R, y = PA)) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "R2"))) +
  geom_point()
  
wRC <- ggplot(d, aes(x = R, y = `wRC+`)) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "R2"))) +
  geom_point()

innings/plate.attempts/wRC

r.per.inning.model <- lm(d$R ~ d$Inn)
residual.runs <- residuals(r.per.inning.model)
plot(residual.runs, d$PA)
plot(residual.runs, d$`wRC+`)

runs.per.inning <- d$R/d$Inn
plot(runs.per.inning, d$PA)
plot(runs.per.inning, d$`wRC+`)

d2 <- data.frame(
  runs.per.inning = runs.per.inning, 
  PA = d$PA,
  `wRC+` = d$`wRC+`, 
  OBP = d$OBP,
  ISO = d$ISO,
  AVG = d$AVG,
  SLG = d$SLG,
  check.names = FALSE
)

plate.attempts <- ggplot(d2, aes(x = runs.per.inning, y = PA)) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "R2"))) +
  geom_point()

wRC <- ggplot(d2, aes(x = runs.per.inning, y = `wRC+`)) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "R2"))) +
  geom_point()

OBP <- ggplot(d2, aes(x = runs.per.inning, y = OBP)) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "R2"))) +
  geom_point()

AVG <- ggplot(d2, aes(x = runs.per.inning, y = AVG)) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "R2"))) +
  geom_point()

SLG <- ggplot(d2, aes(x = runs.per.inning, y = SLG)) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "R2"))) +
  geom_point()

ISO <- ggplot(d2, aes(x = runs.per.inning, y = ISO)) +
  stat_poly_line() +
  stat_poly_eq(use_label(c("eq", "R2"))) +
  geom_point()

plate.attempts/wRC/OBP/AVG/SLG/ISO
