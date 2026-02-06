#'---
#' title: MAIN MODEL ON CASE TIME SERIES DATA
#' author: ""
#' date: ""
#' output:
#'  github_document
#'---

#+ message=FALSE
pacman::p_load(
  rio,            # import and export files
  here,           # locate files 
  tidyverse,      # data management and visualization
  gnm,
  dlnm,
  splines
)

#' # Data
# data #-----------
#' ## MSOA data
## MSOA data #--------------------------------
(datafull_0 <- rio::import(here("note_lehuynh/data_full_temperature.csv")) %>% 
  tibble())

#' Define the strata
(datafull_1 <- datafull_0 %>% 
  mutate(across(c(MSOA11CD, year, month), 
                as.factor),
         stratum = factor(paste(MSOA11CD, year, month, sep = ":"))) %>% 
  select(MSOA11CD,
         date,
         doy,
         dow,
         stratum,
         d074,
         d75plus,
         dtot,
         tmean,
         year,
         month,
         day))

#' Generate `ind`: sum of events within stratum
(ind_df <- datafull_1 %>% group_by(stratum) %>% summarise(ind = sum(dtot)))

(datafull <- datafull_1 %>% left_join(ind_df, by = join_by(stratum)))

#' ## Aggregated data
## aggregated data #-------------------------------
(dataggr <- rio::import(here("note_lehuynh/data_aggregated_temperature.csv")) %>% 
  tibble())

#' # Model fitting
# model fitting #-----------
#' ## MSOA data
## MSOA data #--------------------------------
# DEFINE SPLINES OF DAY OF THE YEAR
spldoy <- onebasis(datafull$doy, "ns", df=3)
summary(spldoy)

# DEFINE THE CROSS-BASIS FOR TEMPERATURE FROM THE EXPOSURE HISTORY MATRIX
# NB: USE group TO IDENTIFY LACK OF CONTINUITY IN SERIES BY MSOA AND YEAR
argvar <- list(fun = "ns", knots = quantile(datafull$tmean, c(50, 90) / 100, na.rm = T))
arglag <- list(fun = "ns", knots = 1)

# group = factor(MSOA-year)
group <- factor(paste(datafull$MSOA11CD, datafull$year, sep = "-"))
length(group)
head(group)

cbtmean <- crossbasis(datafull$tmean,
                      lag = 3, 
                      argvar = argvar, 
                      arglag = arglag,
                      group = group)

summary(cbtmean)

# model fitting
modfull <- gnm(dtot ~ cbtmean + spldoy:factor(year) + factor(dow), 
               eliminate = stratum, 
               data = datafull, 
               family = quasipoisson, 
               subset = ind > 0)

summary(modfull)

#' ## Aggregated data
## aggregated data #-------------------------------
# RE-DEFINE THE CROSS-BASIS WITH THE SAME PARAMETRISATION
# NB: CAN USE SERIES DIRECTLY INSTEAD THAN MATRIX, BUT USE group FOR YEARS
cbtmeanaggr <- crossbasis(dataggr$tmean,
                          lag = 3, 
                          argvar = argvar, 
                          arglag = arglag,
                          group = dataggr$year)
summary(cbtmeanaggr)

# RUN THE MODEL ON AGGREGATED DATA
modaggr <- glm(dtot ~ cbtmeanaggr + ns(doy, df = 3):factor(year) + factor(dow),
               data = dataggr, 
               family = quasipoisson)

summary(modaggr)

#' ## PREDICT AND PLOT
## predict and plot #------------------------
# PREDICT
cpfull <- crosspred(cbtmean, modfull, cen = 16)
cpaggr <- crosspred(cbtmeanaggr, modaggr, cen = 16)

# PLOT
col <- c("darkgoldenrod3", "aquamarine3")
parold <- par(no.readonly = T)

par(mar = c(4, 4, 1, 0.5), 
    las = 1, 
    mgp = c(2.5, 1, 0))

plot(cpfull, "overall",
  ylim = c(0.8, 1.8), 
  ylab = "RR", 
  col = col[1], 
  lwd = 1.5,
  xlab = expression(paste("Temperature (" * degree, "C)")),
  ci.arg = list(col = alpha(col[1], 0.2)))

lines(cpaggr, "overall",
  ci = "area", 
  col = col[2], 
  lwd = 1.5,
  ci.arg = list(col = alpha(col[2], 0.2)))

legend("top", c("Case TS", "Aggregated TS"),
  lty = 1, 
  lwd = 1.5, 
  col = col, 
  bty = "n",
  inset = 0.05, 
  y.intersp = 2, 
  cex = 0.8)

par(parold)

#' # INTERACTION MODELS
# interaction models #----------------------------
#' ## Data
## data #-----------
(df_imd <- rio::import(here("note_lehuynh/data_full_imd.csv")) %>% 
   tibble() %>% 
   select(MSOA11CD,
          imdscore,
          imdrank) %>% 
   distinct())

df_imd %>% 
  count(MSOA11CD, imdscore, imdrank) %>% 
  arrange(MSOA11CD) %>% 
  print(n = 20)

(datafull_int <- datafull %>% left_join(df_imd, by = join_by(MSOA11CD)))

#' ## Model fitting
## model fitting #-----------
#' DEFINE INTERACTION CROSS-BASES WITH LINEAR IMD SCORE
(intval <- quantile(datafull_int$imdscore, c(0.25, 0.75)))
cbint1 <- cbtmean * (datafull_int$imdscore - intval[1])
cbint2 <- cbtmean * (datafull_int$imdscore - intval[2])

#' RUN THE MODELS
modint1 <- gnm(formula = dtot ~ cbtmean + spldoy:factor(year) + factor(dow) + cbint1, 
               eliminate = stratum, 
               family = quasipoisson, 
               data = datafull_int,
               subset = ind > 0)
summary(modint1)

modint2 <- gnm(formula = dtot ~ cbtmean + spldoy:factor(year) + factor(dow) + cbint2,
               eliminate = stratum,
               family = quasipoisson,
               data = datafull_int,
               subset = ind > 0)
summary(modint2)

#' TEST SIGNIFICANCE OF INTERACTION
anova(modfull, modint1, test="Chisq")
anova(modfull, modint2, test="Chisq")

#' ## PREDICT FOR EACH OF THE TWO IMD VALUES
## predict and plot #-------------------
cpint1 <- crosspred(cbtmean, modint1, cen = 16)
cpint2 <- crosspred(cbtmean, modint2, cen = 16)

#' ## PLOT
col <- c("royalblue", "tomato3")
parold <- par(no.readonly = T)
par(mar = c(4, 4, 1, 0.5), las = 1, mgp = c(2.5, 1, 0))

plot(cpint1, 
     "overall",
     ylim = c(0.8, 2), 
     ylab = "RR", 
     xlab = expression(paste("Temperature (" * degree, "C)")),
     col = col[1], 
     lwd = 1.5,
     ci.arg = list(col = alpha(col[1], 0.2)))

lines(cpint2, 
      "overall",
      ci = "area", 
      col = col[2], 
      lwd = 1.5,
      ci.arg = list(col = alpha(col[2], 0.2)))

legend("top", 
       c("Low IMD score", "High IMD score"),
       lty = 1, 
       lwd = 1.5, 
       col = col,
       bty = "n", 
       inset = 0.05, 
       y.intersp = 2, 
       cex = 0.8)

par(parold)

