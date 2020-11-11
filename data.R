# Loading packages =================================================
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(DT)
library(forcats)
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(readxl)
library(tibble)
library(reshape2)
library(openxlsx)
library(data.table)
library(zoo)
library(epitools)
library(here)
library(patchwork)


# Current week number ================================================================
current_week <- strftime(Sys.Date(), format="%V") %>% as.numeric()
previous_week <- current_week - 1
previous2_week <- current_week - 2
previous3_week <- current_week - 3


# Downloading file from NRS website ==================================================
# Generating link name as it changes each week
link <- paste0("https://www.nrscotland.gov.uk/files//statistics/covid19/covid-deaths-data-week-", "41")
link <- paste0(link, ".xlsx")
link2 <- paste0("https://www.nrscotland.gov.uk/files//statistics/covid19/covid-deaths-data-week-", "41")
link2 <- paste0(link2, ".xlsx")

# Downloading file from ONS website =================================================
# Generating link name
link_ons <- paste0("https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fweeklyprovisionalfiguresondeathsregisteredinenglandandwales%2f2020/publishedweek",
                   previous_week)
link_ons <- paste0(link_ons, "2020.xlsx")

link2_ons <- paste0("https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fweeklyprovisionalfiguresondeathsregisteredinenglandandwales%2f2020/publishedweek",
                    previous2_week)
link2_ons <- paste0(link2_ons, "2020.xlsx")

link3_ons <- paste0("https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fweeklyprovisionalfiguresondeathsregisteredinenglandandwales%2f2020/publishedweek",
                    previous3_week)
link3_ons <- paste0(link3_ons, "2020.xlsx")


# Download NRS link
download.file(link2, "nrs2.xlsx", mode="wb")
try(download.file(link, "nrs1.xlsx", mode="wb"))

# Download ONS link
try(download.file(link3_ons, "ons3.xlsx", mode="wb"))
try(download.file(link2_ons, "ons2.xlsx", mode="wb"))
try(download.file(link_ons, "ons1.xlsx", mode="wb"))

# Download NISRA link
try(download.file("https://www.nisra.gov.uk/sites/nisra.gov.uk/files/publications/Weekly_Deaths.xls", "nisra.xls", mode="wb",headers=NULL))

# Extracting data from xlsx =========================================================
{if (file.exists("nrs2.xlsx")){
  raw <- read.xlsx("nrs2.xlsx", sheet="Table 3 ")
  }
}

{if (file.exists("nrs1.xlsx")){
  raw <- read.xlsx("nrs1.xlsx", sheet="Table 3 ")
  }
}

# ONS data
{if (file.exists("ons3.xlsx")){
  raw_ons <- read.xlsx("ons3.xlsx", sheet="Weekly figures 2020")
  }
}
{if (file.exists("ons2.xlsx")){
  raw_ons <- read.xlsx("ons2.xlsx", sheet="Weekly figures 2020")
  }
}
{if (file.exists("ons1.xlsx")){
  raw_ons <- read.xlsx("ons1.xlsx", sheet="Weekly figures 2020")
  }
}
{if (file.exists("ons3.xlsx")){
  covid_ons <- read.xlsx("ons3.xlsx", sheet="Covid-19 - Weekly registrations")
  }
}
{if (file.exists("ons2.xlsx")){
  covid_ons <- read.xlsx("ons2.xlsx", sheet="Covid-19 - Weekly registrations")
  }
}
{if (file.exists("ons1.xlsx")){
  covid_ons <- read.xlsx("ons1.xlsx", sheet="Covid-19 - Weekly registrations")
  }
}

# NISRA data
nisra <- read_excel("nisra.xls", sheet="Table 1")
nisra <- as.data.frame(nisra)

## Organising NRS data ============================================================
raw <- as.data.frame(t(raw))
names(raw)[2:117] <- as.matrix(raw[2, ])[2:117]
raw <- raw[-c(1, 2), ]
raw[] <- lapply(raw, function(x) type.convert(as.character(x)))
raw <- raw[colSums(!is.na(raw)) > 0]
raw <- raw[colSums(!is.na(raw)) > 40]

# Naming columns
names(raw)[1:21] <- c("week",
                "expected_cancer",
                "expected_dementia",
                "expected_cardiovascular",
                "expected_respiratory",
                "expected_others",
                "expected",
                "cancer",
                "dementia",
                "cardiovascular",
                "respiratory",
                "covid",
                "others",
                "total",
                "difference_cancer",
                "difference_dementia",
                "difference_cardiovascular",
                "difference_respiratory",
                "difference_covid",
                "difference_others",
                "difference")

# Removing columns of location of death statistics
start_location_death <- which(colnames(raw)=="difference") + 1
number_column <- ncol(raw)
raw <- raw %>% select("week",
                      "expected_cancer",
                      "expected_dementia",
                      "expected_cardiovascular",
                      "expected_respiratory",
                      "expected_others",
                      "expected",
                      "cancer",
                      "dementia",
                      "cardiovascular",
                      "respiratory",
                      "covid",
                      "others",
                      "total",
                      "difference_cancer",
                      "difference_dementia",
                      "difference_cardiovascular",
                      "difference_respiratory",
                      "difference_covid",
                      "difference_others",
                      "difference")
# Removing last row
raw <- raw[-c(nrow(raw)), ]

# Calculating difference in percentages
raw$difference_cancer <- (raw$difference_cancer / raw$difference_cancer) * 100
raw$difference_dementia <- (raw$difference_dementia / raw$expected_dementia) * 100
raw$difference_cardiovascular <- (raw$difference_cardiovascular / raw$expected_cardiovascular) * 100
raw$difference_respiratory <- (raw$difference_respiratory / raw$expected_respiratory) * 100
raw$difference_others <- (raw$difference_others / raw$expected_others) * 100
raw$difference <- (raw$difference / raw$expected) * 100

# Calculating non-covid deaths
raw$non_covid <- raw$total - raw$covid

deaths <- select(raw, c("week",
                        "cancer",
                        "dementia",
                        "cardiovascular",
                        "respiratory",
                        "covid",
                        "others",
                        "total"))
rownames(deaths) <- NULL

deaths <- cbind(deaths, raw$non_covid)

names(deaths) <- c("week",
                   "Cancer",
                   "Dementia",
                   "Cardiovascular",
                   "Respiratory",
                   "Covid",
                   "Others",
                   "All",
                   "Non-COVID")

five_year_average <- select(raw, c("week",
                                   "expected_cancer",
                                   "expected_dementia",
                                   "expected_cardiovascular",
                                   "expected_respiratory",
                                   "expected_others",
                                   "expected"))
rownames(five_year_average) <- NULL
five_year_average <- cbind(five_year_average, five_year_average$expected)


names(five_year_average) <- c("week",
                              "Cancer",
                              "Dementia",
                              "Cardiovascular",
                              "Respiratory",
                              "Others",
                              "All",
                              "Non-COVID")


difference <- select(raw, c("week",
                            "difference_cancer",
                            "difference_dementia",
                            "difference_cardiovascular",
                            "difference_respiratory",
                            "difference_covid",
                            "difference_others",
                            "difference"))
rownames(difference) <- NULL
names(difference) <- c("week",
                       "Cancer",
                       "Dementia",
                       "Cardiovascular",
                       "Respiratory",
                       "Covid",
                       "Others",
                       "All")

nrs <- raw
rownames(nrs) <- NULL


## Organising ONS data ==================================================================
# Selecting columns with data
raw_ons <- t(raw_ons)
raw_ons <- as.data.frame(raw_ons)
raw_ons <- raw_ons %>% filter(is.na(raw_ons[5])==FALSE)

# Replacing column names and removing NA columns
names(raw_ons)[1:11] <- as.matrix(raw_ons[1, ])[1:11]
raw_ons <- raw_ons[-1, ]
raw_ons[] <- lapply(raw_ons, function(x) type.convert(as.character(x)))
raw_ons <- raw_ons[colSums(!is.na(raw_ons)) > 0]

# Renaming columns
raw_ons <- raw_ons[ , -2]
raw_ons <- raw_ons[ , -c(8:76)]
names(raw_ons) <- c("week",
                    "total",
                    "expected",
                    "expected_england",
                    "expected_wales",
                    "respiratory",
                    "covid",
                    "total_wales")

# Generate England deaths
raw_ons$total_england <- raw_ons$total - raw_ons$total_wales

# Organising covid sheet
covid_ons <- t(covid_ons)
covid_ons <- as.data.frame(covid_ons)
covid_ons <- covid_ons %>% filter(is.na(covid_ons[5])==FALSE)

# Replacing column names and removing NA columns
names(covid_ons)[1:5] <- as.matrix(covid_ons[1, ])[1:5]
covid_ons <- covid_ons[-1, ]
covid_ons[] <- lapply(covid_ons, function(x) type.convert(as.character(x)))
covid_ons <- covid_ons[colSums(!is.na(covid_ons)) > 0]


covid_ons <- covid_ons[ , -c(4:72)]
covid_ons <- covid_ons[ , -2]
names(covid_ons) <- c("week",
                      "covid_total",
                      "covid_wales")

# Add covid_wales into raw_ons
ons <- merge(raw_ons, covid_ons, by="week")
ons <- select(ons, -c("covid_total"))
ons$covid_england <- ons$covid - ons$covid_wales

# Calculating percentage difference
ons$difference <- ((ons$total - ons$expected) / ons$expected) * 100
ons$difference_wales <- ((ons$total_wales - ons$expected_wales) / ons$expected_wales) * 100
ons$difference_england <- ((ons$total_england - ons$expected_england) / ons$expected_england) * 100

# Calculating non-covid deaths
ons$non_covid <- ons$total - ons$covid
ons$non_covid_england <- ons$total_england - ons$covid_england
ons$non_covid_wales <- ons$total_wales - ons$covid_wales


## Orgnising Northern Ireland data ===========================================================
nisra <- nisra %>% filter(is.na(nisra[3])==FALSE)
names(nisra) <- as.matrix(nisra[1, ])
nisra[] <- lapply(nisra, function(x) type.convert(as.character(x)))
names(nisra) <- c("week",
                  "date",
                  "total",
                  "expected",
                  "range1",
                  "range2",
                  "respiratory",
                  "expected_respiratory",
                  "covid")
nisra <- apply(nisra, 2, as.numeric) %>% as.data.frame()
nisra <- nisra %>% filter(is.na(nisra[3])==FALSE)


# Selecting the relevant rows and columns of data
nisra <- select(nisra, -c("date", "range1", "range2"))
nisra$covid[is.na(nisra$covid)] <- 0

# Calculating difference percentage
nisra$difference <- ((nisra$total - nisra$expected) / nisra$expected) * 100

# Calculating non-covid death
nisra$non_covid <- nisra$total - nisra$covid


# Population data
population <- c("England", "Wales", "Scotland", "Northern Ireland", "England and Wales")
population <- as.data.frame(population)
population$pop19 <- c(56286961, 3152879, 5463300, 1893667, 56286961+3152879)
population$pop18 <- c(55977178, 3138631, 5438100, 1881641, 55977178+1881641)
population$pop17 <- c(55619400, 3125200, 5424800, 1870800, 55619400+1870800)
population$pop16 <- c(55268100, 3113200, 5404700, 1862100, 55268100+1862100)
population$pop15 <- c(54786300, 3099100, 5373000, 1851600, 54786300+1851600)
population$pop14 <- c(54316600, 3092000, 5347600, 1840500, 54316600+1840500)
names(population)[1] <- c("nation")


# Calculating 5-year average of population size
population$avg <- (population$pop18 + population$pop17 + population$pop16 + population$pop15 + population$pop14) / 5


# Calculating mortality per 100000 and CI
# England
x <- select(ons, contains("england")) %>% select(-contains("expected")) %>% select(-contains("difference"))
for (i in colnames(x)) {
  y <- binom.approx(x[, i], population[1,2], conf.level = 0.95)
  y$proportion <- y$proportion * 100000
  y$lower <- y$lower * 100000
  y$upper <- y$upper * 100000
  colnames(y) <- paste(i, c("_x", "px", "_std", "_lb", "_ub", "_ci"), sep="")
  ons <- cbind(ons, y[, 3:5])
}

# England expected
y <- binom.approx(ons$expected_england, population[1,8], conf.level = 0.95)
y$proportion <- y$proportion * 100000
y$lower <- y$lower * 100000
y$upper <- y$upper * 100000
colnames(y) <- paste("expected_england", c("_x", "px", "_std", "_lb", "_ub", "_ci"), sep="")
ons <- cbind(ons, y[, 3:5])

# Wales
x <- select(ons, contains("wales")) %>% select(-contains("expected")) %>% select(-contains("difference"))
for (i in colnames(x)) {
  y <- binom.approx(x[, i], population[2,2], conf.level = 0.95)
  y$proportion <- y$proportion * 100000
  y$lower <- y$lower * 100000
  y$upper <- y$upper * 100000
  colnames(y) <- paste(i, c("_x", "px", "_std", "_lb", "_ub", "_ci"), sep="")
  ons <- cbind(ons, y[, 3:5])
}

# Wales expected
y <- binom.approx(ons$expected_wales, population[2,8], conf.level = 0.95)
y$proportion <- y$proportion * 100000
y$lower <- y$lower * 100000
y$upper <- y$upper * 100000
colnames(y) <- paste("expected_wales", c("_x", "px", "_std", "_lb", "_ub", "_ci"), sep="")
ons <- cbind(ons, y[, 3:5])

# Scotland
x <- select(nrs, -contains("expected")) %>% select(-contains("difference")) %>% select(-contains("week"))
for (i in colnames(x)) {
  y <- binom.approx(x[, i], population[3,2], conf.level = 0.95)
  y$proportion <- y$proportion * 100000
  y$lower <- y$lower * 100000
  y$upper <- y$upper * 100000
  colnames(y) <- paste(i, c("_x", "px", "_std", "_lb", "_ub", "_ci"), sep="")
  nrs <- cbind(nrs, y[, 3:5])
}

# Scotland expected
x <- select(nrs, contains("expected")) %>% select(-contains("difference"))
for (i in colnames(x)) {
  y <- binom.approx(x[, i], population[3,8], conf.level = 0.95)
  y$proportion <- y$proportion * 100000
  y$lower <- y$lower * 100000
  y$upper <- y$upper * 100000
  colnames(y) <- paste(i, c("_x", "px", "_std", "_lb", "_ub", "_ci"), sep="")
  nrs <- cbind(nrs, y[, 3:5])
}

# Northern Ireland
x <- select(nisra, -contains("expected")) %>% select(-contains("difference"))
for (i in colnames(x)) {
  y <- binom.approx(x[, i], population[4,2], conf.level = 0.95)
  y$proportion <- y$proportion * 100000
  y$lower <- y$lower * 100000
  y$upper <- y$upper * 100000
  colnames(y) <- paste(i, c("_x", "px", "_std", "_lb", "_ub", "_ci"), sep="")
  nisra <- cbind(nisra, y[, 3:5])
}

# Northern Ireland expected
x <- select(nisra, contains("expected")) %>% select(-contains("difference"))
for (i in colnames(x)) {
  y <- binom.approx(x[, i], population[4,8], conf.level = 0.95)
  y$proportion <- y$proportion * 100000
  y$lower <- y$lower * 100000
  y$upper <- y$upper * 100000
  colnames(y) <- paste(i, c("_x", "px", "_std", "_lb", "_ub", "_ci"), sep="")
  nisra <- cbind(nisra, y[, 3:5])
}


# Calculating moving averages =================================================
# ONS England
x <- select(ons, contains("england")) %>% select(-contains("expected")) %>%
  select(-contains("difference")) %>% select(-contains("std")) %>%
  select(-contains("lb")) %>% select(-contains("ub"))
y <- rollapply(x, 4, "mean", align="left", by.column=TRUE) %>% as.data.frame()
for (i in colnames(y)) {
  z <- binom.approx(y[, i], population[1,2], conf.level = 0.95)
  z$proportion <- z$proportion * 100000
  z$lower <- z$lower * 100000
  z$upper <- z$upper * 100000
  colnames(z) <- paste(i, c("_x", "px", "_std_ma", "_std_ma_lb", "_std_ma_ub", "_ci"), sep="")
  n <- rep(as.numeric(NA), ncol(z))
  n <- rbind.data.frame(n, n, n)
  colnames(n) <- colnames(z)
  z <- rbind.data.frame(n, z)
  z <- apply(z, 2, as.numeric, by.column=TRUE)
  ons <- cbind(ons, z[, 3:5])
}

# ONS expected England
x <- select(ons, expected_england)
y <- rollapply(x, 4, "mean", align="left", by.column=TRUE) %>% as.data.frame()
for (i in colnames(y)) {
  z <- binom.approx(y[, i], population[1,8], conf.level = 0.95)
  z$proportion <- z$proportion * 100000
  z$lower <- z$lower * 100000
  z$upper <- z$upper * 100000
  colnames(z) <- paste(i, c("_x", "px", "_std_ma", "_std_ma_lb", "_std_ma_ub", "_ci"), sep="")
  n <- rep(as.numeric(NA), ncol(z))
  n <- rbind.data.frame(n, n, n)
  colnames(n) <- colnames(z)
  z <- rbind.data.frame(n, z)
  z <- apply(z, 2, as.numeric, by.column=TRUE)
  ons <- cbind(ons, z[, 3:5])
}


# ONS Wales
x <- select(ons, contains("wales")) %>% select(-contains("expected")) %>%
  select(-contains("difference")) %>% select(-contains("std")) %>%
  select(-contains("lb")) %>% select(-contains("ub"))
y <- rollapply(x, 4, "mean", align="left", by.column=TRUE) %>% as.data.frame()
for (i in colnames(y)) {
  z <- binom.approx(y[, i], population[2,2], conf.level = 0.95)
  z$proportion <- z$proportion * 100000
  z$lower <- z$lower * 100000
  z$upper <- z$upper * 100000
  colnames(z) <- paste(i, c("_x", "px", "_std_ma", "_std_ma_lb", "_std_ma_ub", "_ci"), sep="")
  n <- rep(as.numeric(NA), ncol(z))
  n <- rbind.data.frame(n, n, n)
  colnames(n) <- colnames(z)
  z <- rbind.data.frame(n, z)
  z <- apply(z, 2, as.numeric, by.column=TRUE)
  ons <- cbind(ons, z[, 3:5])
}

# ONS expected Wales
x <- select(ons, expected_wales)
y <- rollapply(x, 4, "mean", align="left", by.column=TRUE) %>% as.data.frame()
for (i in colnames(y)) {
  z <- binom.approx(y[, i], population[2,8], conf.level = 0.95)
  z$proportion <- z$proportion * 100000
  z$lower <- z$lower * 100000
  z$upper <- z$upper * 100000
  colnames(z) <- paste(i, c("_x", "px", "_std_ma", "_std_ma_lb", "_std_ma_ub", "_ci"), sep="")
  n <- rep(as.numeric(NA), ncol(z))
  n <- rbind.data.frame(n, n, n)
  colnames(n) <- colnames(z)
  z <- rbind.data.frame(n, z)
  z <- apply(z, 2, as.numeric, by.column=TRUE)
  ons <- cbind(ons, z[, 3:5])
}


# NISRA
x <- select(nisra, -contains("expected")) %>%
  select(-contains("difference")) %>% select(-contains("std")) %>%
  select(-contains("lb")) %>% select(-contains("ub"))
y <- rollapply(x, 4, "mean", align="left", by.column=TRUE) %>% as.data.frame()
for (i in colnames(y)) {
  z <- binom.approx(y[, i], population[4,2], conf.level = 0.95)
  z$proportion <- z$proportion * 100000
  z$lower <- z$lower * 100000
  z$upper <- z$upper * 100000
  colnames(z) <- paste(i, c("_x", "px", "_std_ma", "_std_ma_lb", "_std_ma_ub", "_ci"), sep="")
  n <- rep(as.numeric(NA), ncol(z))
  n <- rbind.data.frame(n, n, n)
  colnames(n) <- colnames(z)
  z <- rbind.data.frame(n, z)
  z <- apply(z, 2, as.numeric, by.column=TRUE)
  nisra <- cbind(nisra, z[, 3:5])
}

# NISRA expected
x <- select(nisra, expected)
y <- rollapply(x, 4, "mean", align="left", by.column=TRUE) %>% as.data.frame()
for (i in colnames(y)) {
  z <- binom.approx(y[, i], population[4,8], conf.level = 0.95)
  z$proportion <- z$proportion * 100000
  z$lower <- z$lower * 100000
  z$upper <- z$upper * 100000
  colnames(z) <- paste(i, c("_x", "px", "_std_ma", "_std_ma_lb", "_std_ma_ub", "_ci"), sep="")
  n <- rep(as.numeric(NA), ncol(z))
  n <- rbind.data.frame(n, n, n)
  colnames(n) <- colnames(z)
  z <- rbind.data.frame(n, z)
  z <- apply(z, 2, as.numeric, by.column=TRUE)
  nisra <- cbind(nisra, z[, 3:5])
}


# NRS
x <- select(nrs, -contains("expected")) %>%
  select(-contains("difference")) %>% select(-contains("std")) %>%
  select(-contains("lb")) %>% select(-contains("ub")) %>% select(-contains("week"))
y <- rollapply(x, 4, "mean", align="left", by.column=TRUE) %>% as.data.frame()
for (i in colnames(y)) {
  z <- binom.approx(y[, i], population[3,2], conf.level = 0.95)
  z$proportion <- z$proportion * 100000
  z$lower <- z$lower * 100000
  z$upper <- z$upper * 100000
  colnames(z) <- paste(i, c("_x", "px", "_std_ma", "_std_ma_lb", "_std_ma_ub", "_ci"), sep="")
  n <- rep(as.numeric(NA), ncol(z))
  n <- rbind.data.frame(n, n, n)
  colnames(n) <- colnames(z)
  z <- rbind.data.frame(n, z)
  z <- apply(z, 2, as.numeric, by.column=TRUE)
  nrs <- cbind(nrs, z[, 3:5])
}

# NRS expected
x <- select(nrs, contains("expected"))
y <- rollapply(x, 4, "mean", align="left", by.column=TRUE) %>% as.data.frame()
for (i in colnames(y)) {
  z <- binom.approx(y[, i], population[3,8], conf.level = 0.95)
  z$proportion <- z$proportion * 100000
  z$lower <- z$lower * 100000
  z$upper <- z$upper * 100000
  colnames(z) <- paste(i, c("_x", "px", "_std_ma", "_std_ma_lb", "_std_ma_ub", "_ci"), sep="")
  n <- rep(as.numeric(NA), ncol(z))
  n <- rbind.data.frame(n, n, n)
  colnames(n) <- colnames(z)
  z <- rbind.data.frame(n, z)
  z <- apply(z, 2, as.numeric, by.column=TRUE)
  nrs <- cbind(nrs, z[, 3:5])
}


# Tidying dates from week numbers
nrs$week <- as.Date(paste(2020, (nrs$week), 1, sep="-"), "%Y-%U-%u") - 1
deaths$week <- as.Date(paste(2020, (deaths$week), 1, sep="-"), "%Y-%U-%u") - 1
five_year_average$week <- as.Date(paste(2020, (five_year_average$week), 1, sep="-"), "%Y-%U-%u") - 1
ons$week <- as.Date(paste(2020, (ons$week-1), 6, sep="-"), "%Y-%U-%u") - 1
nisra$week <- as.Date(paste(2020, (nisra$week), 6, sep="-"), "%Y-%U-%u") -1

max_week_range = as.Date(max(nrs$week))

