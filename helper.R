##### Libraries #####
library(tidyverse)
library(ggplot2)
library(readxl)
library(zoo)
library(glue)
library(rstan)

dir_dat <- "../01-data/"

#### Helper data ####

tb_km <- read_csv(paste0(dir_dat,"data-products/notifications.csv")) %>%
  mutate(date = zoo::as.yearqtr(date)) %>%
  group_by(date) %>%
  summarize(n = sum(n), .groups = 'drop')

pop_km <- read_csv(paste0(dir_dat,"data-products/population-karamoja.csv"))%>%
  mutate(date = zoo::as.yearqtr(date))

### Number of future years
# future <- 4
future <- 10*4
Q <- nrow(tb_km)
N <- Q + future ## in helper

## Scenarios
fi_start <- c(2,2, 3, 4,2,2,2,2,3,4)
fi_end   <- c(8,4,8,12,8,8,8,4,8,12)
fi_bmi   <- c(0,1, 1.5,2,0,0,0,1,1.5,2)
fi_es    <- c(0,0.1, 0.15, 0.2,0,0,0,.1,.15,.2)

ha_es    <- c(1,1,1,1,0.9, 0.8, 0.7,.9,.8,.7)

S <- length(fi_start)


## Quarters
qs <- data.frame("quarter" = c(1,2,3,4), "months" = c("Jan.to.Mar",
                                                      "Apr.to.Jun",
                                                      "Jul.to.Sep",
                                                      "Oct.to.Dec"),
                 "months2" = c("Jan to Mar",
                               "Apr to Jun",
                               "Jul to Sep",
                               "Oct to Dec"))

districts <- c("Abim",
               "Amudat",
               "Kaabong",
               "Kotido",
               "Napak",
               "Nakapiripirit",
               "Nabilatuk",
               "Moroto",
               "Karenga")

subregions <- data.frame(region = districts,
                         topdistrict = districts) %>%
  mutate(topdistrict = case_when(region == "Napak" ~ "Moroto",
                                 region == "Nabilatuk" ~ "Nakapiripirit",
                                 region == "Karenga" ~ "Kaabong",
                                 region == "Amudat" ~ "Nakapiripirit",
                                 TRUE ~ region))

#### REscaling to quarterly

y2q <- function(r, t= 4){
  (1+r)^(1/t) -1
}

y2qFull <- function(r, t){
  r_sum = sum(r)
  r_scale = r/r_sum
  q_sum = y2q(r_sum, t)
  return(r_scale * q_sum)
}
