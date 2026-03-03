# crawling
library(tidyverse)
library(readr)
library(stringr)
library(rvest)
library(lubridate)

googlesheets4::gs4_auth(email = "ruminoreticulum@gmail.com")

dd <- rvest::read_html("https://www.ekapepia.com/supPrice/liveStock/type/cowNew.do?menuSn=86&boardInfoNo=&bbsSn=&userGroupType=40&paramSearchTxt=&pstSn=0") %>%
  html_table() %>%
  .[[1]] %>%
  janitor::clean_names(ascii = FALSE) |>
  dplyr::slice(-c(1:2))


# dd <- rvest::read_html("https://www.ekapepia.com/priceStat/distrPriceBeef.do") |>
#   html_table() %>%
#   .[[1]] |>
#   janitor::clean_names(ascii = FALSE) |>
#   dplyr::slice(-c(1:2))

names(dd) <- c("date",	"암송아지",	"숫송아지",	"농가수취가격_600kg",	"지육_평균",	"지육_1등급",	"도매_등심1등급",	"소비자_등심1등급")

dd1 <- dd %>%
  mutate_at(
    vars(암송아지:소비자_등심1등급), function(x) {parse_number(x)}
  ) %>%
  mutate_at(
    vars(암송아지:농가수취가격_600kg),
    function(x) x*1000
  ) %>%
  mutate(명절 = NA) %>%
  select(date, 명절, everything())

# read & write google sheet

library(hanwoo)
library(googlesheets4)

df <- read_sheet("https://docs.google.com/spreadsheets/d/1baViBjPUz1wyicG-I-vN5RhyoCcFt-6lw550evTowEg/edit#gid=1181990123", sheet = 1)

df <- df %>% select(-wday) %>% unique() %>% mutate(wday = wday(date, label = TRUE)) %>% select(date:week, wday, 비고)

df1 <- dd1 %>%
  mutate(date = ymd(date),
         year = year(date),
         week = isoweek(date),
         wday = wday(date, label = TRUE)
         )

df2 <- df %>%
  mutate(date = ymd(date)) %>%
  filter(date %in% df1$date == FALSE)

df_final <- bind_rows(df1, df2)
# janitor::get_dupes(df_final %>% select(date))

janitor::get_dupes(df_final)

glimpse(df_final)

write_sheet(data = df_final, "https://docs.google.com/spreadsheets/d/1baViBjPUz1wyicG-I-vN5RhyoCcFt-6lw550evTowEg/edit#gid=1181990123", sheet = 1)

rio::export(df_final, file = paste0("data_hanwoo_stock/hanwoo-stock-", today(),".xlsx"))



# show result
test_result <- hanwoo::hanwoo_stock() %>%
  mutate(
    rank_지육 = row_number(desc(지육_평균)),
    rank_숫송아지 = row_number(desc(숫송아지))
  ) %>%
  select(rank_지육,rank_숫송아지,everything())

print(test_result %>% slice(1:20))

glimpse(test_result)


# remotes::install_github("cheuerde/plotcli")

# library(plotcli)
# p <- test_result %>%
#   slice(1:90) %>%
#   mutate(order = -row_number()) %>%
#   mutate(price = 지육_평균) %>%
#   filter(!is.na(지육_평균)) %>%
#   filter(wday != "Mon") %>%
#   ggplot(aes(x = order, y = price)) +
#   geom_line()
#
#
# plotcli::ggplotcli(p, braille = FALSE)
