# crawling
library(tidyverse)
library(readr)
library(stringr)
library(rvest)
library(lubridate)

googlesheets4::gs4_auth(email = "ruminoreticulum@gmail.com")

dd <- rvest::read_html("https://www.ekapepia.com/priceStat/distrPriceBeef.do") |>
  html_table() %>%
  .[[1]] |>
  janitor::clean_names(ascii = FALSE) |>
  dplyr::slice(-c(1:2))

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


# Visulization

df <- df_final %>%
  select(date, 암송아지, 숫송아지, 지육_평균) %>%
  mutate(
    암송아지 = 암송아지/1000,
    숫송아지 = 숫송아지/1000
  )


names(df) <- c("date", "암송아지", "수송아지", "평균")

df1 <- df %>%
  mutate(암송아지 = str_replace_all(암송아지, ",", "")) %>%
  separate(
    암송아지, into = c("암송아지", "등락")
  ) %>%
  mutate(수송아지 = str_replace_all(수송아지, ",", "")) %>%
  separate(
    수송아지, into = c("수송아지", "등락")
  ) %>%
  mutate(평균 = str_replace_all(평균, ",", "")) %>%
  separate(
    평균, into = c("평균", "등락")
  ) %>%
  select(date, 암송아지, 수송아지, 평균) %>%
  mutate_if(
    is.character,
    as.numeric
  )

# add year & week
df1 <- df1 %>%
  mutate(
    year = year(date),
    week = isoweek(date)
  )


df_by_week_지육가 <- df1 %>%
  group_by(year, week) %>%
  arrange(평균) %>% slice(-1) %>% ungroup() %>% group_by(year, week) %>%
  summarise(
    암송아지 = mean(암송아지, na.rm = TRUE),
    수송아지 = mean(수송아지, na.rm = TRUE),
    평균지육가 = mean(평균, na.rm = TRUE)
  ) %>%
  mutate(
    암송아지 = 암송아지 / 10,
    수송아지 = 수송아지 / 10,
    yearweek = parse_number(paste0(year, week))
  ) %>%
  arrange(desc(year), desc(week)) %>%
  ungroup()

df_by_week <- df1 %>%
  group_by(year, week) %>%
  summarise(
    암송아지 = mean(암송아지, na.rm = TRUE),
    수송아지 = mean(수송아지, na.rm = TRUE),
    평균지육가 = mean(평균, na.rm = TRUE)
  ) %>%
  mutate(
    암송아지 = 암송아지 / 10,
    수송아지 = 수송아지 / 10,
    yearweek = parse_number(paste0(year, week))
  ) %>%
  arrange(desc(year), desc(week)) %>%
  ungroup()



df_by_week <- df_by_week %>% filter(week != 53)
df_by_week_지육가 <- df_by_week_지육가 %>% filter(week != 53)

df_2021 <- df_by_week %>%
  # filter(year == 2021) %>%
  slice(1:8) %>%
  gather(
    암송아지, 수송아지,
    key = "성별",
    value = "가격"
  ) %>%
  mutate(
    가격 = ceiling(가격)
    #ceiling은 x 보다 크거나 같은 정수
  )

df_2020 <- df_by_week %>%
  # filter(year == 2020) %>%
  slice((52-4):(52+8)) %>%
  gather(
    암송아지, 수송아지,
    key = "성별",
    value = "가격"
  ) %>%
  mutate(
    가격 = ceiling(가격)
  )


df_2021 <- df_2021 %>%
  group_by(성별) %>%
  arrange(year, week) %>%
  mutate(
    week1 = row_number()
  ) %>%
  ungroup()


df_2020 <- df_2020 %>%
  group_by(성별) %>%
  arrange(year, week) %>%
  mutate(
    week1 = row_number()
  ) %>%
  ungroup()

fontsize <- 3
linesize <- 0.35
pointsize <- 0.85

p1 <- ggplot(aes(week1, 가격), data = filter(df_2020, !is.na(가격))) +
  geom_point(aes(group = 성별), color = "grey", size = pointsize) +
  geom_line(aes(group = 성별), color = "grey", size = linesize) +
  ggrepel::geom_text_repel(
    aes(label = 가격), color = "grey", size = fontsize, family = "NotoSansKR-Regular",
    data = df_2020 %>% filter(week1 <= max(week1) & week1 > max(week1) - 6),
    force = 5, max.iter = 100000, direction = "y", segment.size = 0.2
  ) +
  # geom_text(aes(label = 가격), vjust = 1.5) +
  geom_point(aes(week1, 가격, color = 성별), size = pointsize, data = df_2021) +
  geom_line(aes(week1, 가격, color = 성별), data = df_2021, size = linesize) +
  # geom_point(aes(week, 가격, group = 성별), shape = 1, size = 5, data = df_2021 %>% filter(week == max(week))) +
  # ggrepel::geom_label_repel(aes(label = as.character(today()), group = 성별), size = 2.5, hjust = -0.5, vjust = 1, data = df_2021 %>% filter(week == max(week))) +
  # ggforce::geom_mark_circle(aes(fill = 가격, group = 성별, label = "2020-09-09", filter = week == max(week)), label.fill = "grey", data = df_2021) +
  scale_colour_manual(values = c("#3981C3", "#E05451")) +
  ggrepel::geom_text_repel(
    aes(label = 가격, color = 성별), data = df_2021, size = fontsize,
    show.legend = FALSE, family = "NotoSansKR-Regular",
    force = 5, max.iter = 100000, direction = "y", segment.size = 0.2
  ) +
  scale_x_continuous(
    breaks = min(df_2020$week1, na.rm = TRUE):max(df_2020$week1, na.rm = TRUE),
    labels = df_2020 %>% arrange(desc(year), desc(week)) %>% pull(week) %>% unique() %>% rev()
  ) +
  labs(
    title = "",
    # subtitle = "회색선: 작년 동일 주차",
    caption = "단위: 만원",
    xlab = "주차"
    # ylab = "가격(만원)"
  ) +
  ggthemes::theme_fivethirtyeight(base_family = "NotoSansKR-Regular") +
  theme(
    # rect = element_rect(fill = "transparent"),
    plot.title = element_text(size = 14*0.8, color = "grey60", face = "bold"),
    plot.subtitle = element_text(size = 10*0.8, color = "grey60"),
    # plot.caption = element_text(size = 10),
    text = element_text(size = 8, color = "grey60"),
    plot.background = element_rect(fill = "transparent"),
    legend.background = element_rect(fill = "transparent"),
    axis.text.y = element_blank(), #y축 안보이게
    legend.position = c(0.8, 1.1),
    legend.key = element_rect(fill = "transparent"),
    panel.background = element_rect(fill = "transparent"),
    panel.grid.major = element_line(size = 0.1, color = "grey"),
    panel.grid.minor = element_line(size = 0.05, color = "grey")
    # legend.position = "top"
  )

ggsave(
  filename = paste0("figure/", Sys.Date(), "-송아지.png"),
  plot = p1,
  dpi = 1000,
  # scale = 1,
  width = 5,
  height = 3,
  bg = "transparent"
)

## 지육가

beef_2021 <- df_by_week_지육가 %>%
  # filter(year == 2021) %>%
  slice(1:8)

min(df_2021$week)

beef_2020 <- df_by_week %>%
  # filter(year == 2020) %>%
  slice((52-4):(52+8))


beef_2021 <- beef_2021 %>%
  arrange(year, week) %>%
  mutate(
    week1 = row_number(),
    가격 = 평균지육가 %>% ceiling()
  )


beef_2020 <- beef_2020 %>%
  arrange(year, week) %>%
  mutate(
    week1 = row_number(),
    가격 = 평균지육가 %>% ceiling()
  )



fontsize <- 3
linesize <- 0.35
pointsize <- 0.85


p2 <- ggplot(aes(week1, 가격), data = beef_2020) +
  geom_point(color = "grey", size = pointsize) +
  geom_line(color = "grey", size = linesize) +
  ggrepel::geom_text_repel(
    aes(label = format(가격, big.mark = ",", scientific = FALSE)),
    color = "grey", size = fontsize, family = "NotoSansKR-Regular",
    data = beef_2020 %>% filter(week1 <= max(week1) & week1 > max(week1) - 6),
    force = 5, max.iter = 100000, direction = "y", segment.size = 0.2
  ) +
  # geom_text(aes(label = 가격), vjust = 1.5) +
  geom_point(aes(week1, 가격), data = beef_2021, color = "#F1AF12", size = pointsize) +
  geom_line(aes(week1, 가격), data = beef_2021, color = "#F1AF12", size = linesize) +
  ggrepel::geom_text_repel(
    aes(label = format(가격, big.mark = ",", scientific = FALSE)),
    color = "#F1AF12",
    data = beef_2021, size = fontsize,
    family = "NotoSansKR-Regular",
    force = 5, max.iter = 100000, direction = "y", segment.size = 0.2
  ) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE)) +
  scale_x_continuous(
    breaks = min(beef_2020$week1, na.rm = TRUE):max(beef_2020$week1, na.rm = TRUE),
    labels = beef_2020 %>% arrange(desc(year), desc(week)) %>% pull(week) %>% unique() %>% rev()
  ) +
  labs(
    # title = "주차별 지육 가격 변동",
    # subtitle = "회색선: 작년 동일 주차",
    caption = "단위: 원",
    xlab = "주차"
    # ylab = "가격(원)"
  ) +
  ggthemes::theme_fivethirtyeight(base_family = "NotoSansKR-Regular") +
  theme(
    plot.title = element_text(size = 14*0.8, color = "grey60", face = "bold"),
    plot.subtitle = element_text(size = 10*0.8, color = "grey60"),
    text = element_text(size = 8, color = "grey60"),
    plot.background = element_rect(fill = "transparent"),
    legend.background = element_rect(fill = "transparent"),
    axis.text.y = element_blank(), #y축 안보이게
    legend.position = c(0.8, 1.1),
    legend.key = element_rect(fill = "transparent"),
    panel.background = element_rect(fill = "transparent"),
    panel.grid.major = element_line(size = 0.1, color = "grey"),
    panel.grid.minor = element_line(size = 0.05, color = "grey")
    # legend.position = "top"
  )


ggsave(
  filename = paste0("figure/", Sys.Date(), "-지육.png"),
  plot = p2,
  dpi = 1000,
  # scale = 1,
  width = 5,
  height = 3*0.9,
  bg = "transparent"
)

