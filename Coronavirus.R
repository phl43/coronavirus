library(tidyverse)
library(lubridate)

url_confirmed <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"
url_deaths <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv"
url_recovered <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv"

data_confirmed <- read_csv(url(url_confirmed)) %>%
  select(-c(`Province/State`, Lat, Long)) %>%
  rename(Country = `Country/Region`) %>%
  group_by(Country) %>%
  summarize_at(vars(-group_cols()), sum, na.rm = TRUE) %>%
  pivot_longer(-Country, names_to = "Date", values_to = "Confirmed")

data_confirmed$Confirmed[data_confirmed$Date == "3/12/20" & data_confirmed$Country == "Spain"] <- 3146
data_confirmed$Confirmed[data_confirmed$Date == "3/12/20" & data_confirmed$Country == "France"] <- 2876
data_confirmed$Confirmed[data_confirmed$Date == "3/12/20" & data_confirmed$Country == "Italy"] <- 15113
data_confirmed$Confirmed[data_confirmed$Date == "3/12/20" & data_confirmed$Country == "Japan"] <- 691

data_deaths <- read_csv(url(url_deaths)) %>%
  select(-c(`Province/State`, Lat, Long)) %>%
  rename(Country = `Country/Region`) %>%
  group_by(Country) %>%
  summarize_at(vars(-group_cols()), sum, na.rm = TRUE) %>%
  pivot_longer(-Country, names_to = "Date", values_to = "Deaths")

data_deaths$Deaths[data_deaths$Date == "3/12/20" & data_deaths$Country == "Spain"] <- 86
data_deaths$Deaths[data_deaths$Date == "3/12/20" & data_deaths$Country == "France"] <- 61
data_deaths$Deaths[data_deaths$Date == "3/12/20" & data_deaths$Country == "Italy"] <- 1016
data_deaths$Deaths[data_deaths$Date == "3/12/20" & data_deaths$Country == "Japan"] <- 19

data_recovered <- read_csv(url(url_recovered)) %>%
  select(-c(`Province/State`, Lat, Long)) %>%
  rename(Country = `Country/Region`) %>%
  group_by(Country) %>%
  summarize_at(vars(-group_cols()), sum, na.rm = TRUE) %>%
  pivot_longer(-Country, names_to = "Date", values_to = "Recovered")

data_recovered$Recovered[data_recovered$Date == "3/12/20" & data_recovered$Country == "Spain"] <- 189
data_recovered$Recovered[data_recovered$Date == "3/12/20" & data_recovered$Country == "France"] <- 12
data_recovered$Recovered[data_recovered$Date == "3/12/20" & data_recovered$Country == "Italy"] <- 1258
data_recovered$Recovered[data_recovered$Date == "3/12/20" & data_recovered$Country == "Japan"] <- 118

countries <- c(
  "France",
  "États-Unis",
  "Royaume-Uni",
  "Corée du Sud",
  "Japon",
  "Allemagne",
  "Espagne",
  "Italie",
  "Iran"
)

data_for_cfr <- inner_join(data_confirmed, data_deaths, by = c("Country", "Date")) %>%
  inner_join(data_recovered, by = c("Country", "Date")) %>%
  mutate(
    Date = mdy(Date),
    Country = case_when(
      Country == "Korea, South" ~ "Corée du Sud",
      Country == "US" ~ "États-Unis",
      Country == "United Kingdom" ~ "Royaume Uni",
      Country == "Japan" ~ "Japon",
      Country == "Germany" ~ "Allemagne",
      Country == "Spain" ~ "Espagne",
      Country == "Italy" ~ "Italie",
      TRUE ~ Country
    ),
    CFR = ifelse(Confirmed > 0, Deaths / Confirmed, 0)
  ) %>%
  filter(Country %in% countries & Date >= ymd("2020-02-28"))
  
ggplot(data_for_cfr, aes(x = Date, y = CFR, group = Country, color = Country)) +
  geom_line(size = 1) +
  theme_bw() +
  ggtitle("Évolution du taux de mortalité du coronavirus par pays") +
  xlab("Date") +
  ylab("Taux de mortalité") +
  scale_color_discrete(name = "Pays") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_date(
    labels = scales::date_format("%d/%m/%Y"),
    date_breaks = "5 day"
    ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(caption = "Source : Johns Hopkins University (https://github.com/CSSEGISandData/COVID-19) - Graphique de Philippe Lemoine (@phl43)") +
  ggsave("Évolution du taux de mortalité du coronavirus par pays.png", width = 12, height = 6)

url_italian_data <- "https://raw.githubusercontent.com/pcm-dpc/COVID-19/master/dati-regioni/dpc-covid19-ita-regioni.csv"

italian_data <- read_csv(url(url_italian_data)) %>%
  mutate(Date = ymd(floor_date(ymd_hms(data), unit = "day"))) %>%
  group_by(Date) %>%
  summarize(
    ICU = sum(terapia_intensiva) / sum(totale_attualmente_positivi)
  )

ggplot(italian_data, aes(x = Date, y = ICU)) +
  geom_line(size = 1, color = "steelblue") +
  theme_bw() +
  ggtitle("Évolution de la proportion des cas diagnostiqués de coronavirus toujours en cours admis en soins intensifs en Italie") +
  xlab("Date") +
  ylab("Proportion") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_x_date(
    labels = scales::date_format("%d/%m/%Y"),
    date_breaks = "5 day"
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(caption = "Source : Gouvernement italien (https://github.com/pcm-dpc/COVID-19) - Graphique de Philippe Lemoine (@phl43)") +
  ggsave("Évolution de la proportion des cas diagnostiqués de coronavirus toujours en cours admis en soins intensifs en Italie.png", width = 12, height = 6)

data_all <- inner_join(data_confirmed, data_deaths, by = c("Country", "Date")) %>%
  inner_join(data_recovered, by = c("Country", "Date")) %>%
  pivot_longer(-c(Country, Date), names_to = "Type", values_to = "Number") %>%
  mutate(
    Date = mdy(Date),
    Type = factor(Type)
  )

data_france <- data_all %>%
  filter(Country == "France")

cfr_france <- round(data_france$Number[data_france$Type == "Deaths" & data_france$Date == max(data_france$Date)] / data_france$Number[data_france$Type == "Confirmed" & data_france$Date == max(data_france$Date)] * 100, 1)

ggplot(data_france, aes(x = Date, y = Number, group = Type, color = Type)) +
  geom_line(size = 1) +
  theme_bw() +
  ggtitle("Le coronavirus en France") +
  xlab("Date") +
  ylab("Nombre") +
  annotate(
    geom = "text",
    label = paste0("Taux de mortalité = ", as.character(cfr_france), "%"),
    x = min(data_france$Date),
    y = 0.985 * max(data_france$Number),
    hjust = 0,
    vjust = 1
    ) +
  scale_color_discrete(
    name = "Catégorie",
    labels = c(
      "Infections",
      "Décès",
      "Guérisons"
      )
    ) +
  scale_x_date(
    labels = scales::date_format("%d/%m/%Y"),
    date_breaks = "5 day"
    ) +
  # scale_y_continuous(trans = "log10") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(caption = "Source : Johns Hopkins University (https://github.com/CSSEGISandData/COVID-19) - Graphique de Philippe Lemoine (@phl43)") +
  ggsave("Évolution du coronavirus en France.png", width = 12, height = 6)

data_france_italy <- data_all %>%
  filter(
    Country %in% c("France", "Italy"),
    Type == "Confirmed"
    ) %>%
  mutate(
    Country = ifelse(Country == "Italy", "Italie", Country)
  )

data_france_italy$Date[data_france_italy$Country == "Italie"] <- data_france_italy$Date[data_france_italy$Country == "Italie"] + 8

ggplot(data_france_italy, aes(x = Date, y = Number, group = Country, color = Country)) +
  geom_line(size = 1) +
  theme_bw() +
  ggtitle("Comparaison de la progression du coronavirus en France et en Italie par superposition des courbes en décalant la courbe italienne de 8 jours") +
  xlab("Date") +
  ylab("Nombre de cas diagnostiqués") +
  scale_color_discrete(
    name = "Pays",
    labels = c(
      "France",
      "Italie"
    )
  ) +
  scale_x_date(
    labels = scales::date_format("%d/%m/%Y"),
    date_breaks = "5 day"
  ) +
  # scale_y_continuous(trans = "log10") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(caption = "Source : Johns Hopkins University (https://github.com/CSSEGISandData/COVID-19) - Graphique de Philippe Lemoine (@phl43)") +
  ggsave("Comparaison de la progression du coronavirus en France et en Italie par superposition des courbes en décalant la courbe italienne de 8 jours.png", width = 12, height = 6)

data_taiwan <- data_all %>%
  filter(Country == "Taiwan*")

cfr_taiwan <- round(data_taiwan$Number[data_taiwan$Type == "Deaths" & data_taiwan$Date == max(data_taiwan$Date)] / data_taiwan$Number[data_taiwan$Type == "Confirmed" & data_taiwan$Date == max(data_taiwan$Date)] * 100, 1)

ggplot(data_taiwan, aes(x = Date, y = Number, group = Type, color = Type)) +
  geom_line(size = 1) +
  theme_bw() +
  ggtitle("Évolution du coronavirus à Taiwan") +
  xlab("Date") +
  ylab("Nombre") +
  annotate(
    geom = "text",
    label = paste0("Taux de mortalité = ", as.character(cfr_taiwan), "%"),
    x = min(data_taiwan$Date),
    y = 0.985 * max(data_taiwan$Number),
    hjust = 0,
    vjust = 1
  ) +
  scale_color_discrete(
    name = "Catégorie",
    labels = c(
      "Infections",
      "Décès",
      "Guérisons"
    )
  ) +
  scale_x_date(
    labels = scales::date_format("%m/%d/%Y"),
    date_breaks = "5 day"
  ) +
  # scale_y_continuous(trans = "log10") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(caption = "Source : Johns Hopkins University (https://github.com/CSSEGISandData/COVID-19) - Graphique de Philippe Lemoine (@phl43)") +
  ggsave("Évolution du coronavirus à Taiwan.png", width = 12, height = 6)

data_south_korea <- data_all %>%
  filter(Country == "Korea, South")

cfr_south_korea <- round(data_south_korea$Number[data_south_korea$Type == "Deaths" & data_south_korea$Date == max(data_south_korea$Date)] / data_south_korea$Number[data_south_korea$Type == "Confirmed" & data_south_korea$Date == max(data_south_korea$Date)] * 100, 1)

ggplot(data_south_korea, aes(x = Date, y = Number, group = Type, color = Type)) +
  geom_line(size = 1) +
  theme_bw() +
  ggtitle("Évolution du coronavirus en Corée du Sud") +
  xlab("Date") +
  ylab("Nombre") +
  annotate(
    geom = "text",
    label = paste0("Taux de mortalité = ", as.character(cfr_south_korea), "%"),
    x = min(data_south_korea$Date),
    y = 0.985 * max(data_south_korea$Number),
    hjust = 0,
    vjust = 1
  ) +
  scale_color_discrete(
    name = "Catégorie",
    labels = c(
      "Infections",
      "Décès",
      "Guérisons"
    )
  ) +
  scale_x_date(
    labels = scales::date_format("%m/%d/%Y"),
    date_breaks = "5 day"
  ) +
  # scale_y_continuous(trans = "log10") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(caption = "Source : Johns Hopkins University (https://github.com/CSSEGISandData/COVID-19) - Graphique de Philippe Lemoine (@phl43)") +
  ggsave("Évolution du coronavirus en Corée du Sud.png", width = 12, height = 6)