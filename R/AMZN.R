library(ggplot2)
library(dplyr)
readr::read_csv("data/AMZN_last_5m_50d.csv") -> AMZN_5m_50d
readr::read_csv("data/AMZN_last_60m_2y.csv") -> AMZN_60m_2y

ggplot(AMZN_60m_2y) +
  geom_line(aes(x = Datetime, y = Close)) +
  geom_line(aes(x = Datetime, y = Low)) +
  theme_bw()

AMZN_d_2y <- AMZN_60m_2y %>%
  mutate(Date = as.Date(Datetime)) %>%
  group_by(Date) %>%
  summarise(Open = first(Open),
            High = max(High),
            Low = min(Low),
            Close = last(Close)) %>%
  ungroup() %>% arrange(Date) %>%
  mutate(Pclose = lag(Close),
         Low_pct  = (Low - Pclose ) / Pclose,
         Close_low_pct = (Close - Low ) / Low)

AMZN_d_2y %>%
  filter(Low_pct < -0.02) %>%
  filter(Close_low_pct > 0.02)

ggplot(AMZN_d_2y) +
  geom_line(aes(x = Date, y = Low_pct)) +
  theme_bw()
