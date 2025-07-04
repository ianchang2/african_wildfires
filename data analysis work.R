install.packages("ggtext")
library(here)
library(tidyverse)
library(ggtext)

wildfire <- read_csv(here("data","annual-area-burnt-by-wildfires.csv"))

wildfire_clean <- wildfire %>%
  filter(Entity %in% c("North America", "South America", "Asia", "Europe", "Africa", "Oceania")) %>%
  filter(2012 <= Year & 2024 >= Year) %>%
  rename(area_ha = "Annual area burnt by wildfires")

wildfire_clean$Entity <- factor(wildfire_clean$Entity, 
                                levels = c("Europe", 
                                           "Asia", 
                                           "Oceania", 
                                           "North America", 
                                           "South America", "Africa"))

wildfire_clean <- wildfire_clean %>%
  group_by(Year)%>%
  mutate(area_pct = area_ha/sum(area_ha))%>%
  ungroup()

gg_labels <- tibble(
  Entity = c("Europe", "Asia", "Oceania", "North", "and", "South America", "Africa"),
              x = 14,
              y = c(.985, .93, .86, .795, .74, .68, .30), 
  color = c("#BFCBB7", "#C1A686", "#B3A3C2", "#b3c3c5", "#6B6B6B", "#81919c", "#821E1E"))


ggplot(wildfire_clean, aes(x = factor(Year), y = area_pct, fill = Entity)) +
  geom_bar(stat = "identity", width = 0.8) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_discrete(breaks = seq(min(wildfire_clean$Year), max(wildfire_clean$Year), by = 4)) +
  scale_fill_manual(values = c(
    "Africa" = "#821E1E",
    "North America" = "#b3c3c5",
    "South America" = "#81919c",
    "Oceania" = "#B3A3C2",
    "Asia" = "#C1A686",
    "Europe" = "#BFCBB7"
  )) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "black", linewidth = 0.7) +
  labs(
    title = "More than half of the area burned by<br><b>wildfire is in <span style='color:#821E1E'>Africa</span></b>",
    subtitle = "Each region's share of the global area burned by wildfire.",
    x = NULL,
    y = NULL,
    fill = NULL,
    caption = "**Data source:** Global Wildfire Information System (2025)"
  ) +
  geom_text(data = gg_labels, aes(x = x, y = y, label = Entity, hjust = 0, color = color), size = 3.5) +
  scale_color_identity() +
  theme_minimal() +
  coord_cartesian(clip = "off") +
  theme(
    legend.position = "none",
    plot.title = ggtext::element_markdown(family = "serif", face = "bold", size = 16),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.caption = ggtext::element_markdown(hjust = 0, color = "#6B6B6B"),
    plot.subtitle = element_text(size = 10),
    plot.margin = margin(t = 10, r = 100, b = 10, l = 10, unit = "pt")
  ) 

?theme

ggplot(wildfire_clean) +
  geom_line(aes(x = Year, y = area_pct, color = Entity)) +
  scale_x_continuous(breaks = seq(min(wildfire_clean$Year), max(wildfire_clean$Year), by = 2)) +
  scale_y_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  scale_color_manual(values = c(
    "Africa" = "#0072B2",
    "North America" = "#E69F00",
    "South America" = "#56B4E9",
    "Oceania" = "#009E73",
    "Asia" = "#F0E442",
    "Europe" = "#CC0000"
  )) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "black", linewidth = 0.5)+
  labs(
    title = "More than half of the area burned by<br><b>wildfire each year is in <span style='color:#0072B2'>Africa</span></b>",
    subtitle = "Each region's share of the global area burned by wildfire.",
    x = NULL,
    y = "Percentage of Global Area Burned by Wildfire",
    fill = NULL,
    caption = "**Data source:** Global Wildfire Information System (2025)"
  ) +
  theme_minimal(base_family = "serif") +
  theme(
    legend.position = "right",
    plot.title = ggtext::element_markdown(family = "serif", face = "bold", size = 16),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.caption = ggtext::element_markdown(hjust = 0, color = "#6B6B6B"),
    plot.subtitle = element_text(size = 10),
    plot.margin = margin(t = 10, r = 10, b = 10, l = 10, unit = "pt")
  )
