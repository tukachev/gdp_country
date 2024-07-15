# Загрузка необходимых пакетов
library(ggbump)
library(tidyverse)
library(shadowtext)

# Загрузка данных
data <- read.csv("gdp_country.csv")

# Перевод в правильный порядок показателей
data$measure <-
  factor(data$measure,
         levels = c("gdp_per_person", "adjusted_cost", "adjusted_cost_hours"))

# Кодируем цвета для стран
country_colors <- c(
  "Norway" = "black",
  "Luxembourg" = "#CDCDCD",
  "Qatar" = "#CDCDCD",
  "Belgium" = "#CDCDCD",
  "Denmark" = "#CDCDCD",
  "Switzerland" = "#FA9390",
  "Iceland" = "#CDCDCD",
  "Austria" = "#CDCDCD",
  "Sweden" = "#CDCDCD",
  "United States" = "#E3120B",
  "Netherlands" = "#CDCDCD",
  "Germany" = "#8FAECC",
  "Singapore" = "#CDCDCD",
  "France" = "#CDCDCD",
  "Finland" = "#CDCDCD",
  "Britain" = "#CDCDCD",
  "UAE" = "#CDCDCD",
  "Italy" = "#CDCDCD",
  "Macao" = "#CDCDCD",
  "Australia" = "#CDCDCD",
  "Canada" = "#1F5C99",
  "Israel" = "#CDCDCD",
  "Hong Kong" = "#CDCDCD",
  "Brunei" = "#CDCDCD",
  "Andorra" = "#CDCDCD"
)

# Создание графика с использованием ggbump
ggplot(data, aes(
  x = measure,
  y = rank,
  group = country,
  color = country
)) +
  geom_bump(size = 1, smooth = 6) +
  geom_point(color = "white", size = 5) +
  geom_point(size = 4) +
  geom_text(
    data = data %>%
      filter(measure == "adjusted_cost_hours"),
    aes(label = country),
    colour = "black",
    fontface = if_else(
      data$country[data$measure == "adjusted_cost_hours"] %in% c("Norway", "Switzerland", "United States", "Germany", "Canada"),
      "bold",
      "plain"
    ),
    hjust = 0,
    nudge_x = 0.1,
    size = 4.5
  ) +
  # geom_text(
  #   data = data %>%
  #     filter(measure == "adjusted_cost" &
  #              country %in% c("Brunei", "Andorra")),
  #   aes(label = country),
  #   colour = "black",
  #   hjust = 0,
  #   nudge_x = 0.05,
  #   size = 4.5
  # ) +
  # geom_text(
  #   data = data %>%
  #     filter(measure == "gdp_per_person" & country %in% c("Canada", "Israel", "Hong Kong")),
  #   aes(label = country),
  #   colour = "black",
  #   fontface = if_else(
  #     c("Canada", "Israel", "Hong Kong") %in% c("Canada"),
  #     "bold",
  #     "plain"
  #   ),
  #   hjust = 0,
  #   nudge_x = 0.05,
  #   size = 4.5
  # ) +
  shadowtext::geom_shadowtext(
    data = data %>%
      filter(measure == "adjusted_cost" &
               country %in% c("Brunei", "Andorra")),
    aes(label = country),
    colour = "black",
    bg.color = "white",
    bg.r = 0.25,
    hjust = 0,
    nudge_x = 0.05,
    size = 4.5
  ) +
  shadowtext::geom_shadowtext(
    data = data %>%
      filter(
        measure == "gdp_per_person" &
          country %in% c("Canada", "Israel", "Hong Kong")
      ),
    aes(label = country),
    colour = "black",
    fontface = if_else(
      c("Canada", "Israel", "Hong Kong") %in% c("Canada"),
      "bold",
      "plain"
    ),
    bg.color = "white",
    bg.r = 0.25,
    hjust = 0,
    nudge_x = 0.05,
    size = 4.5
  ) +
  scale_y_continuous(
    breaks = c(1, 5, 10, 15, 20),
    transform = "reverse",
    labels = c("1st", "5th", "10th", "15th", "20th"),
    expand = c(0.025, 0.025)
  ) +
  scale_x_discrete(
    position = "top",
    labels = c(
      "↓ GDP per person\nat market exchange rates",
      "↓ Adjusted for\ncost differences*",
      "↓ Adjusted for costs\nand hours worked"
    ),
    expand = expansion(mult = c(.025, .25))
  ) +
  scale_color_manual(values = country_colors) +
  labs(
    title = "Top countries ranked using three GDP measures",
    subtitle = "2023, $, current prices",
    x = "",
    y = "Rank",
    caption = "*At purchasing-power parity"
  ) +
  theme_minimal() +
  coord_cartesian(ylim = c(20, 1),
                  clip = "on",
                  expand = T) +
  theme(
    text = element_text(family = hrbrthemes::font_es, size = 16),
    axis.title.y = element_blank(),
    plot.caption = element_text(
      hjust = 0,
      color = "gray50",
      margin = margin(t = 25)
    ),
    legend.position = "none",
    axis.title.x = element_text(
      hjust = 0,
      color = "grey20",
      margin = margin(t = 12)
    ),
    axis.text.x = element_text(
      color = "black",
      size = 13,
      face = "bold",
      angle = 0,
      hjust = 0.03,
      vjust = 0
    ),
    axis.text.y = element_text(
      color = "black",
      size = 13,
      face = "bold"
    ),
    plot.title = element_text(face = "bold"),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    panel.grid = element_blank(),
    plot.margin = margin(rep(15, 4))
  )

ggsave(
  filename = "gdp.png",
  bg = "white",
  dpi = 300,
  width = 6,
  height = 6,
  scale = 1.5
)
