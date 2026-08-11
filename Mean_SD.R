library(readxl)
library(dplyr)
library(ggplot2)
library(ggh4x)

#IMPORT DATA
data <- read_excel(
  "copy_your_file_path.xlsx"
)
names(data)

#REMOVE UNWANTED ROWS
data_clean <- data %>%
  filter(
    Strain != "Blank",
    Concentration %in% c(
      "3.125",
      "6.25",
      "12.5",
      "25",
      "50",
      "100"
    )
  )
#CHECK
unique(data_clean$Concentration)

#CALCULATE MEAN AND SD
summary_data <- data_clean %>%
  group_by(Strain, Treatment, Concentration) %>%
  summarise(
    Mean_OD595 = mean(OD595, na.rm = TRUE),
    SD_OD595 = sd(OD595, na.rm = TRUE),
    .groups = "drop"
  )

#REORDER ACCORDING TO YOUR WANTS
summary_data$Concentration <- factor(
  summary_data$Concentration,
  levels = c(
    "3.125",
    "6.25",
    "12.5",
    "25",
    "50",
    "100"
  )
)

#DOUBLE CHECK FOR ANY NA
sum(is.na(summary_data$Concentration))

#CREATE GRAPH
p <- ggplot(
  summary_data,
  aes(
    x = Concentration,
    y = Mean_OD595,
    fill = Treatment
  )
) +
  geom_col(
    colour = "black",
    position = position_dodge(width = 0.8),
    width = 0.8
  ) +
  geom_errorbar(
    aes(
      ymin = Mean_OD595 - SD_OD595,
      ymax = Mean_OD595 + SD_OD595
    ),
    position = position_dodge(width = 0.8),
    width = 0.2
  ) +
  facet_wrap(
    ~ Strain,
    ncol = 3
  ) +
  scale_fill_manual(
    values = c(
      "Cerana" = "#F4A300",
      "Kelulut" = "#6A3D9A"
    )
  ) +
  labs(
    title = "Minimum Concentration (Mean ± SD)",
    x = "Concentration (% w/v)",
    y = "OD595",
    fill = "Treatment"
  ) +
  theme_bw(base_size = 14) +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = 18
    ),
    strip.background = element_rect(
      fill = "grey90",
      colour = "black"
    ),
    strip.text = element_text(
      face = "bold",
      size = 13
    ),
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    axis.title = element_text(
      face = "bold"
    ),
    legend.position = "right",
    legend.title = element_text(
      face = "bold"
    ),
    panel.grid.minor = element_blank()
  )

p

#FREE Y-AXIS ACCORDING TO DATA (NOT UNIFORM)
ggh4x::facet_wrap2(
  ~ Strain,
  ncol = 3,
  axes = "all",
  scales = "free_y"
)


#SAVE GRAPH
ggsave(
  "BarChart.png",
  plot = p,
  width = 14,
  height = 8,
  dpi = 600
)

#SAVE SUMMARY TABLE OF SD AND MEAN
library(writexl)

write_xlsx(
  summary_data,
  "file_name"
)
