# ============================================================
# Mean ± SD Bar Graph for Biofilm Assay Data
# ============================================================

# PACKAGES ---------------------------------------------------
install.packages(c("readxl", "dplyr", "ggplot2", "writexl", "ggh4x", "writexl"))
library(readxl)
library(dplyr)
library(ggplot2)
library(ggh4x)
library(writexl)


# IMPORT DATA ------------------------------------------------

# Change this path to the location of your own Excel file.
data <- read_excel(
  "your_data_file.xlsx"
)

# Check column names
names(data)


# REMOVE UNWANTED ROWS ---------------------------------------

# Remove blank wells and keep only the concentrations
# included in the analysis.

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

# Check the concentrations
unique(data_clean$Concentration)


# CALCULATE MEAN AND SD --------------------------------------

summary_data <- data_clean %>%
  group_by(Strain, Treatment, Concentration) %>%
  summarise(
    Mean_OD595 = mean(OD595, na.rm = TRUE),
    SD_OD595 = sd(OD595, na.rm = TRUE),
    .groups = "drop"
  )


# REORDER CONCENTRATIONS -------------------------------------

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


# CHECK FOR MISSING VALUES -----------------------------------

sum(is.na(summary_data$Concentration))


# CREATE GRAPH ------------------------------------------------

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
  
  # SD error bars
  geom_errorbar(
    aes(
      ymin = Mean_OD595 - SD_OD595,
      ymax = Mean_OD595 + SD_OD595
    ),
    position = position_dodge(width = 0.8),
    width = 0.2
  ) +
  
  # Separate panel for each strain
  ggh4x::facet_wrap2(
    ~ Strain,
    ncol = 3,
    axes = "all",
    scales = "free_y"
  ) +
  
  # Treatment colours
  scale_fill_manual(
    values = c(
      "Manuka" = "#F4A300",
      "Kelulut" = "#6A3D9A"
    )
  ) +
  
  labs(
    title = "Minimum Biofilm Inhibitory Concentration (Mean ± SD)",
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


# DISPLAY GRAPH ------------------------------------------------

p


# SAVE GRAPH ---------------------------------------------------

ggsave(
  "MBIC_BarChart.png",
  plot = p,
  width = 14,
  height = 8,
  dpi = 600
)


# SAVE SUMMARY TABLE ------------------------------------------

write_xlsx(
  summary_data,
  "MBIC_Summary_Table.xlsx"
)
