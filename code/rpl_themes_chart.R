
## wrangle and plot Social Vulnerability Index data (https://www.atsdr.cdc.gov/placeandhealth/svi/data_documentation_download.html, 2018 version to best match other input datasets). Only for exploration.  Chart not used in manuscript.
# data downloaded and loaded into ArcGIS Pro.  Fields in the data below were exported from the attribute table as a .csv file.


## load packages
library(tidyverse)
library(scales)

## read data
svi <- read.csv("data/rpl_themes.csv")

## add intervals to match ArcGIS quantiles (mostly for labels)
svi <- svi %>%
      mutate(quantiles =cut(rpl_themes, 
                            breaks = c(
                              0,
                              0.2,
                              0.4,
                              0.6,
                              0.8,
                              1.0),
                            labels = c(
                              "0 - 0.2",
                              "0.2 - 0.4",
                              "0.4 - 0.6",
                              "0.6 - 0.8",
                              "0.8 - 1.0")
                            ))

## create colors to match map
colors <- c(
  "#EDF8FB",
  "#B3CDE3",
  "#8C96C6",
  "#8856A7",
  "#632360"
  
)


# group by quantiles for chart
svi_pop_quantiles <- svi %>%
  group_by(quantiles) %>%
  summarize(total_pop = sum(e_pop)) %>%
  mutate(percentage = round(total_pop/sum(total_pop)*100))

# make chart

svi_quantiles_pop_chart <-
  ggplot(svi_pop_quantiles, aes(x = quantiles, y = total_pop, fill = quantiles)) +
  geom_bar(stat = 'identity', color = '#3d3d3d') +
  coord_flip() +
  labs(
    x = "SVI Level",
    y = "Total Population"
    ) +
  scale_fill_manual(values = c(
    "#EDF8FB",
    "#B3CDE3",
    "#8C96C6",
    "#8856A7",
    "#632360"  )) +
  scale_y_continuous(labels = comma) +
  # geom_text(aes(label = paste0(percentage, "%")),
  #           #vjust = -0.0, 
  #           hjust = -0.15, 
  #           color = "black",
  #           size = 14) + 
  theme_bw(base_size = 14) +
  theme(legend.position = 'none') +
  # theme(plot.margin = margin(0, #top
  #                            3, #right
  #                            0, # bottom
  #                            0, #left
  #                            "cm")) + 
  expand_limits(y = 160000000) + 
  theme(
    text = element_text(color = "black"),
    title = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    axis.text = element_text(color = "black"),
    axis.ticks = element_line(color = "black"),
    panel.grid = element_line(color = "black"),
    legend.text = element_text(color = "black"),
    legend.title = element_text(color = "black"),
    panel.background = element_rect(fill = 'transparent'), #transparent panel bg
    plot.background = element_rect(fill = 'transparent', color = NA), #transparent plot bg
    panel.grid.major = element_blank(), #remove major gridlines
    panel.grid.minor = element_blank(), #remove minor gridlines
    legend.background = element_rect(fill = 'transparent'), #transparent legend bg
    legend.box.background = element_rect(fill = 'transparent'), #transparent legend panel
    panel.border = element_blank(), axis.line = element_line(),
    #axis.title.y = element_blank(),
    #axis.text.y = element_blank(),
    axis.ticks.y = element_blank())


svi_quantiles_pop_chart

# save
ggsave("svi_600x200.png", 
       svi_quantiles_pop_chart, 
       width = 6, 
       height = 2, 
       units = "in", 
       dpi = 300, 
       bg ='transparent')





