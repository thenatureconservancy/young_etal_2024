

## wrangle and plot data to make bubble chart that depicts Social Vulnerability Index and Wildfire Potential to Surface Water and County population.
# input data created in Excel, by simply selecting the necessary columns in the 'data/main_df.csv' file, then exporting that as the file read in on line 13 below.

# packages
library(tidyverse)
library(plotly)
library(scales)


# read data
fire_svi_wfp_pop <- read.csv('data/cnty_svi_wfp.csv')

# add in quartiles for svi and sfp

fire_svi_wfp_pop <- fire_svi_wfp_pop %>%
  mutate(wfp_quantiles = cut(weighted_wfp, 
                        breaks = c(
                          -1,
                          25,
                          50,
                          75,
                          100),
                        labels = c(
                          "0 - 25",
                          "26 - 50",
                          "51- 75",
                          "76 - 100")
  ))


fire_svi_wfp_pop <- fire_svi_wfp_pop %>%
  mutate(svi_quantiles = cut(svi, 
                            breaks = c(
                              -1,
                              25,
                              50,
                              75,
                              100),
                            labels = c(
                              "0 - 25",
                              "26 - 50",
                              "51- 75",
                              "76 - 100")
  ))

#  write to csv to manually paste in colors in excel by first filtering  then pasting
write.csv(fire_svi_wfp_pop, file = 'data/fire_svi_wfp_pop_quarts.csv')


# read data in with colors
fire_svi_wfp_pop <- read.csv('data/fire_svi_wfp_pop_quarts_colors.csv')


fire_svi_wfp_pop$color <- as.factor(fire_svi_wfp_pop$color)

# static bubble chart



fire_svi_pop_chart <- fire_svi_wfp_pop %>%
  ggplot(aes(x = weighted_wfp, 
             y = svi, 
             size = pop,
             fill = color)) +
  geom_point(shape = 21) +
  scale_size(range = c(.05, 15), 
             name = "County Population", 
             labels = comma,
             breaks =  c(1000,
                         10000,
                         100000,
                         1000000,
                         10000000)) +
  scale_fill_manual(values = levels(fire_svi_wfp_pop$color), guide = 'none') +
  theme_bw() +
  labs(
    x = "Wildfire Potential to Surface Water",
    y = "Social Vulnerability"
  )

fire_svi_pop_chart


ggsave("bubble_1200x600.png", 
       fire_svi_pop_chart, 
       width = 12, 
       height = 6, 
       units = "in", 
       dpi = 300, 
       bg ='transparent')


# Convert ggplot chart to Plotly for exploration
plotly_chart <- ggplotly(fire_svi_pop_chart)

plotly_chart <- hide_legend(plotly_chart) 


  

# Display the Plotly chart
plotly_chart 


# try panel chart

ggplot(fire_svi_wfp_pop, aes(x = wfp_quantiles, y = pop, fill = svi_quantiles)) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Current status of Geophysical Settings ",
    subtitle = "Descending order by amount of NaturalVeg",
    caption = "Data from TNC's Center for Resilient Conservation Science and LANDFIRE",
    x = "Geophysical Setting",
    y = "Percentage" ) +
  theme_minimal(base_size = 16) +
  theme(panel.grid = element_blank()) +
  facet_wrap(~ svi_quantiles, ncol = 4) + 
  guides(fill = "none") 





















