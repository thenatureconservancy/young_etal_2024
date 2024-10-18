
## wrangle and plot forest to faucets data, IMP attribute for exploration.  Chart not used in manuscript.
# input datasets include main dataframe from ArcGIS pro with most relevant inputs joined in plus a .csv file of the forest to faucets IMP attribute

## load packages
library(tidyverse)
library(scales)



## read data
main_df <- read_csv("data/main_df.csv", 
                    col_types = cols(geoid = col_character())) %>%
                    select(geoid, pop)  %>%
                    mutate(geoid = str_pad(geoid, 
                                           width = 5, 
                                           side = "left", 
                                           pad = "0"))

imp <- read_csv("data/imp_cnties_full.csv")

## merge in population data

imp <- left_join(imp, main_df,
                 by = c('geoid_text' = 'geoid')) %>%
                 select(geoid_text, weighted_imp, pop)
        

## add intervals to match ArcGIS quantiles (mostly for labels)
imp <- imp %>%
  mutate(quantiles = cut(weighted_imp, 
                        breaks = c(
                          -1,
                          20,
                          40,
                          60,
                          80,
                          100),
                        labels = c(
                          "0 - 20",
                          "20 - 40",
                          "40 - 60",
                          "60 - 80",
                          "80 - 100")
))

## create colors to match map
colors <- c(
  "#EFF3FF",
  "#BDD7E7",
  "#6BAED6",
  "#3182BD",
  "#08519C"
)




# group by quantiles for chart
imp_pop_quantiles <- imp %>%
  group_by(quantiles) %>%
  summarize(total_pop = sum(pop)) %>%
  mutate(percentage = round(total_pop/sum(total_pop)*100))

# make chart

imp_quantiles_pop_chart <-
  ggplot(imp_pop_quantiles, aes(x = quantiles, y = total_pop, fill = quantiles)) +
  geom_bar(stat = 'identity', color = '#3d3d3d') +
  coord_flip() +
  labs(
    x = "",
    y = "Total Population"
  ) +
  scale_fill_manual(values = c(
    "#EFF3FF",
    "#BDD7E7",
    "#6BAED6",
    "#3182BD",
    "#08519C" ))  +
  scale_y_continuous(labels = comma) +
  geom_text(aes(label = paste0(percentage, "%")),
            vjust = -0.5, 
            hjust = -0.10, 
            color = "black",
            size = 6) + 
  theme_bw(base_size = 18) +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  theme(legend.position = 'none') +
  theme(plot.margin = margin(0, #top
                              3, #right
                              0, # bottom
                              0, #left
                              "cm")) + 
  expand_limits(y = 160000000) 


imp_quantiles_pop_chart
