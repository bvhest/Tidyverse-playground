``` r
#
# Read data from table in PDF and show as animated plot.
# BvH. 2019-04-05
#

library(tidyverse)  # i like to use the tidyverse
library(tabulizer)  # needed to read tables from pdf-documents
library(gganimate)  # animated plots based on ggplot2
library(gifski)     # fastest renderer...

###############################################################################
# load the data (source:The Brewers of Europe) and extract table from pdf
beer_production_2010_2016.df <- 
  tabulizer::extract_tables(file = "https://brewersofeurope.org/uploads/mycms-files/documents/publications/2017/Statistics-201712-001.pdf", 
                            pages = 9,
                            area = list(c(65, 55, 530, 550))) %>%
  as.data.frame(stringsAsFactors = FALSE)

# set column-names: CAN THIS BE SIMPLIFIED WITH TIDYVERSE FUNCTIONS ?
col_names <- c("Country", "2010", "2011", "2012", "2013", "2014", "2015", "2016")
colnames(beer_production_2010_2016.df) <- col_names

# extract countries
Country <-
  beer_production_2010_2016.df %>%
  slice(2:29) %>% 
  dplyr::pull(Country)

animated.plot.beer_production_2010_2016 <-
  # remove first row and data from non-EU contries and totals 
  beer_production_2010_2016.df %>%
  slice(2:29) %>%
  # remove the country column (contains alphabetical characters): CAN THIS BE SIMPLIFIED ?
  dplyr::select(-Country) %>%
  # remove all decimal grouping symbol's and transform country totals to numeric values
  purrr::map_df(str_replace, pattern = ",", replacement = "") %>% 
  purrr::map_df(as.numeric) %>%
  # add the country column again (as first column)
  tibble::add_column(Country = as.factor(Country), .before = 1) %>%
  # convert from wide to long
  tidyr::gather(key = "year", value = "production", "2010":"2016") %>%
  # keep the top 15 countries for each year. Add utility-columns with display labels for the plot.
  group_by(year) %>%
  mutate(rank = rank(-production),
         Value_rel = production / production[rank == 1],
         Value_lbl = paste0(" ", round(production, digits = 1),  " x 1000 hl")) %>%
  group_by(Country) %>% 
  filter(rank <= 15) %>%
  ungroup() %>%
  # create the plot
  ggplot(aes(x = rank, 
             group = Country,
             fill = Country, 
             color = Country)) +
  geom_tile(aes(y = production / 2,
                height = production,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(Country, " ")), vjust = 0.2, hjust = 1) +
  geom_text(aes(y = production, label = Value_lbl, hjust = 0)) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  theme_void() + 
  theme(legend.position = "none",
        panel.grid.major.x = element_line( size = .1, color = "grey" ),
        panel.grid.minor.x = element_line( size = .1, color = "grey" ),
        plot.title = element_text(size = 25, hjust = 0.5, face = "bold", vjust = -1),
        plot.subtitle = element_text(size = 18, hjust = 0.5, face = "italic"),
        plot.margin = margin(2,2, 2, 4, "cm")) + 
  # animate the plot (with dynamic title that includes the year)
  gganimate::transition_states(year, transition_length = 4, state_length = 1) +
  gganimate::view_follow(fixed_x = TRUE) +
  ggplot2::labs(title = 'European beer production per year : {closest_state}',  
                subtitle = "Top 15 Countries",
                caption = "Data Source: The Brewers of Europe")

# Render into an animated gif
anim.gif <-
  gganimate::animate(animated.plot.beer_production_2010_2016, 
                     nframes = 200, 
                     fps = 20,  
                     width = 1200, 
                     height = 1000, 
                     renderer = gifski_renderer("eu_bier_productie_animatie.gif")) 
```

<sup>Created on 2019-04-08 by the [reprex package](https://reprex.tidyverse.org) (v0.2.1.9000)</sup>
