## https://www.business-science.io/business/2017/10/16/sales_backorder_prediction.html#background

library(tidyverse)
library(naniar)
library(simputation)
library(assertr)

model_data <- read_csv("Data/model_data.csv")
summary(model_data)
dim(model_data)
table(model_data$retired)


# NA's --------------------------------------------------------------------

# https://naniar.njtierney.com/articles/naniar-visualisation.html
# https://allisonhorst.shinyapps.io/missingexplorer/#section-initial-na-counts-proportions
# https://github.com/njtierney/naniar
#NA's
miss_var_summary(model_data)

gg_miss_var(model_data)

model_data %>% 
  miss_case_table()

gg_miss_which(model_data)

vis_miss(model_data)

model_data$retired = as.factor(model_data$retired)
gg_miss_fct(model_data, fct = retired)

gg_miss_upset(model_data, 
              nsets = 5)

ggplot(data = model_data,
       aes(x = cycles,
           y = avg_DisAhNeg)) +
  geom_miss_point()

model_data %>%
  bind_shadow() %>%
  ggplot(aes(x = cycles,
             fill = avg_DisAhNeg_NA )) + 
  geom_density(alpha = 0.5)

model_data %>%
  bind_shadow() %>%
  simputation::impute_lm(avg_DisAhNeg ~ cycles + avg_disch) %>%
  ggplot(aes(x = avg_disch,
             y = avg_DisAhNeg,
             colour = avg_DisAhNeg_NA)) + 
  geom_point()

miss_var_span(model_data, cycles, span_every = 30)

gg_miss_span(model_data, cycles, span_every = 30)

gg_miss_var(model_data, show_pct = TRUE)

gg_miss_var(model_data, show_pct = TRUE, facet = retired)

gg_miss_case(model_data)



# Assertion ---------------------------------------------------------------

model_data %>% 
  insist_rows(maha_dist, within_n_mads(10), everything())  %>% 
  summarise(avg.svgDisc = mean(avg_disch) )
  

tibble(maha_dist(model_data))


# tmp ---------------------------------------------------------------------


mtcars %>% 
  verify(mpg < 30) %>% 


mtcars %>%
  chain_start %>%
  verify(nrow(mtcars) > 10) %>%
  verify(mpg > 0) %>%
  insist(within_n_sds(4), mpg) %>%
  assert(in_set(0,1), am, vs) %>%
  chain_end %>%
  group_by(cyl) %>%
  summarise(avg.mpg=mean(mpg))
