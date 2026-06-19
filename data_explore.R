# Data explore
library(sf)
library(terra)
library(dplyr)
library(readr)
library(here)
library(ggplot2)
library(plotly)

harvest <- read_csv(here("public","hvstat_africa_data_v1.2.csv"))
harvest_africa_boundary <- st_read(here("public", "hvstat_africa_boundary_v1.2.shp"))
class(harvest$fnid)
class(harvest_africa_boundary$fnid)  

final_data <- harvest_africa_boundary %>%
  left_join(harvest, by = "fnid")

clean_data <- final_data %>%
  filter(qc_flag == 0)

data_summary <- clean_data %>%
  st_drop_geometry() %>% 
  group_by(country.x, product) %>%
  summarise(
    start_year = min(harvest_year, na.rm = TRUE),
    end_year = max(harvest_year, na.rm = TRUE),
    total_records = n(),
    yield_data_count = sum(!is.na(yield)),
    .groups = "drop"
  )

print(data_summary, n = 20)

# Split countries to three groups
all_countries <- sort(unique(data_summary$country.x))
num_countries <- length(all_countries)
n_groups <- 5
group_list <- split(all_countries, cut(seq_along(all_countries), n_groups, labels = FALSE))

# Create plot data
plot_data <- clean_data %>%
  st_drop_geometry() %>%
  group_by(country.x, admin_1.x, product, harvest_year) %>%
  summarise(avg_yield = mean(yield, na.rm = TRUE), .groups = "drop")

# Stat by country
create_dynamic_heatmap <- function(country_list, title_suffix) {
  sub_data <- plot_data %>% filter(country.x %in% country_list)
  
  data_range <- range(plot_data$avg_yield, na.rm = TRUE)
  
  p <- ggplot(sub_data, aes(x = harvest_year, y = product, fill = avg_yield,
                            text = paste("Country:", country.x, 
                                         "<br>Year:", harvest_year, 
                                         "<br>Product:", product, 
                                         "<br>Avg Yield:", round(avg_yield, 2)))) + 
    geom_tile() + 
    facet_wrap(~country.x, scales = "free_y", ncol = 2) + 
    scale_fill_viridis_c(na.value = "grey90", limits = data_range) +
    theme_minimal() +
    labs(title = paste("Harvest Data Heatmap - Part", title_suffix),
         x = "Harvest Year", y = "Product", fill = "Avg Yield") +
    theme(axis.text.y = element_text(size = 7),
          strip.text = element_text(size = 8))
  
  ggplotly(p, tooltip = "text")
}

plot_results <- list()
for (i in 1:n_groups) {
  plot_results[[i]] <- create_dynamic_heatmap(group_list[[i]], paste("Group", i))
}

plot_results[[1]]
plot_results[[2]]
plot_results[[3]]
plot_results[[4]]
plot_results[[5]]


# Stat by adim_1
plot_admin_level <- function(target_country) {
  
  sub_data <- plot_data %>% filter(country.x == target_country)
  
  p <- ggplot(sub_data, aes(x = harvest_year, y = product, fill = avg_yield)) + 
    geom_tile() +
    facet_wrap(~admin_1.x, scales = "free_y", ncol = 5) + 
    scale_fill_viridis_c(na.value = "grey90") +
    theme_minimal() +
    labs(title = paste("Yield Heatmap by Admin_1:", target_country),
         x = NULL, y = NULL, fill = "Yield") + 
    theme(
      axis.text.y = element_text(size = 5),
      strip.text = element_text(size = 6, face = "bold"),
      panel.spacing = unit(0.5, "lines"),
      legend.key.height = unit(0.5, "cm"),
      legend.key.width = unit(0.2, "cm"),
      legend.text = element_text(size = 6),

      plot.title = element_text(size = 14, face = "bold", margin = margin(b = 20))
    )
  

  ggplotly(p, width = 1600, height = 1200) %>%
    layout(
      margin = list(t = 120, l = 60, r = 20, b = 40),
      title = list(
        text = paste("Yield Heatmap by Admin_1:", target_country),
        y = 0.98,
        x = 0.5,
        xanchor = 'center',
        yanchor = 'top'
      ),
      legend = list(font = list(size = 8))
    )
}

plot_admin_level("Nigeria")

for (country in all_countries) {
  message("Plot: ", country)
  p <- plot_admin_level(country)
  print(p)
}


# Might want to use production not yield because guinea doesn't have area and yield
# Do we need to consider the season of planting and harvest? The production is based on the harvest time?


# ggplot(plot_data, aes(x = harvest_year, y = product, fill = avg_yield)) +
#   geom_tile() +
#   facet_wrap(~country.x, scales = "free_y") +
#   scale_fill_viridis_c(na.value = "white") + # na.value = "white"
#   theme_minimal() +
#   labs(title = "Crop Yield Data Availability by Country",
#        x = "Year", y = "Product", fill = "Avg Yield")
# 
# p <- ggplot(plot_data, aes(x = harvest_year, y = product, fill = avg_yield)) +
#   geom_tile() +
#   facet_wrap(~country.x) +
#   theme_minimal()
# ggplotly(p)
