library(tidyverse)

# Total retention capacity available across the catchment
capacity <- seq(0, 100, by = 0.5)

benefit_data <- tibble(
  retention_capacity = capacity,
  
  # Capacity placed in locations that strongly control nuisance flooding.
  # Large early benefits, followed by diminishing returns.
  high_value_location =
    75 * (1 - exp(-capacity / 22)),
  
  # Capacity initially has little effect, but eventually crosses a
  # hydraulic threshold and produces a substantial benefit.
  threshold_location =
    58 / (1 + exp(-(capacity - 48) / 5)),
  
  # Moderate but relatively inefficient spatial allocation.
  moderate_location =
    43 * (1 - exp(-capacity / 38)),
  
  # Storage placed in a location with little influence on the
  # nuisance-flooding problem.
  low_value_location =
    10 * (1 - exp(-capacity / 35)),
  
  # Poorly coordinated distributed capacity.
  uncoordinated =
    27 * (1 - exp(-capacity / 50))
) |>
  pivot_longer(
    cols = -retention_capacity,
    names_to = "allocation",
    values_to = "flood_benefit"
  ) |>
  mutate(
    allocation = factor(
      allocation,
      levels = c(
        "high_value_location",
        "threshold_location",
        "moderate_location",
        "uncoordinated",
        "low_value_location"
      ),
      labels = c(
        "High-value location",
        "Threshold-sensitive location",
        "Moderate-value location",
        "Uncoordinated allocation",
        "Low-value location"
      )
    )
  )

# Upper envelope: the maximum benefit available at each capacity
efficient_frontier <- benefit_data |>
  group_by(retention_capacity) |>
  summarise(
    flood_benefit = max(flood_benefit),
    .groups = "drop"
  )

ggplot(
  benefit_data,
  aes(
    x = retention_capacity,
    y = flood_benefit,
    colour = allocation
  )
) +
  geom_line(
    linewidth = 1.25,
    alpha = 0.85
  ) +
  
  # Efficient frontier shown as a thicker black dashed line
  geom_line(
    data = efficient_frontier,
    aes(
      x = retention_capacity,
      y = flood_benefit
    ),
    inherit.aes = FALSE,
    colour = "black",
    linewidth = 1.8,
    linetype = "dashed"
  ) +
  
  scale_colour_manual(
    values = c(
      "High-value location"          = "#0072B2",
      "Threshold-sensitive location" = "#D55E00",
      "Moderate-value location"      = "#009E73",
      "Uncoordinated allocation"     = "#CC79A7",
      "Low-value location"           = "#999999"
    )
  ) +
  
  annotate(
    "text",
    x = 62,
    y = 76,
    label = "Maximum efficient frontier",
    hjust = 0,
    fontface = "bold",
    size = 4
  ) +
  
  annotate(
    "segment",
    x = 61,
    xend = 53,
    y = 74,
    yend = 68,
    arrow = arrow(length = unit(0.18, "cm"))
  ) +
  
  labs(
    x = "Retention capacity available",
    y = "Nuisance-flood mitigation benefit",
    colour = "Spatial allocation",
    title = "Spatially dependent nuisance-flood mitigation",
    subtitle = paste(
      "Identical retention capacity can produce substantially different",
      "benefits depending on its location and coordination"
    ),
    caption = paste(
      "The maximum efficient frontier represents the greatest nuisance-flood",
      "benefit achievable from each level of retention capacity."
    )
  ) +
  
  theme_classic(base_size = 13) +
  
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    plot.caption = element_text(
      hjust = 0,
      colour = "grey35"
    )
  )