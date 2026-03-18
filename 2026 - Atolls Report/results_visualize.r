# results_visualize.r

## Define color palettes ------------------------
palette <- c(
    "#007bb8", "#E6A93A", "#7BB800", "#eeaadc", "#a3d1e1",
    "#bb75e7", "#d85b5b", "#cee406", "#755656", "#4cb1b5"
) # discrete
palette_cont <- c("#f3fafd", "#a3d1e1", "#e6e64c", "#ddb263", "red") # continuous

## Define ggplot theme ------------------------
custom_theme <- theme(
    axis.title = element_text(size = 26),
    axis.text = element_text(size = 18),
    axis.ticks = element_line(color = "black", size = 1.2),
    axis.ticks.length = unit(0.4, "cm"),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 16),
    panel.grid.major.y = element_line(color = "black", linewidth = 0.5, linetype = "dashed"),
    panel.grid.minor.y = element_line(color = "grey85", linewidth = 0.3)
)

## Create Fish Biomass Boxplots  ------------------------
plot_comm_biomass <- df_fish_biomass_transect %>%
    filter(Biomass_Category == "C") %>%
    ggplot(aes(x = Site, y = Biomass_Transects_Density)) +
    geom_boxplot(outlier.shape = NA, fill = "grey90", colour = "grey40") +
    geom_jitter(width = 0.15, size = 1.2, alpha = 0.5, colour = "grey40") +
    stat_summary(aes(colour = "Mean"), fun = mean, geom = "point", shape = 18, size = 3) +
    stat_summary(aes(colour = "Median"), fun = median, geom = "point", shape = 16, size = 2.5) +
    facet_wrap(~Atoll, scales = "free_x") +
    scale_color_manual(name = "Metric", values = palette) +
    labs(
        x = "Site",
        y = expression("Biomass density (g / 100 m"^2 * ")")
    ) +
    custom_theme +
    theme_pubclean() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
plot_herb_biomass <- df_fish_biomass_transect %>%
    filter(Biomass_Category == "H") %>%
    ggplot(aes(x = Site, y = Biomass_Transects_Density)) +
    geom_boxplot(outlier.shape = NA, fill = "grey90", colour = "grey40") +
    geom_jitter(width = 0.15, size = 1.2, alpha = 0.5, colour = "grey40") +
    stat_summary(aes(colour = "Mean"), fun = mean, geom = "point", shape = 18, size = 3) +
    stat_summary(aes(colour = "Median"), fun = median, geom = "point", shape = 16, size = 2.5) +
    facet_wrap(~Atoll, scales = "free_x") +
    scale_color_manual(name = "Metric", values = palette) +
    labs(
        x = "Site",
        y = expression("Biomass density (g / 100 m"^2 * ")")
    ) +
    custom_theme +
    theme_pubclean() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
ggsave("2026 - Atolls Report/outputs/plot_comm_biomass.png", plot_comm_biomass, width = 12, height = 6, dpi = 150)
ggsave("2026 - Atolls Report/outputs/plot_herb_biomass.png", plot_herb_biomass, width = 12, height = 6, dpi = 150)

## Create Fish Biomass Boxplots Turneffe Only  ------------------------
plot_turneffe_biomass <- df_fish_biomass_transect %>%
    filter(Atoll == "Turneffe") %>%
    ggplot(aes(x = Site, y = Biomass_Transects_Density)) +
    geom_boxplot(outlier.shape = NA, fill = "grey90", colour = "grey40") +
    geom_jitter(width = 0.15, size = 1.2, alpha = 0.5, colour = "grey40") +
    stat_summary(aes(colour = "Mean"), fun = mean, geom = "point", shape = 18, size = 3) +
    stat_summary(aes(colour = "Median"), fun = median, geom = "point", shape = 16, size = 2.5) +
    facet_wrap(~Biomass_Category, scales = "free", labeller = labeller(Biomass_Category = c("C" = "Commercial Fish", "H" = "Herbivorous Fish"))) +
    scale_color_manual(name = "Metric", values = palette) +
    labs(
        x = "Site",
        y = expression("Biomass density (g / 100 m"^2 * ")")
    ) +
    custom_theme +
    theme_pubclean() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
ggsave("2026 - Atolls Report/outputs/plot_turneffe_biomass.png", plot_turneffe_biomass, width = 12, height = 6, dpi = 150)
