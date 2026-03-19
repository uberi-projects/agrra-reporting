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
    geom_boxplot(outlier.shape = 4, outlier.colour = "black", outlier.size = 3, outlier.stroke = 1.5, fill = "grey90", colour = "grey40") +
    geom_jitter(width = 0.15, size = 1.2, alpha = 0.5, colour = "grey40") +
    stat_summary(aes(colour = "Mean"), fun = mean, geom = "point", shape = 18, size = 3) +
    stat_summary(aes(colour = "Median"), fun = median, geom = "point", shape = 16, size = 2.5) +
    facet_wrap(~Year, scales = "free_x") +
    scale_color_manual(name = "Metric", values = palette) +
    labs(
        x = "Site",
        y = expression("Commercial biomass density (g / 100 m"^2 * ")")
    ) +
    custom_theme +
    theme_pubclean() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
plot_herb_biomass <- df_fish_biomass_transect %>%
    filter(Biomass_Category == "H") %>%
    ggplot(aes(x = Site, y = Biomass_Transects_Density)) +
    geom_boxplot(outlier.shape = 4, outlier.colour = "black", outlier.size = 3, outlier.stroke = 1.5, fill = "grey90", colour = "grey40") +
    geom_jitter(width = 0.15, size = 1.2, alpha = 0.5, colour = "grey40") +
    stat_summary(aes(colour = "Mean"), fun = mean, geom = "point", shape = 18, size = 3) +
    stat_summary(aes(colour = "Median"), fun = median, geom = "point", shape = 16, size = 2.5) +
    facet_wrap(~Year, scales = "free_x") +
    scale_color_manual(name = "Metric", values = palette) +
    labs(
        x = "Site",
        y = expression("Herbivorous iomass density (g / 100 m"^2 * ")")
    ) +
    custom_theme +
    theme_pubclean() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
ggsave("2026 - HRI Data Presentation/outputs/plot_comm_biomass.png", plot_comm_biomass, width = 12, height = 6, dpi = 150)
ggsave("2026 - HRI Data Presentation/outputs/plot_herb_biomass.png", plot_herb_biomass, width = 12, height = 6, dpi = 150)

## Create Coral Cover Boxplots  ------------------------
df_coral_transect <- df_benthic_pim_cover_lcc %>%
    distinct(Year, Site, Transect, Coral_Cover_Tran)
plot_coral_cover <- df_coral_transect %>%
    ggplot(aes(x = Site, y = Coral_Cover_Tran)) +
    geom_boxplot(outlier.shape = 4, outlier.colour = "black", outlier.size = 3, outlier.stroke = 1.5, fill = "grey90", colour = "grey40") +
    geom_jitter(width = 0.15, size = 1.2, alpha = 0.5, colour = "grey40") +
    stat_summary(aes(colour = "Mean"), fun = mean, geom = "point", shape = 18, size = 3) +
    stat_summary(aes(colour = "Median"), fun = median, geom = "point", shape = 16, size = 2.5) +
    facet_wrap(~Year, scales = "free_x") +
    scale_color_manual(name = "Metric", values = palette) +
    labs(
        x = "Site",
        y = "Coral Cover (%)"
    ) +
    custom_theme +
    theme_pubclean() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
ggsave("2026 - HRI Data Presentation/outputs/plot_coral_cover.png", plot_coral_cover, width = 12, height = 6, dpi = 150)

## Create Algae Cover Boxplots  ------------------------
df_algae_transect <- df_benthic_pim_cover_fma %>%
    distinct(Year, Site, Transect, Algae_Cover_Tran)
plot_algae_cover <- df_algae_transect %>%
    ggplot(aes(x = Site, y = Algae_Cover_Tran)) +
    geom_boxplot(outlier.shape = 4, outlier.colour = "black", outlier.size = 3, outlier.stroke = 1.5, fill = "grey90", colour = "grey40") +
    geom_jitter(width = 0.15, size = 1.2, alpha = 0.5, colour = "grey40") +
    stat_summary(aes(colour = "Mean"), fun = mean, geom = "point", shape = 18, size = 3) +
    stat_summary(aes(colour = "Median"), fun = median, geom = "point", shape = 16, size = 2.5) +
    facet_wrap(~Year, scales = "free_x") +
    scale_color_manual(name = "Metric", values = palette) +
    labs(
        x = "Site",
        y = "Algae Cover (%)"
    ) +
    custom_theme +
    theme_pubclean() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
ggsave("2026 - HRI Data Presentation/outputs/plot_algae_cover.png", plot_algae_cover, width = 12, height = 6, dpi = 150)

## Create Year Comparison Boxplots (side-by-side by year)  ------------------------
palette_years <- c("2023" = "#007bb8", "2025" = "#E6A93A")

plot_comm_biomass_compare <- df_fish_biomass_transect %>%
    filter(Biomass_Category == "C") %>%
    ggplot(aes(x = Site, y = Biomass_Transects_Density, fill = factor(Year), group = interaction(Site, Year))) +
    geom_boxplot(outlier.shape = 4, outlier.colour = "black", outlier.size = 3, outlier.stroke = 1.5, colour = "black", alpha = 0.4, position = position_dodge(0.75)) +
    geom_jitter(aes(colour = factor(Year)), position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.75), size = 1.2) +
    stat_summary(aes(group = factor(Year)), fun = mean, geom = "point", shape = 18, size = 3, position = position_dodge(0.75)) +
    scale_fill_manual(name = "Year", values = palette_years) +
    scale_colour_manual(name = "Year", values = palette_years) +
    labs(
        x = "Site",
        y = expression("Commercial biomass density (g / 100 m"^2 * ")")
    ) +
    custom_theme +
    theme_pubclean() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

plot_herb_biomass_compare <- df_fish_biomass_transect %>%
    filter(Biomass_Category == "H") %>%
    ggplot(aes(x = Site, y = Biomass_Transects_Density, fill = factor(Year), group = interaction(Site, Year))) +
    geom_boxplot(outlier.shape = 4, outlier.colour = "black", outlier.size = 3, outlier.stroke = 1.5, colour = "black", alpha = 0.4, position = position_dodge(0.75)) +
    geom_jitter(aes(colour = factor(Year)), position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.75), size = 1.2) +
    stat_summary(aes(group = factor(Year)), fun = mean, geom = "point", shape = 18, size = 3, position = position_dodge(0.75)) +
    scale_fill_manual(name = "Year", values = palette_years) +
    scale_colour_manual(name = "Year", values = palette_years) +
    labs(
        x = "Site",
        y = expression("Herbivorous biomass density (g / 100 m"^2 * ")")
    ) +
    custom_theme +
    theme_pubclean() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

plot_coral_cover_compare <- df_coral_transect %>%
    ggplot(aes(x = Site, y = Coral_Cover_Tran, fill = factor(Year), group = interaction(Site, Year))) +
    geom_boxplot(outlier.shape = 4, outlier.colour = "black", outlier.size = 3, outlier.stroke = 1.5, colour = "black", alpha = 0.4, position = position_dodge(0.75)) +
    geom_jitter(aes(colour = factor(Year)), position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.75), size = 1.2) +
    stat_summary(aes(group = factor(Year)), fun = mean, geom = "point", shape = 18, size = 3, position = position_dodge(0.75)) +
    scale_fill_manual(name = "Year", values = palette_years) +
    scale_colour_manual(name = "Year", values = palette_years) +
    labs(
        x = "Site",
        y = "Coral Cover (%)"
    ) +
    custom_theme +
    theme_pubclean() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

plot_algae_cover_compare <- df_algae_transect %>%
    ggplot(aes(x = Site, y = Algae_Cover_Tran, fill = factor(Year), group = interaction(Site, Year))) +
    geom_boxplot(outlier.shape = 4, outlier.colour = "black", outlier.size = 3, outlier.stroke = 1.5, colour = "black", alpha = 0.4, position = position_dodge(0.75)) +
    geom_jitter(aes(colour = factor(Year)), position = position_jitterdodge(jitter.width = 0.15, dodge.width = 0.75), size = 1.2) +
    stat_summary(aes(group = factor(Year)), fun = mean, geom = "point", shape = 18, size = 3, position = position_dodge(0.75)) +
    scale_fill_manual(name = "Year", values = palette_years) +
    scale_colour_manual(name = "Year", values = palette_years) +
    labs(
        x = "Site",
        y = "Algae Cover (%)"
    ) +
    custom_theme +
    theme_pubclean() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

palette_years_div <- c("2021" = "#007bb8", "2025" = "#E6A93A")

plot_diversity_compare <- df_coral_diversity %>%
    ggplot(aes(x = factor(Year), y = Diversity, fill = factor(Year))) +
    geom_boxplot(outlier.shape = 4, outlier.colour = "black", outlier.size = 3, outlier.stroke = 1.5, colour = "black", alpha = 0.4) +
    geom_jitter(aes(colour = factor(Year)), width = 0.15, size = 2) +
    stat_summary(fun = mean, geom = "point", shape = 18, size = 4) +
    scale_fill_manual(name = "Year", values = palette_years_div) +
    scale_colour_manual(name = "Year", values = palette_years_div) +
    labs(
        x = "Year",
        y = "Coral Diversity (Shannon Index)"
    ) +
    custom_theme +
    theme_pubclean()

plot_recruits_compare <- df_recruits_site %>%
    pivot_longer(cols = c(All, Small, Large), names_to = "Recruit_Type", values_to = "Density") %>%
    mutate(Recruit_Type = factor(Recruit_Type, levels = c("All", "Small", "Large"))) %>%
    ggplot(aes(x = factor(Year), y = Density, fill = factor(Year))) +
    geom_boxplot(outlier.shape = 4, outlier.colour = "black", outlier.size = 3, outlier.stroke = 1.5, colour = "black", alpha = 0.4) +
    geom_jitter(aes(colour = factor(Year)), width = 0.15, size = 2) +
    stat_summary(fun = mean, geom = "point", shape = 18, size = 4) +
    facet_wrap(~Recruit_Type) +
    scale_fill_manual(name = "Year", values = palette_years) +
    scale_colour_manual(name = "Year", values = palette_years) +
    labs(
        x = "Year",
        y = expression("Recruit density (recruits / m"^2 * ")")
    ) +
    custom_theme +
    theme_pubclean()

ggsave("2026 - HRI Data Presentation/outputs/plot_comm_biomass_compare.png", plot_comm_biomass_compare, width = 14, height = 6, dpi = 150)
ggsave("2026 - HRI Data Presentation/outputs/plot_herb_biomass_compare.png", plot_herb_biomass_compare, width = 14, height = 6, dpi = 150)
ggsave("2026 - HRI Data Presentation/outputs/plot_coral_cover_compare.png", plot_coral_cover_compare, width = 14, height = 6, dpi = 150)
ggsave("2026 - HRI Data Presentation/outputs/plot_algae_cover_compare.png", plot_algae_cover_compare, width = 14, height = 6, dpi = 150)
ggsave("2026 - HRI Data Presentation/outputs/plot_diversity_compare.png", plot_diversity_compare, width = 14, height = 6, dpi = 150)
ggsave("2026 - HRI Data Presentation/outputs/plot_recruits_compare.png", plot_recruits_compare, width = 14, height = 6, dpi = 150)
