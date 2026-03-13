# data_analysis.r

# Add Presence/Absence for Coral/Algae Cover ---------------------------
df_benthic_pim_cover <- df_benthic_pim %>%
    mutate(
        Coral_Presence = Vectorize(calculate_type_presence)(Organism_Bucket, Secondary_Bucket, "Coral"),
        Algae_Presence = Vectorize(calculate_type_presence)(Organism_Bucket, Secondary_Bucket, "Algae_Macro_Fleshy")
    )

# Calculate Live Coral Cover ---------------------------
df_benthic_pim_cover_lcc <- df_benthic_pim_cover %>%
    group_by(Atoll, Site, Transect) %>%
    mutate(Coral_Cover_Tran = 100 * sum(Coral_Presence) / n()) %>%
    group_by(Atoll, Site) %>%
    mutate(
        Coral_Cover_Site = mean(Coral_Cover_Tran),
        Coral_Cover_Site_Median = median(Coral_Cover_Tran)
    )
df_benthic_lcc <- df_benthic_pim_cover_lcc %>%
    group_by(Atoll) %>%
    summarize(
        `Min (Site)` = min(Coral_Cover_Site),
        `Av. (Site)` = mean(Coral_Cover_Site),
        `Median (Site)` = mean(Coral_Cover_Site_Median),
        `Max (Site)` = max(Coral_Cover_Site),
        `Min (Transect)` = min(Coral_Cover_Tran),
        `Av. (Transect)` = mean(Coral_Cover_Tran),
        `Median (Transect)` = median(Coral_Cover_Tran),
        `Max (Transect)` = max(Coral_Cover_Tran)
    ) %>%
    mutate(across(-Atoll, ~ round(.x, 2)))
df_benthic_lcc_sites <- df_benthic_pim_cover_lcc %>%
    group_by(Atoll, Site) %>%
    summarize(
        `Min (Transect)` = min(Coral_Cover_Tran),
        `Av. (Transect)` = mean(Coral_Cover_Tran),
        `Median (Transect)` = median(Coral_Cover_Tran),
        `Max (Transect)` = max(Coral_Cover_Tran)
    )

# Calculate Fleshy Macroalgae Cover ---------------------------
df_benthic_pim_cover_fma <- df_benthic_pim_cover %>%
    group_by(Atoll, Site, Transect) %>%
    mutate(Algae_Cover_Tran = 100 * sum(Algae_Presence) / n()) %>%
    group_by(Atoll, Site) %>%
    mutate(
        Algae_Cover_Site = mean(Algae_Cover_Tran),
        Algae_Cover_Site_Median = median(Algae_Cover_Tran)
    )
df_benthic_fma <- df_benthic_pim_cover_fma %>%
    group_by(Atoll) %>%
    summarize(
        `Min (Site)` = min(Algae_Cover_Site),
        `Av. (Site)` = mean(Algae_Cover_Site),
        `Median (Site)` = mean(Algae_Cover_Site_Median),
        `Max (Site)` = max(Algae_Cover_Site),
        `Min (Transect)` = min(Algae_Cover_Tran),
        `Av. (Transect)` = mean(Algae_Cover_Tran),
        `Median (Transect)` = median(Algae_Cover_Tran),
        `Max (Transect)` = max(Algae_Cover_Tran)
    ) %>%
    mutate(across(-Atoll, ~ round(.x, 2)))
df_benthic_fma_sites <- df_benthic_pim_cover_fma %>%
    group_by(Atoll, Site) %>%
    summarize(
        `Min (Transect)` = min(Algae_Cover_Tran),
        `Av. (Transect)` = mean(Algae_Cover_Tran),
        `Median (Transect)` = median(Algae_Cover_Tran),
        `Max (Transect)` = max(Algae_Cover_Tran)
    )

# Calculate Fish Biomass ---------------------------
df_fish_biomass_obs <- df_fish %>% # Observation-level
    mutate(Biomass_Observations = Observations * (LWRa * ((LWRconv * Size_Class)^LWRb))) %>%
    mutate(Biomass_Category = case_when(
        Fish_Family == "Acanthuridae" ~ "H",
        Fish_Family == "Scaridae" ~ "H",
        Fish_Family == "Epinephelidae" ~ "C",
        Fish_Family == "Serranidae" ~ "C",
        Fish_Family == "Lutjanidae" ~ "C"
    )) %>%
    filter(!is.na(Biomass_Category))
distinct_fish_transects <- distinct(select(df_fish, Atoll, Site, Transect))
distinct_biomass_categories <- c("C", "H")
df_fish_biomass_transect_incomplete <- df_fish_biomass_obs %>%
    group_by(Atoll, Site, Transect, Biomass_Category) %>%
    summarize(Biomass_Transects = sum(Biomass_Observations)) %>%
    ungroup()
df_fish_biomass_transect <- distinct_fish_transects %>% # Transect-level
    tidyr::crossing(Biomass_Category = distinct_biomass_categories) %>%
    left_join(
        df_fish_biomass_transect_incomplete,
        by = c("Atoll", "Site", "Transect", "Biomass_Category")
    ) %>%
    mutate(
        Biomass_Transects = tidyr::replace_na(Biomass_Transects, 0),
        Biomass_Transects_Density = 100 * Biomass_Transects / (2 * 30)
    )
df_fish_biomass_site <- df_fish_biomass_transect %>% # Site-level
    group_by(Atoll, Site, Biomass_Category) %>%
    summarize(
        Biomass_Sites_Density = mean(Biomass_Transects_Density),
        Biomass_Sites_Density_Median = median(Biomass_Transects_Density),
        Biomass_Sites_Detection_Rate = mean(Biomass_Transects_Density > 0)
    )
df_fish_biomass_overall <- df_fish_biomass_site %>%
    group_by(Atoll, Biomass_Category) %>%
    summarize(
        Biomass_Sites_Density_Min = min(Biomass_Sites_Density),
        Biomass_Sites_Density_Max = max(Biomass_Sites_Density),
        Biomass_Sites_Density_Median = mean(Biomass_Sites_Density_Median),
        Biomass_Sites_Density = mean(Biomass_Sites_Density)
    )
