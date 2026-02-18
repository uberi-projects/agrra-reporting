# data_analysis.r

# Merge Turneffe and Lighthouse Data ---------------------------
df_benthic_pim <- df_benthic_pim_t %>% filter(Year == 2025)
df_coral <- df_coral_t %>% filter(Year == 2025)
df_fish <- df_fish_t %>% filter(Year == 2025)

# Add Percents for Coral/Algae ---------------------------
ref_organisms_benthic_unique <- ref_organisms_benthic %>%
    filter(!is.na(Organism)) %>%
    distinct(Organism, AGRRA_Bucket)
df_benthic_cover_presence <- df_benthic_pim %>%
    left_join(ref_organisms_benthic_unique %>% select(Organism, Organism_Bucket = AGRRA_Bucket), by = "Organism") %>%
    left_join(ref_organisms_benthic_unique %>% select(Organism, Secondary_Bucket = AGRRA_Bucket), by = c("Secondary" = "Organism")) %>%
    mutate(
        Coral_Presence = Vectorize(calculate_type_presence)(Organism_Bucket, Secondary_Bucket, "Coral"),
        Algae_Presence = Vectorize(calculate_type_presence)(Organism_Bucket, Secondary_Bucket, "Algae_Macro_Fleshy"),
        Year = format(as.Date(Date), format = "%Y")
    )

# Calculate Live Coral Cover ---------------------------
df_benthic_cover_presence_lcc <- df_benthic_cover_presence %>%
    group_by(Year, Site, Transect) %>%
    mutate(Coral_Cover_Tran = 100 * sum(Coral_Presence) / n()) %>%
    group_by(Year, Site) %>%
    mutate(Coral_Cover_Site = mean(Coral_Cover_Tran))
df_benthic_lcc <- df_benthic_cover_presence_lcc %>%
    group_by(Year) %>%
    summarize(
        `Min (Site)` = min(Coral_Cover_Site),
        `Av. (Site)` = mean(Coral_Cover_Site),
        `Median (Site)` = median(Coral_Cover_Site),
        `Max (Site)` = max(Coral_Cover_Site),
        `Min (Transect)` = min(Coral_Cover_Tran),
        `Av. (Transect)` = mean(Coral_Cover_Tran),
        `Median (Transect)` = median(Coral_Cover_Tran),
        `Max (Transect)` = max(Coral_Cover_Tran)
    ) %>%
    mutate(across(-Year, ~ round(.x, 2)))
df_benthic_lcc_sites <- df_benthic_cover_presence_lcc %>%
    group_by(Year, Site) %>%
    summarize(
        `Min (Transect)` = min(Coral_Cover_Tran),
        `Av. (Transect)` = mean(Coral_Cover_Tran),
        `Median (Transect)` = median(Coral_Cover_Tran),
        `Max (Transect)` = max(Coral_Cover_Tran)
    )

# Calculate Fleshy Macroalgae Cover ---------------------------
df_benthic_cover_presence_fma <- df_benthic_cover_presence %>%
    group_by(Year, Site, Transect) %>%
    mutate(Algae_Cover_Tran = 100 * sum(Algae_Presence) / n()) %>%
    group_by(Year, Site) %>%
    mutate(Algae_Cover_Site = mean(Algae_Cover_Tran))
df_benthic_fma <- df_benthic_cover_presence_fma %>%
    group_by(Year) %>%
    summarize(
        `Min (Site)` = min(Algae_Cover_Site),
        `Av. (Site)` = mean(Algae_Cover_Site),
        `Median (Site)` = median(Algae_Cover_Site),
        `Max (Site)` = max(Algae_Cover_Site),
        `Min (Transect)` = min(Algae_Cover_Tran),
        `Av. (Transect)` = mean(Algae_Cover_Tran),
        `Median (Transect)` = median(Algae_Cover_Tran),
        `Max (Transect)` = max(Algae_Cover_Tran)
    ) %>%
    mutate(across(-Year, ~ round(.x, 2)))
df_benthic_fma_sites <- df_benthic_cover_presence_fma %>%
    group_by(Year, Site) %>%
    summarize(
        `Min (Transect)` = min(Algae_Cover_Tran),
        `Av. (Transect)` = mean(Algae_Cover_Tran),
        `Median (Transect)` = median(Algae_Cover_Tran),
        `Max (Transect)` = max(Algae_Cover_Tran)
    )

# Calculate Fish Biomass ---------------------------
df_fish_biomass_obs <- df_fish %>% # Observation-level
    select(Year, Site, Transect, Fish_Scientific, Size_Class, Observations) %>%
    left_join(ref_fish_species, by = "Fish_Scientific") %>%
    left_join(ref_biomass, by = c("Fish_Scientific" = "Binomial")) %>%
    mutate(Biomass_Observations = Observations * (LWRa * ((LWRconv * Size_Class)^LWRb))) %>%
    mutate(Biomass_Category = case_when(
        Family == "Acanthuridae" ~ "H",
        Family == "Scaridae" ~ "H",
        Family == "Epinephelidae" ~ "C",
        Family == "Lutjanidae" ~ "C"
    )) %>%
    filter(!is.na(Biomass_Category))
distinct_fish_transects <- distinct(select(df_fish, Year, Site, Transect))
distinct_biomass_categories <- c("C", "H")
df_fish_biomass_transect_incomplete <- df_fish_biomass_obs %>%
    group_by(Year, Site, Transect, Biomass_Category) %>%
    summarize(Biomass_Transects = sum(Biomass_Observations)) %>%
    ungroup()
df_fish_biomass_transect <- distinct_fish_transects %>% # Transect-level
    tidyr::crossing(Biomass_Category = distinct_biomass_categories) %>%
    left_join(
        df_fish_biomass_transect_incomplete,
        by = c("Year", "Site", "Transect", "Biomass_Category")
    ) %>%
    mutate(
        Biomass_Transects = tidyr::replace_na(Biomass_Transects, 0),
        Biomass_Transects_Density = 100 * Biomass_Transects / (2 * 30)
    )
df_fish_biomass_site <- df_fish_biomass_transect %>% # Transect-level
    group_by(Year, Site, Biomass_Category) %>%
    summarize(Biomass_Sites_Density = mean(Biomass_Transects_Density))
