# results_export.r

## Create report --------------------------------------
df_fish_biomass_H_site <- df_fish_biomass_site %>%
    filter(Biomass_Category == "H")
df_fish_biomass_C_site <- df_fish_biomass_site %>%
    filter(Biomass_Category == "C")
df_report <- df_benthic_fma_sites %>%
    left_join(df_benthic_lcc_sites, by = c("Atoll", "Site")) %>%
    left_join(df_fish_biomass_H_site, by = c("Atoll", "Site")) %>%
    left_join(df_fish_biomass_C_site, by = c("Atoll", "Site")) %>%
    ungroup() %>%
    select(Atoll, Site,
        LCC_Mean = `Av. (Transect).y`,
        LCC_Median = `Median (Transect).y`,
        FMA_Mean = `Av. (Transect).x`,
        FMA_Median = `Median (Transect).x`,
        Herb_Biomass = Biomass_Sites_Density.x,
        Herb_Biomass_Median = Biomass_Sites_Density_Median.x,
        Herb_Biomass_Detection = Biomass_Sites_Detection_Rate.x,
        Comm_Biomass = Biomass_Sites_Density.y,
        Comm_Biomass_Median = Biomass_Sites_Density_Median.y,
        Comm_Biomass_Detection = Biomass_Sites_Detection_Rate.y
    )
df_report <- df_report %>%
    bind_rows(
        df_report %>%
            filter(Site != "Overall") %>%
            group_by(Atoll) %>%
            summarize(
                Site = "Overall",
                FMA_Mean = mean(FMA_Mean, na.rm = TRUE),
                FMA_Median = mean(FMA_Median, na.rm = TRUE),
                LCC_Mean = mean(LCC_Mean, na.rm = TRUE),
                LCC_Median = mean(LCC_Median, na.rm = TRUE),
                Herb_Biomass = mean(Herb_Biomass, na.rm = TRUE),
                Herb_Biomass_Median = mean(Herb_Biomass_Median, na.rm = TRUE),
                Herb_Biomass_Detection = mean(Herb_Biomass_Detection, na.rm = TRUE),
                Comm_Biomass = mean(Comm_Biomass, na.rm = TRUE),
                Comm_Biomass_Median = mean(Comm_Biomass_Median, na.rm = TRUE),
                Comm_Biomass_Detection = mean(Comm_Biomass_Detection, na.rm = TRUE),
            )
    )

## Round and Save Report --------------------------------------
df_report <- df_report %>%
    mutate(across(where(is.numeric), ~ round(.x, 1)))
write.csv(df_report, "2026 - Atolls Report/outputs/report_2026.csv")
