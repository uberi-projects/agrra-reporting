# results_export.r

## Create report --------------------------------------
df_benthic_fma_sites_report <- df_benthic_fma_sites
df_benthic_lcc_sites_report <- df_benthic_lcc_sites
df_fish_biomass_H_site_report <- df_fish_biomass_site %>%
    filter(Biomass_Category == "H")
df_fish_biomass_C_site_report <- df_fish_biomass_site %>%
    filter(Biomass_Category == "C")
df_report <- df_benthic_fma_sites_report %>%
    left_join(df_benthic_lcc_sites_report, by = "Site") %>%
    left_join(df_fish_biomass_H_site_report, by = "Site") %>%
    left_join(df_fish_biomass_C_site_report, by = "Site") %>%
    ungroup() %>%
    select(Site, LCC_Mean = `Av. (Transect).y`, FMA_Mean = `Av. (Transect).x`, Herb_Biomass = Biomass_Sites_Density.x, Comm_Biomass = Biomass_Sites_Density.y)
df_report <- df_report %>%
    bind_rows(
        df_report %>%
            summarize(
                Site = "Overall",
                FMA_Mean = mean(FMA_Mean, na.rm = TRUE),
                LCC_Mean = mean(LCC_Mean, na.rm = TRUE),
                Herb_Biomass = mean(Herb_Biomass, na.rm = TRUE),
                Comm_Biomass = mean(Comm_Biomass, na.rm = TRUE)
            )
    )

## Save Report --------------------------------------
write.csv(df_report, "2026 - Atolls Report/outputs/report_2026.csv")
