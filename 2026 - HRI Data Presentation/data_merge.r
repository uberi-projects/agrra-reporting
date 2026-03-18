# data_merge.r

## Merge Benthic PIM Data --------------------------------------
df_benthic_pim_merged <- df_benthic_pim_t
df_benthic_pim <- df_benthic_pim_merged %>%
    filter(Year %in% c(2023, 2025)) %>%
    left_join(ref_organisms_benthic %>% select(Organism, Organism_Bucket = AGRRA_Bucket), by = "Organism") %>%
    left_join(ref_organisms_benthic %>% select(Organism, Secondary_Bucket = AGRRA_Bucket), by = c("Secondary" = "Organism")) %>%
    select(Year, Site, Transect, Point, Organism, Secondary, Organism_Bucket, Secondary_Bucket)

## Merge Coral Community Data --------------------------------------
df_coral_merged <- df_coral_t
df_coral <- df_coral_merged %>%
    filter(Year %in% c(2023, 2025)) %>%
    select(Year, Site, Transect, Organism)

## Merge Fish Data --------------------------------------
df_fish_merged <- mutate(df_fish_t, Size_Class = as.character(Size_Class))
df_fish <- df_fish_merged %>%
    filter(Year %in% c(2023, 2025)) %>%
    left_join(ref_fish_species, by = "Fish") %>%
    left_join(ref_biomass, by = c("Fish_Scientific.y" = "Binomial")) %>%
    mutate(Size_Class = as.numeric(Size_Class)) %>%
    select(Year, Site, Transect, Fish_Family, Size_Class, Observations, LWRa, LWRb, LWRconv)
