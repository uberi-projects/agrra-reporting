# data_merge.r

## Merge Benthic PIM Data --------------------------------------
df_benthic_pim <- df_benthic_pim_t %>%
    filter(Year %in% c(2023, 2025)) %>%
    left_join(ref_organisms_benthic %>% select(Organism, Organism_Bucket = AGRRA_Bucket), by = "Organism") %>%
    left_join(ref_organisms_benthic %>% select(Organism, Secondary_Bucket = AGRRA_Bucket), by = c("Secondary" = "Organism")) %>%
    select(Year, Site, Transect, Point, Organism, Secondary, Organism_Bucket, Secondary_Bucket)

## Merge Coral Community Data --------------------------------------
df_coral <- df_coral_t %>%
    filter(Year %in% c(2021, 2025)) %>%
    select(Year, Site, Transect, Organism)

## Merge Fish Data --------------------------------------
df_fish_class <- mutate(df_fish_t, Size_Class = as.character(Size_Class))
df_fish <- df_fish_class %>%
    filter(Year %in% c(2023, 2025)) %>%
    left_join(ref_fish_species, by = "Fish") %>%
    left_join(ref_biomass, by = c("Fish_Scientific.y" = "Binomial")) %>%
    mutate(Size_Class = as.numeric(Size_Class)) %>%
    select(Year, Site, Transect, Fish_Family, Size_Class, Observations, LWRa, LWRb, LWRconv)

## Merge Recruits Data --------------------------------------
df_recruits <- df_recruits_t %>%
    filter(Year %in% c(2023, 2025)) %>%
    mutate(Num = replace_na(Num, 0)) %>%
    select(Year, Site, Transect, Quadrat, Organism, Num, Size)
