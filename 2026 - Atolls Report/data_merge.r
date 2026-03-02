# data_merge.r

## Merge Benthic PIM Data --------------------------------------
df_benthic_pim_l_restructured <- df_benthic_pim_l %>%
    mutate(
        Year = as.integer(Year), Date = as.character(Date), Locality = NA, Site = Site,
        Transect = as.integer(Transect), Protocol = "AGRRA", Start_Time = NA, Start_Depth = NA,
        End_Depth = NA, Temp = NA, Point = Point, Organism = Organism, Secondary = Organism.2,
        Algae_Height = Algae_Height, Bleaching = NA, ND_A = NA, Cloud_Cover = NA, Collector = NA,
        Notes = NA
    ) %>%
    select(-c(Zone, Organism.2, functional_group, category))
df_benthic_pim_merged <- bind_rows(mutate(df_benthic_pim_t, Atoll = "Turneffe"), mutate(df_benthic_pim_l_restructured, Atoll = "Lighthouse"))
df_benthic_pim <- df_benthic_pim_merged %>%
    filter(Year == 2025) %>%
    left_join(ref_organisms_benthic %>% select(Organism, Organism_Bucket = AGRRA_Bucket), by = "Organism") %>%
    left_join(ref_organisms_benthic %>% select(Organism, Secondary_Bucket = AGRRA_Bucket), by = c("Secondary" = "Organism"))

## Merge Coral Community Data --------------------------------------
df_coral_l_restructured <- df_coral_l %>%
    mutate(
        Year = as.integer(2025), Date = as.character(date), Site = site, Site = NA, site_comments = NA,
        Transect = as.integer(transect), Transect_Comments = NA, Area_Surveyed = NA, Protocol = "AGRRA",
        Start_Time = NA, End_Time = NA, Start_Depth = NA, End_Depth = NA, Temp = NA, Organism = organism,
        Isolates = isolates, Depth_Top = NA, Max_Diam = NA, Max_Length = max_length, Max_Width = max_width,
        Max_Height = max_height, OD = `old death`, TD = transition_death, RD = recent_death, Disease = disease,
        Other_Health_Concerns = other_health_concerns, Percent_Pale = percent_pale, Percent_Bleach = percent_bleach,
        Bleaching = bleaching, Base = base, Base_Coral = NA, Clump_L = clump_l, Clump_P = clump_p, Clump_BL = clump_bl,
        Clump_NM = clump_nm, Clump_TM = clump_tm, Clump_OM = clump_om, Clump_Other = NA, Clump_Interval = clump_interval,
        Collector = collector, Notes = notes
    ) %>%
    select(-isolate_type)
df_coral_merged <- bind_rows(mutate(df_coral_t, Atoll = "Turneffe"), mutate(df_coral_l_restructured, Atoll = "Lighthouse"))
df_coral <- df_coral_merged %>%
    filter(Year == 2025) %>%
    select(Atoll, Site, Transect, Organism)

## Merge Fish Data --------------------------------------
df_fish_l_restructured() <- df_fish_l %>%
    mutate(across(c(`2.5`, `7.5`, `15.5`, `25.5`, `35.5`, `>40`), as.integer)) %>%
    pivot_longer(cols = c(`2.5`, `7.5`, `15.5`, `25.5`, `35.5`, `>40`), names_to = "Size_Class", values_to = "Observations") %>%
    mutate(Observations = replace_na(Observations, 0)) %>%
    mutate(
        Year = as.integer(Year), Date = as.character(Date), Locality = NA, Site = Site, Transect = Transect,
        Protocol = "AGRRA", Start_Time = NA, Start_Depth = `Start Depth (ft)`, End_Depth = `End Depth (ft)`,
        Max_Relief = NA, Temp = `Temp (F)`, Fish = Species, Fish_Scientific = ScientificName,
        Cloud_Cover = NA, Sea_Condition = NA, Collector = NA, Notes = NA
    ) %>%
    select(-ID)
df_fish_merged <- bind_rows(mutate(df_fish_t, Atoll = "Turneffe", Size_Class = as.character(Size_Class)), mutate(df_fish_l_restructured, Atoll = "Lighthouse"))
df_fish <- df_fish_merged %>%
    filter(Year == 2025) %>%
    left_join(ref_fish_species, by = "Fish_Scientific") %>%
    left_join(ref_biomass, by = c("Fish_Scientific" = "Binomial"))
