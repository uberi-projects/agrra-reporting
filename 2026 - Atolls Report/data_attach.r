# data_attach.r

## Define NA Strings --------------------------------------
na_strings <- c("NA", "", "MISSING", "UNKNOWN")

## Retrieve Turneffe AGRRA data from Dryad repository --------------------------------------
if (dir.exists("2026 - Atolls Report/data_deposit") && length(list.files("2026 - Atolls Report/data_deposit")) > 0) {
    message("Data files already present in 2026 - Atolls Report/data_deposit folder")
} else {
    message("Downloading data from Dryad...")
    downloaded_files <- dryad_download(dois = "10.5061/dryad.c866t1gcn")
    file.copy(unlist(downloaded_files), "2026 - Atolls Report/data_deposit", overwrite = TRUE)
    message("Files copied to 2026 - Atolls Report/data_deposit/")
}

## Attach Turneffe AGRRA data --------------------------------------
df_benthic_pim_t <- read.csv("2026 - Atolls Report/data_deposit/Master_Benthic_PIM_2010-2025.csv", na.strings = na_strings)
df_coral_t <- read.csv("2026 - Atolls Report/data_deposit/Master_Coral_Community_2010-2025.csv", na.strings = na_strings)
df_fish_t <- read.csv("2026 - Atolls Report/data_deposit/Master_Fish_Survey_2010-2025.csv", na.strings = na_strings)

## Attach reference data --------------------------------------
ref_biomass <- read.csv("2026 - Atolls Report/data_provided/fish_biomass_specification.csv") %>%
    mutate(
        Family = word(Name, 1),
        Binomial = ifelse(str_detect(Name, " "), word(Name, 2, 3), NA_character_)
    ) %>%
    na.omit()
ref_fish_species <- read.csv("2026 - Atolls Report/data_deposit/Ref_Fish_Species.csv", na.strings = na_strings)
ref_organisms_benthic <- read.csv("2026 - Atolls Report/data_deposit/Ref_Organisms_Benthic.csv", na.strings = na_strings)
