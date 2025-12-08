Sys.setlocale('LC_ALL','en_US.UTF-8')
packages <- c("haven", "writexl", "sf", "readr")
packages_to_install <- packages[!(packages %in% installed.packages()[,"Package"])]
if (length(packages_to_install) > 0) {
  install.packages(packages_to_install, dependencies = TRUE)
}
invisible(lapply(packages, library, character.only = TRUE))

# df <- read_sav("./data/Environmental_attitudes.sav")
# df_numeric <- zap_labels(df)
# write_csv(df_numeric, './data/Environmental_Attitudes.csv')

file_path <- "./data/NUTS/NUTS_RG_20M_2021_3035.shp"
map_data <- st_read(file_path)
print(map_data)
head(map_data)
names(map_data)
plot(st_geometry(map_data))