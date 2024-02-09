analysis_years <- seq(analysis_years_start, analysis_years_end, by = 1)

flight_data <- import_iata_multi_nat(analysis_years, folder)

saveRDS(flight_data, "iata_data.rds")

# Note: this dataframe has the following 5 columns
# year "character"
# month "character"
# orig_country "character"
# dest_country "character"
# passengers "numeric"
