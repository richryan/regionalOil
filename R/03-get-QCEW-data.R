file_prg <- "03-get-QCEW-data"

csub_blue <- rgb(0, 53, 148, maxColorValue = 255)


# Data --------------------------------------------------------------------
# See here: https://www.bls.gov/cew/downloadable-data-files.htm

# By Area, Quarterly data:
# files look like
# https://data.bls.gov/cew/data/files/2022/csv/2022_qtrly_by_area.zip
# https://data.bls.gov/cew/data/files/2021/csv/2021_qtrly_by_area.zip
# ...
# https://data.bls.gov/cew/data/files/2014/csv/2014_qtrly_by_area.zip
# ...
# https://data.bls.gov/cew/data/files/1990/csv/1990_qtrly_by_area.zip

# Download the files
yrs <- seq(1990, 2024, by = 1)
fout <- here("dta", "src")
for (ii in yrs) {
  fzip_path <- paste0("https://data.bls.gov/cew/data/files/", ii, "/csv/")
  fzip_file <- paste0(ii, "_qtrly_by_area.zip")
  fzip_out <- here("dta", "src", fzip_file)
  fzip <- paste0(fzip_path, fzip_file)
  print(paste("Downloading file", fzip))
  
  # Download the file
  download.file(fzip, destfil = fzip_out)
  
  # Extract files from Zip
  fpath_year_qtr <- paste0(ii, ".q1-q4")
  fpath_by_area <- ".by_area"
  my_counties <- c(" 06029 Kern County, California.csv",
                   " 38053 McKenzie County, North Dakota.csv",
                   " 48255 Karnes County, Texas.csv",
                   " 35015 Eddy County, New Mexico.csv",
                   " 08123 Weld County, Colorado.csv")
  fpaths <- vector(mode = "double")
  for (cc in my_counties) {
    cc_fpath <- paste0(fpath_year_qtr, cc)
    cc_fpath <- paste0(fpath_year_qtr, fpath_by_area, "/", cc_fpath)
    fpaths <- c(fpaths, cc_fpath)
  }
  
  unzip(fzip_out, exdir = fout, files = fpaths)
}


