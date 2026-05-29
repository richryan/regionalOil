file_prg <- "00-NAICS-documentation"

# Versions of NAICS from 2002, 2007, 2012, 2017, 2022

# 2002
dat_2002 <- read_fwf(here("doc", "naics_2_6_02.txt"), 
                     skip = 8,
                     col_types = "cc",
                     fwf_cols(
                       naics_code = c(1, 6),
                       title = c(9, NA)
                     )) |> 
  tidyr::drop_na(naics_code) |> 
  mutate(year = 2002)

dat_2002_look <- dat_2002 |> 
  filter(str_detect(naics_code, "^21")) |> 
  mutate(naics_code_len = str_length(naics_code)) |> 
  filter(naics_code_len <= 3)

# 2007
dat_2007 <- readxl::read_xls(here("doc", "naics07.xls")) |> 
  mutate(year = 2007)

dat_2007_look <- dat_2007 |> 
  rename(naics_code = `2007 NAICS US Code`,
         title = `2007 NAICS US Title`) |> 
  filter(`Seq. No.` > 1) |> 
  filter(str_detect(naics_code, "^21")) |> 
  mutate(naics_code_len = str_length(naics_code)) |> 
  filter(naics_code_len <= 3) |> 
  select(-`Seq. No.`)

# 2012
dat_2012 <- readxl::read_xls(here("doc", "2-digit_2012_Codes.xls")) |> 
  mutate(year = 2012)

dat_2012_look <- dat_2012 |> 
  rename(naics_code = `2012 NAICS US   Code`,
         title = `2012 NAICS US Title`) |> 
  filter(`Seq. No.` > 1) |> 
  filter(str_detect(naics_code, "^21")) |> 
  mutate(naics_code_len = str_length(naics_code)) |> 
  filter(naics_code_len <= 3) |> 
  select(-`Seq. No.`)

# 2017
dat_2017 <- readxl::read_xlsx(here("doc", "2-6 digit_2017_Codes.xlsx")) |> 
  mutate(year = 2017)

dat_2017_look <- dat_2017 |> 
  rename(naics_code = `2017 NAICS US   Code`,
         title = `2017 NAICS US Title`) |> 
  filter(`Seq. No.` > 1) |> 
  filter(str_detect(naics_code, "^21")) |> 
  mutate(naics_code_len = str_length(naics_code)) |> 
  filter(naics_code_len <= 3) |> 
  select(naics_code, title, naics_code_len, year)

# 2022
dat_2022 <- readxl::read_xlsx(here("doc", "2-6 digit_2022_Codes.xlsx")) |> 
  mutate(year = 2022)

dat_2022_look <- dat_2022 |> 
  rename(naics_code = `2022 NAICS US   Code`,
         title = `2022 NAICS US Title`) |> 
  filter(`Seq. No.` > 1) |> 
  filter(str_detect(naics_code, "^21")) |> 
  mutate(naics_code_len = str_length(naics_code)) |> 
  filter(naics_code_len <= 3) |> 
  select(naics_code, title, naics_code_len, year)

# All together
dat <- bind_rows(dat_2002_look,
                 dat_2007_look,
                 dat_2012_look,
                 dat_2017_look,
                 dat_2022_look)

dat <- dat |> 
  arrange(year, naics_code) |> 
  select(-naics_code_len) |> 
  write_csv(here("dta", "cln", paste0(file_prg, ".csv")))
