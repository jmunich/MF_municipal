library(tidyverse) # Data wrangling tools and the pipe operator (%>%)
library(readxl) # Reading excel files
library(xml2) # Joining municipality identifiers with their IČO codes requires reading .xml entires from the ARES database 
library(XML) # Read xml data with accounting unit identifiers
library(RCzechia) # Cool package for handling Czech maps and local unit identifiers

## The following code sets the system locale to UTF-8 in order to facilitate the work with czech special characters

if (.Platform$OS.type == "windows") {
  Sys.setlocale(category = "LC_ALL", "English_United States.1250")
} else {
  Sys.setlocale(category = "LC_ALL", "en_US.UTF-8")
}

## Fill in the locations of downoaded data

loc_data <- "data/MinFin/V2_12/" # Folder with "Fin 2-12 M - Plnění rozpočtu ÚSC" files from https://monitor.statnipokladna.cz/datovy-katalog/transakcni-data
loc_ucjed <- "data/MinFin/ucjed.xml" # XML file with accounting unit identifiers from https://monitor.statnipokladna.cz/datovy-katalog/ciselniky/prohlizec/7
loc_items <- "data/MinFin/Pols.xlsx" # Expenditure type identifier file (it is recommended to rename the downloaded file (here to "Pols.xlsx") due to special characters in the original name) https://monitor.statnipokladna.cz/datovy-katalog/ciselniky/prohlizec/43 
loc_paragraphs <- "data/MinFin/Paragraf.xlsx" # Entry type file (paragraf, it is recommended to rename the downloaded file (here to "Paragraf.xlsx") due to special characters in the original name) https://monitor.statnipokladna.cz/datovy-katalog/ciselniky/prohlizec/22
loc_citizens <- "data/CSU/Citizens/" # Files with municipal population sizes from https://www.mvcr.cz/clanek/informativni-pocty-obyvatel-v-obcich.aspx
loc_msmt <- "data/MSMT_min/" # Folder with data on pupil numbers in municipalities from https://www.mfcr.cz/cs/verejny-sektor/uzemni-rozpocty/prijmy-kraju-a-obci/financovani-materskych-a-zakladnich-skol
loc_cisob <- "data/CSU/CISOB/"  # Folder with municipality identifiers from http://apl.czso.cz/iSMS/cisinfo.jsp?kodcis=43

## Get municipality identifiers that will be used 

cosib_list <- list()
for(i in 1:length(list.files(loc_cisob))){
  for(j in 2:length(excel_sheets(paste0(loc_cisob, list.files(loc_cisob)[i])))){
    temp_sheet <- excel_sheets(paste0(loc_cisob, list.files(loc_cisob)[i]))[j] 
    cosib_list <- c(cosib_list, list(read_xlsx(paste0(loc_cisob, list.files(loc_cisob)[i]), skip = 1, sheet = temp_sheet) %>%
                                       mutate(time = temp_sheet) %>% `names<-`(c(
                                         "zuj",
                                         "obec",
                                         "status",
                                         "obec_kod",
                                         "obec_u",
                                         "ORP_kod",
                                         "ORP",
                                         "Okres_kod",
                                         "Okres",
                                         "Kraj_kod",
                                         "Kraj",
                                         "Region_kod",
                                         "Region",
                                         "time"
                                       ))))
  }
}

all_ids <- lapply(cosib_list, function(x){x %>% select(time, obec, obec_u, ORP_kod, zuj, obec_kod)}) %>%
  bind_rows() %>%
  mutate(time = paste0("r_", substr(time, 5,9))) %>%
  pivot_wider(id_cols = c(obec, obec_u, ORP_kod), names_from = time, values_from = c(zuj, obec_kod), values_fill = list(zuj = NA, obec_kod = NA))

# Find repetitions within ORPs

temp_ids <- all_ids[,-c(1:3)] %>%
  mutate_all(lengths) %>%
  rowSums()

all_ids_2 <- all_ids[which(temp_ids > 16), -c(1:3)] %>%
  lapply(unlist) %>%
  bind_cols(all_ids[rep(which(temp_ids > 16),2), 1:3]) 

# Create an identifier tibble with municipal id per year

key_bind <- lapply(cosib_list, function(x){x %>% select(time, obec, obec_u, ORP_kod, zuj, obec_kod)}) %>%
  bind_rows() %>%
  mutate(time = paste0("r_", substr(time, 5,9))) %>%
  filter(!(obec %in% all_ids_2$obec & ORP_kod %in% all_ids_2$ORP_kod)) %>%
  pivot_wider(id_cols = c(obec, obec_u, ORP_kod), names_from = time, values_from = c(zuj, obec_kod), values_fill = list(zuj = NA, obec_kod = NA)) %>%
  bind_rows(all_ids_2) %>%
  mutate(my_id = 1:n()) %>%
  pivot_longer(cols = -c(obec, obec_u, ORP_kod, my_id)) %>%
  mutate(year = gsub(".+20", "", name) %>% ifelse(.=="","20",.),
         name = ifelse(grepl("zuj", name),"zuj", "obec_kod")) %>%
  pivot_wider(id_cols = c(my_id, year, obec, obec_u, ORP_kod), names_from = name, values_from = value, values_fill = list(value = NA))

## Read and select filenames for spending data

inds <- list.files(loc_data) %>%
  enframe() %>%
  filter(grepl("FINM201_",value))

# Read data from all files and create a joint data file using "Fin 2-12 M - Plnění rozpočtu ÚSC" files from https://monitor.statnipokladna.cz/datovy-katalog/transakcni-data

data_f_list <- list()
for(i in 1:nrow(inds)){
  data_f_list[[i]] <- read_csv2(paste0(loc_data, inds[i,2])) %>%
    mutate(file = unlist(inds[i,2]))
}

data_f_all <- bind_rows(data_f_list) %>%
  mutate_at(vars(starts_with("ZU_")), function(x){gsub("\\..*|-","",x)}) %>%
  `names<-`(gsub(":.+", "", names(data_f_list[[1]]))) %>%
  mutate(year = as.character(substr(file, 11, 12)),
         month = ordered(substr(file, 14, 15)))

# Read accounting unit identifiers from https://monitor.statnipokladna.cz/datovy-katalog/ciselniky/prohlizec/7

data_id <- xmlParse(loc_ucjed) %>%
  xmlRoot() %>% 
  xmlToDataFrame() %>%
  as_tibble()

# Join spending data with accounting unit identifiers

data_obce <- data_f_all %>%
  left_join(data_id %>%
              select(ico, obec, zuj, druhuj_id) %>%
              filter(obec != "") %>%
              distinct() %>%
              mutate_all(as.character), 
            by = c("ZC_ICO" = "ico")) %>%
  filter(druhuj_id == "4") # Select local municipalities https://monitor.statnipokladna.cz/datovy-katalog/ciselniky/prohlizec/9

fills <- data_obce %>%
  left_join(cosib_list %>%
              bind_rows() %>%
              select(zuj, obec, Okres_kod) %>%
              mutate(zuj_2 = zuj) %>%
              select(-zuj) %>%
              distinct() %>%
              filter(obec %in% data_obce$obec[data_obce$zuj == ""] & Okres_kod %in% data_obce$ZC_NUTS[data_obce$zuj == ""]),
            by = c("obec" = "obec",
                   "ZC_NUTS" = "Okres_kod")
  ) %>%
  mutate(zuj = ifelse(zuj == "", zuj_2, zuj)) %>%
  select(-zuj_2)

data_obce <- fills

# Read accounting item type and create an expenditure variable https://monitor.statnipokladna.cz/datovy-katalog/ciselniky/prohlizec/43 

items <- read_excel(loc_items)

items_code_expenditure <- items %>%
  select(Položka, Druh, `Třída`) %>%
  filter(Druh == "Výdaje")

# Select education-related entries (paragraf) https://monitor.statnipokladna.cz/datovy-katalog/ciselniky/prohlizec/22

paragraphs <- read_excel(loc_paragraphs)

paragraphs_code_education <- paragraphs %>%
  filter(Skupina == 3)

paragraphs_code_education_elementary <- paragraphs %>%
  filter(Pododdíl == 311)

# Get municipal population sizes from https://www.mvcr.cz/clanek/informativni-pocty-obyvatel-v-obcich.aspx

inds_2 <- list.files(loc_citizens) %>%
  enframe()

data_c_list <- list()
for(i in 1:nrow(inds_2)){
  data_c_list[[i]] <- read_xls(paste0(loc_citizens, inds_2[i,2]), skip = 5) %>%
    `names<-`(c("Kraj", "ORP", "zuj", "Obec", "M", "M15", "F", "F15", "T", "T15")) %>%
    mutate(year = substr(inds_2[i,2], 5, 6))
}

data_c_all <- bind_rows(data_c_list)

# Data on pupil numbers in municipalities from https://www.mfcr.cz/cs/verejny-sektor/uzemni-rozpocty/prijmy-kraju-a-obci/financovani-materskych-a-zakladnich-skol

data_msmt <- lapply(list.files(loc_msmt), function(x){read_xlsx(paste0(loc_msmt, x), skip = 2) %>%
    mutate(file = x)})

data_msmt_2 <- lapply(data_msmt[1:5], function(x){`names<-`(x, names(data_msmt[[5]]))})
data_msmt_2[[6]] <- data_msmt[[6]] %>%
  `names<-`(names(data_msmt[[5]])[-16])

data_msmt_2 <- lapply(data_msmt_2, function(x){x %>% mutate_all(as.character)})

data_msmt_zaci <- bind_rows(data_msmt_2) %>%
  mutate(year = parse_number(file)) %>%
  select(-file) %>%
  mutate(`Kod\nLAU 2\nzřizovatele` = ifelse(is.na(`Kod\nLAU 2\nstat. mesto`),
                                            `Kod\nLAU 2\nzřizovatele`,
                                            `Kod\nLAU 2\nstat. mesto`),
         `Název LAU 2\nzrizovatele` = ifelse(is.na(`Kod\nLAU 2\nstat. mesto`),
                                             `Název LAU 2\nzrizovatele`,
                                             `Název LAU 2\nstat. mesto`),
         `Kod\nLAU 2\nzřizovatele` = ifelse(`Kod\nLAU 2\nzřizovatele` == "587427",
                                            "500135",
                                            `Kod\nLAU 2\nzřizovatele`))


data_msmt_zaci_ag_mz <- data_msmt_zaci %>%
  select(year, `Kod\nLAU 2\nzřizovatele`,`Název LAU 2\nzrizovatele`, `Kod\nNUTS 3` ,`Děti v MŠ celkem`, `Žáci v ZŠ celkem`) %>%
  `names<-`(c("year", "Obec_kod", "Obec_nazev", "NUTS3", "ms", "zs")) %>%
  mutate(Obec_kod = ifelse(NUTS3 == "CZ010", "554782", Obec_kod)) %>%
  select(-NUTS3) %>%
  pivot_longer(-c(year, Obec_kod, Obec_nazev), names_to = "skola", values_to = "zaci") %>%
  group_by(year, Obec_kod, Obec_nazev, skola) %>%
  summarise(zaci = sum(as.numeric(zaci))) %>%
  ungroup() %>%
  mutate(year = as.character(year))

data_msmt_zaci_ag <- data_msmt_zaci %>%
  select(year, `Kod\nLAU 2\nzřizovatele`,`Název LAU 2\nzrizovatele`, `Kod\nNUTS 3` ,`Děti v MŠ celkem`, `Žáci v ZŠ celkem`) %>%
  `names<-`(c("year", "Obec_kod", "Obec_nazev", "NUTS3", "ms", "zs")) %>%
  mutate(Obec_kod = ifelse(NUTS3 == "CZ010", "554782", Obec_kod)) %>%
  select(-NUTS3) %>%
  pivot_longer(-c(year, Obec_kod, Obec_nazev), names_to = "skola", values_to = "zaci") %>%
  group_by(year, Obec_kod, Obec_nazev) %>%
  summarise(zaci = sum(as.numeric(zaci))) %>%
  ungroup() %>%
  mutate(year = as.character(year)) %>%
  filter(!is.na(Obec_kod))

data_msmt_zaci_ag <- data_msmt_zaci %>%
  select(year, `Kod\nLAU 2\nzřizovatele`,`Název LAU 2\nzrizovatele`, `Kod\nNUTS 3` ,`Děti v MŠ celkem`, `Žáci v ZŠ celkem`) %>%
  `names<-`(c("year", "Obec_kod", "Obec_nazev", "NUTS3", "ms", "zs")) %>%
  mutate(Obec_kod = ifelse(NUTS3 == "CZ010", "554782", Obec_kod)) %>%
  pivot_longer(-c(year, Obec_kod, Obec_nazev, NUTS3), names_to = "skola", values_to = "zaci") %>%
  group_by(year, Obec_kod, Obec_nazev, NUTS3) %>%
  summarise(zaci = sum(as.numeric(zaci))) %>%
  ungroup() %>%
  group_by(Obec_nazev, NUTS3) %>%
  mutate(year = as.character(year)) %>%
  filter(!is.na(Obec_kod))

# Get municipality polygons for maps

map_municipalities <- obce_polygony()

# Get aggregation on the level of municipality and join with the remaining data, summary_municipal contains one column with summarised ammounts and a variable indicating expense/income, summary_municipal_paragraph contains paragraphs per columns with prefix indicating expense/income

summary_municipal <- data_obce %>%
  filter(month == 12) %>%
  select(year, zuj, `0FUNC_AREA`, ZCMMT_ITM, ZU_ROZKZ) %>%
  mutate(education_el = as.numeric(`0FUNC_AREA` %in% paragraphs_code_education_elementary$Paragraf),
         expenditure = ifelse(ZCMMT_ITM %in% items_code_expenditure$Položka, 1, -1)) %>%
  group_by(year, zuj, education_el, expenditure) %>%
  summarise(balance = sum(as.numeric(ZU_ROZKZ)))

summary_municipal_paragraph <- data_obce %>%
  filter(month == 12) %>%
  select(year, zuj, `0FUNC_AREA`, ZCMMT_ITM, ZU_ROZKZ) %>%
  mutate(ZCMMT_ITM = paste0(ifelse(ZCMMT_ITM %in% items_code_expenditure$Položka, "e_", "i_"), ZCMMT_ITM)) %>%
  group_by(year, zuj, ZCMMT_ITM) %>%
  summarise(ZU_ROZKZ = sum(as.numeric(ZU_ROZKZ))) %>%
  pivot_wider(id_cols = c(year, zuj), values_from = ZU_ROZKZ, names_from = ZCMMT_ITM, values_fill = list(ZU_ROZKZ = 0))

# Join all the data

data_complete <- key_bind %>%
  ungroup() %>%
  left_join(data_c_all %>%
              select(-Obec) %>%
              mutate(zuj = as.character(zuj)) %>%
              ungroup(),
            by = c("year", "zuj")) %>%
  left_join(data_msmt_zaci_ag %>%
              select(-c(Obec_nazev, NUTS3)) %>%
              ungroup(),
            by = c("year", "zuj" = "Obec_kod")) %>%
  left_join(summary_municipal_paragraph %>%
              ungroup(),
            by = c("year", "zuj"))
  

data_cl <- data_complete %>%
  filter(!year %in% c("13", "20"))

write_csv(data_cl, "obce_data.csv")
