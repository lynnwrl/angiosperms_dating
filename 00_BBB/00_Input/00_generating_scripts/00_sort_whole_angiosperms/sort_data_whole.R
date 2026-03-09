# ---------------------------------------------------------------------
# This script converts the original angiosperm fossil table
# (generated using the two scripts provided by Silvestro et al. 2021)
# into a cleaned table that can be used to generate the final input file.
#
# For order-level analyses:
#   - Families are treated as the analytical units rather than species.
#   - If multiple records share the same family, reference, minage,
#     and maxage in the original table, they are counted as a single
#     fossil occurrence.
#
# For analyses including all angiosperms:
#   - Orders are treated as the analytical units.
#   - Two alternative strategies are implemented for handling unnamed taxa:
#
#     (1) Conservative option:
#         All unnamed families/orders are grouped together
#         (e.g., "unnamed_fm", "unnamed_ord").
#
#     (2) Maximum differentiation option:
#         Each unnamed taxon is assigned to a distinct order
#         (e.g., "unnamed_ord_1", "unnamed_ord_2", ...).
#
# Written: 15–21 February 2024
# ---------------------------------------------------------------------

# clean up the workspace 
rm(list=ls()) 

# package installing 
library(readxl)
library(dplyr)
library(openxlsx)
library(writexl)
library(tidyr)
library(stringr)

###################
# 1. preparations #
###################

# I manually change some of the column names of the original table 
# like adding '_' for space 
# or deleted '-' and change names accordingly 

# those without changing any of the record itself didn't show in the script below 


#################
# 2. load files #
#################

# loading the original data file 
path_Ceno_table = "./CenozoicData_updated.xlsx" 
Ceno_table_original <- read_excel(path_Ceno_table) # 24269 records

path_Cret_table = "./CretaceousData.xlsx" 
Cret_table_original <- read_excel(path_Cret_table) # 1416 records

# add source 
Ceno_table_original$source <- "Cenozoic"
Cret_table_original$source <- "Cretaceous"

# select some columns and combine them 
  # 
select_Ceno_table <- Ceno_table_original %>% 
  select( source, ID, Formation, Age_min, Age_max, Specific_epithet, Genspec, Family, RefLink)
select_Cret_table <- Cret_table_original %>% 
  select( source, ID, Formation, Age_min, Age_max, Specific_epithet, Genspec, Family, RefLink)
  # combine two df
combined_table <- rbind(select_Ceno_table, select_Cret_table) # 25685 records
  # 
rm( path_Ceno_table, path_Cret_table, Ceno_table_original, Cret_table_original, select_Ceno_table, select_Cret_table)


#################
# 3. clear ages #
#################

# 1 #
# list those without both age
missing_both_age <- is.na(combined_table$Age_min) & is.na(combined_table$Age_max)
# 
number_missing_both_age <- sum(missing_both_age) # 29 records
# df of those missing records
df_missing_both_age <- combined_table[missing_both_age, ]
# delete them from original table
combined_table <- combined_table %>%
  filter(!(is.na(Age_min) & is.na(Age_max))) # 25656 records
#
rm( missing_both_age, number_missing_both_age)

# 2 #
# list those without max age
missing_max_age <- is.na(combined_table$Age_max)
# 
number_missing_max_age <- sum(missing_max_age) # 819 records
# df of those missing records
df_missing_max_age <- combined_table[missing_max_age, ]
#sum(is.na(combined_table$Age_min)) 
# set the max equals to min, so that they have values 
combined_table$Age_max[is.na(combined_table$Age_max)] <- combined_table$Age_min[is.na(combined_table$Age_max)] # still 25656 records
#sum(is.na(combined_table$Age_max)) 
#
rm( missing_max_age, number_missing_max_age)

# 3 #
# list those without min age
missing_min_age <- is.na(combined_table$Age_min)
# 
number_missing_min_age <- sum(missing_min_age) #  0 records
# because this step equals to zero so no action needed 
#
rm( missing_min_age, number_missing_min_age)

# 4 #
# list those max age smaller than min age 
df_max_smaller_than_min <- combined_table[combined_table$Age_max < combined_table$Age_min, ] # 37 records
# change the min age and max age so that the max would be larger than min
  # find those need change 
indices_to_swap <- which(combined_table$Age_max < combined_table$Age_min)
  # swap values 
temp <- combined_table$Age_min[indices_to_swap] # 
combined_table$Age_min[indices_to_swap] <- combined_table$Age_max[indices_to_swap]
combined_table$Age_max[indices_to_swap] <- temp
#sum(combined_table$Age_max < combined_table$Age_min) # 
#
rm( indices_to_swap, temp)

############################
# 4. filter out age range #
###########################

# step 1: find those have time range more than 20 MA
age_diff_gt_20 <- (combined_table$Age_max - combined_table$Age_min) > 20

# step 2: create a df to save them 
df_age_diff_gt_20 <- combined_table[age_diff_gt_20, ] # 1255 records
#
rm(age_diff_gt_20)

# step 3: narrowing down those fossils with over 20 MA range
# if could, change age, if not, delete them
# only focus on those in Cretaceous # 58 records

# save for check
write.xlsx(df_age_diff_gt_20, file = "df_age_diff_gt_20.xlsx") # 

### Yixian Formation ###
# from Wikipedia it's Barremian–Aptian age
# according to https://stratigraphy.org/ICSchart/ChronostratChart2023-09.pdf it's 121.4-125.8 (round up)
df_age_revised <- df_age_diff_gt_20 %>%
  mutate(Age_min = if_else(Formation == "Yixian Formation", 121.4, Age_min),
         Age_max = if_else(Formation == "Yixian Formation", 125.8, Age_max))

### Patuxent Formation and Alundel Formation ###
# from Wikipedia those two are Aptian age
# check and compare current age (current in the dataset: 100.5-125) (ChronostratChart2023-09 for Aptian: 113.0-121.4)
df_age_revised <- df_age_revised %>%
  mutate(Age_min = if_else(Formation == "Patuxent Formation and Alundel Formation", 113.0, Age_min),
         Age_max = if_else(Formation == "Patuxent Formation and Alundel Formation", 121.4, Age_max))

### Crato Formation ###
# from Wikipedia it's from 113-115 MA
df_age_revised <- df_age_revised %>%
  mutate(Age_min = if_else(Formation == "Crato Formation", 113.0, Age_min),
         Age_max = if_else(Formation == "Crato Formation", 115.0, Age_max))

### Kachaike Formation ###
# https://www.sciencedirect.com/science/article/pii/S0034666722001981?via%3Dihub
# the paper here listed age of this formation as 'upper Aptian–lower Cenomanian'
# and the most constarined one is 'Albian' (100.5-113, according to ChronostratChart2023-09)
df_age_revised <- df_age_revised %>%
  mutate(Age_min = if_else(Formation == "Kachaike Formation", 100.5, Age_min),
         Age_max = if_else(Formation == "Kachaike Formation", 113.0, Age_max))

### Dalazi Formation ###
# from paper in 2021 (https://doi.org/10.1016/j.cretres.2021.104977) it's 103.3 - 104.6 (round up)
df_age_revised <- df_age_revised %>%
  mutate(Age_min = if_else(Formation == "Dalazi Formation", 103.3, Age_min),
         Age_max = if_else(Formation == "Dalazi Formation", 104.6, Age_max))

### Vermejo Formation ###
# from Wikipedia it's from Maastrichtian 
# according to ChronostratChart2023-09 it's 66-72.1
df_age_revised <- df_age_revised %>%
  mutate(Age_min = if_else(Formation == "Vermejo Formation", 66.0, Age_min),
         Age_max = if_else(Formation == "Vermejo Formation", 72.1, Age_max))

### Hell Creek Formation ###
# from Wikipedia it's 66-68 MA
df_age_revised <- df_age_revised %>%
  mutate(Age_min = if_else(Formation == "Hell Creek Formation", 66.0, Age_min),
         Age_max = if_else(Formation == "Hell Creek Formation", 68.0, Age_max))

### any other ? if so can add above, and record number below will change accordingly ###

# step 4: adapted those revised into original df 
# delete data still over 20 MA range after revision 
df_age_revised <- df_age_revised %>%
  filter((Age_max - Age_min) <= 20)

# renew the combined_table 
# first, delete those over 20 MA using both ID and source as keys 
combined_table <- combined_table %>%
  anti_join(df_age_diff_gt_20, by = c("ID", "source")) # 24401 records

# 
combined_table <- rbind(combined_table, df_age_revised) # 24429 records

# make sure no duplicate, ie. the number of records stand same as above 
#combined_table <- combined_table %>%
#  distinct()


# create table for the counts of species per time bin 
max(combined_table$Age_max) # 139.8


### Age clearance (inlcuding block 3 & 4) should be finished till here ###


####################
# 5. check family #
###################

### step 1 ###
### MANUALLY CORRECT THE ENDCODING ISSUES ###
### re-formatting encoding issues ###

# Check if any using Chinese quoting marks as name's beginning 
# 
###  “” ###
indices1 <- grep("^“", combined_table$Genspec) # 
df1_quote_mark <- combined_table[indices1, ]
###  ‘’ ###
indices2 <- grep("^‘", combined_table$Genspec) # 
df2_quote_mark <- combined_table[indices2, ]
# combine two df
df_quote_mark <- rbind(df1_quote_mark,df2_quote_mark)
# revise quote mark
df_quote_mark_revised <- df_quote_mark %>%
  mutate(
    Genspec = case_when(
      source == "Cretaceous" & ID == 2   ~ '"Cocculus" extinctus', # “Cocculus” extinctus
      source == "Cretaceous" & ID == 27  ~ '"Araliaephyllum" dentatum', # “Araliaephyllum” dentatum
      source == "Cretaceous" & ID == 20  ~ '"Diospyros" aff. Steenstrupi', # “Diospyros” aff. Steenstrupi
      source == "Cretaceous" & ID == 975 ~ '"Dryandroides" quercinea', # “Dryandroides” quercinea
      source == "Cretaceous" & ID == 80  ~ '"Fagophyllum" sp.', # “Fagophyllum” sp.
      source == "Cretaceous" & ID == 16  ~ '"Protophyllum" terechovae', # “Protophyllum” terechovae
      source == "Cretaceous" & ID == 157 ~ '"Quercus" viburnifolia', # “Quercus” viburnifolia
      source == "Cretaceous" & ID == 168 ~ '"Vitis" ex gr. Populoudes', # “Vitis” ex gr. Populoudes
      source == "Cretaceous" & ID == 166 ~ '"Vitis" penzhinica', # “Vitis” penzhinica
      source == "Cretaceous" & ID == 169 ~ '"Vitis" sp.', # “Vitis” sp.
      source == "Cretaceous" & ID == 167 ~ '"Vitis" stantoni', # “Vitis” stantoni
      source == "Cretaceous" & ID == 162 ~ '"Zizyphus" kujiensis', # “Zizyphus” kujiensis
      source == "Cretaceous" & ID == 78  ~ '"Zizyphus" smilacifolia', # “Zizyphus” smilacifolia
      source == "Cretaceous" & ID == 114 ~ '"Zizyphus" smilacifolia', # “Zizyphus” smilacifolia
      source == "Cretaceous" & ID == 163 ~ '"Zizyphus" smilacifolia', # “Zizyphus” smilacifolia
      source == "Cretaceous" & ID == 37  ~ '"Zizyphus" sp.', # “Zizyphus” sp.
      source == "Cretaceous" & ID == 79  ~ '"Zizyphus" sp.', # “Zizyphus” sp.
      source == "Cretaceous" & ID == 117 ~ '"Zizyphus" sp.', # “Zizyphus” sp.
      
      source == "Cretaceous" & ID == 948 ~ '"Araliaephyllum" dentatum', # ‘Araliaephyllum’ dentatum
      source == "Cretaceous" & ID == 949 ~ '"Araliaephyllum" medium', # ‘Araliaephyllum’ medium
      source == "Cretaceous" & ID == 954 ~ '"Zizyphus" sp.', # ‘Zizyphus’ sp.
      
      TRUE ~ Genspec # if Order not match any above, keep their original Clade_1 
      )
    )

# delete df_quote_mark from combined_table and rbind df_quote_mark_revised back
# use anti_join() function and both ID & source as keys 
combined_table <- anti_join(combined_table, df_quote_mark, by = c("ID", "source")) # from 24429 to 24408 records
# rbind revised back 
combined_table <- rbind(combined_table, df_quote_mark_revised) %>% 
                arrange(ID, source) # 24429 records again 
#
rm( df1_quote_mark, df2_quote_mark, df_quote_mark_revised, indices1, indices2)

### step 2 ###
# since family would be the unit of order, so those without family name couldn't be used
# find those without family name
missing_fm <- is.na(combined_table$Family)
# 
number_missing_fm <- sum(missing_fm) # 1311 records
# 
df_missing_fm <- combined_table[missing_fm,]



# 
df_unique_Gen <- df_missing_fm %>%
  distinct(Genspec, .keep_all = TRUE) # 888 different Genspec.


# 
unique_first_fields <- df_missing_fm %>%
  mutate(Genera = str_extract(Genspec, "^[^\\s]+")) %>%
  distinct(Genera) %>%
  pull(Genera)

# 
df_genera_family <- data.frame(
                    Genera = unique_first_fields,
                    Family = rep(NA, length(unique_first_fields)),
                    stringsAsFactors = FALSE # 
                    ) # 315 different Gen.

# save for edit
write.xlsx(df_genera_family, file = "df_genera_family.xlsx") # 

# revised Family according to Genera
df_genera_family_revised <- df_genera_family %>% 
  mutate(Genera = ifelse(is.na(Genera), 'NA', Genera)) # if NA then fill the letter 'NA', avoid blanks.

df_genera_family_revised <- df_genera_family_revised %>%
  mutate(Family = case_when(
    
    Genera == '"Araliaephyllum"' ~ 'Araliaceae', # http://www.fossilworks.org/cgi-bin/bridge.pl?a=taxonInfo&taxon_no=277374#
    Genera == '"Diospyros"' ~ 'Ebenaceae', # https://en.wikipedia.org/wiki/Diospyros
    Genera == '"Fagophyllum"' ~ 'Fagaceae', # https://www.mindat.org/taxon-4895534.html
    Genera == '"Protophyllum"' ~ 'Platanaceae', # https://www.mindat.org/taxon-8277371.html
    Genera == '"Quercus"' ~ 'Fagaceae', # https://en.wikipedia.org/wiki/Oak
    Genera == '"Vitis"' ~ 'Vitaceae', # https://en.wikipedia.org/wiki/Vitis
    Genera == '"Zizyphus"' ~ 'Rhamnaceae', # typo, Ziziphus? https://en.wikipedia.org/wiki/Ziziphus
    
    Genera == '?Knightia' ~ 'Proteaceae', # https://en.wikipedia.org/wiki/Knightia_(plant)
    Genera == 'Araliaephyllum?' ~ 'Araliaceae', # http://www.fossilworks.org/cgi-bin/bridge.pl?a=taxonInfo&taxon_no=277374
    Genera == 'Laurophyllum?' ~ 'Lauraceae', # https://www.mindat.org/taxon-4896377.html
    
    Genera == 'Acaciaephyllum' ~ 'Fabaceae',  # https://www.gbif.org/species/4890689
    Genera == 'Actinocalyx' ~ 'Diapensiaceae', # https://www.sciencedirect.com/science/article/abs/pii/0034666785900016
    Genera == 'Alismaphyllum' ~ 'Alismataceae', # https://tr.wikipedia.org/wiki/Alismaphyllum
    Genera == 'Apocynophyllum' ~ 'Lythraceae', # https://www.researchgate.net/publication/266880617_A_noteworthy_plant_taphocoenosis_from_the_Lower_Oligocen_Haselbach_Member_Saxony_Germany_containing_Apocynophyllum_neriifolium_Heer
    Genera == 'Aralia' ~ 'Araliaceae',  # https://en.wikipedia.org/wiki/Aralia
    Genera == 'Araliaephyllum' ~ 'Araliaceae', # http://www.fossilworks.org/cgi-bin/bridge.pl?a=taxonInfo&taxon_no=277374
    Genera == 'Arctocarpus' ~ 'Moraceae', # https://en.wikipedia.org/wiki/Artocarpus
    Genera == 'Aristolochites' ~ 'Aristolochiaceae', # same as Aristolochia, https://en.wikipedia.org/wiki/Aristolochia
    Genera == 'Bicameria' ~ 'Urticaceae', # But it said doubful; https://www.gbif.org/species/4900637
    Genera == 'Bridelioxylon' ~ 'Euphorbiaceae', # https://www.mindat.org/taxon-8488102.html
    Genera == 'Carpites' ~ 'Quillajaceae', # Carpites and Dakotanthus are synonyms, https://en.wikipedia.org/wiki/Dakotanthus
    Genera == 'Celastrinites' ~ 'Celastraceae', # https://en.wikipedia.org/wiki/Celastrinites
    Genera == 'Cinnamomum' ~ 'Lauraceae', # https://en.wikipedia.org/wiki/Cinnamomum
    Genera == 'Cissus' ~ 'Vitaceae', # https://en.wikipedia.org/wiki/Cissus
    Genera == 'Citrophyllum' ~ 'Rutaceae', # https://paleobiodb.org/classic/checkTaxonInfo?taxon_no=56309&is_real_user=1
    Genera == 'Cobbania' ~ 'Araceae', # https://www.jstor.org/stable/27733220
    Genera == 'Corylus' ~ 'Betulaceae', # https://en.wikipedia.org/wiki/Hazel
    Genera == 'Credneria' ~ 'Platanaceae', # https://en.wikipedia.org/wiki/Credneria
    Genera == 'Davidia' ~ 'Nyssaceae', # https://en.wikipedia.org/wiki/Davidia_involucrata
    Genera == 'Deccanocarpon' ~ 'Cucurbitaceae', # https://ijlsci.in/ls/index.php/home/article/view/142
    Genera == 'Diospyros' ~ 'Ebenaceae', # https://en.wikipedia.org/wiki/Diospyros
    Genera == 'Disanthophyllum' ~ 'Hamamelidaceae', # BUT MARKED DOUBFUL? https://www.gbif.org/species/4894024
    Genera == 'Ettingshausenia' ~ 'Platanaceae', # https://en.wikipedia.org/wiki/Platanaceae
    Genera == 'Ficophyllum' ~ 'Moraceae', # https://www.gbif.org/species/4893938
    Genera == 'Icacinoxylon' ~ 'Icacinaceae', # https://www.jstor.org/stable/2443544
    Genera == 'Ilex' ~ 'Aquifoliaceae', # https://en.wikipedia.org/wiki/Holly
    Genera == 'Jarzenia' ~ 'Magnoliales', # https://www.researchgate.net/publication/327449499_Early_Cretaceous_angiosperm_leaves_from_the_Dakota_Formation_Hoisington_III_locality_Kansas_USA
    Genera == 'Laurophyllum' ~ 'Lauraceae', # https://www.mindat.org/taxon-4896377.html
    Genera == 'Liriophyllum' ~ 'Magnoliaceae', # https://www.gbif.org/species/4888766
    Genera == 'Monetianthus' ~ 'Nymphaeaceae',  # https://en.wikipedia.org/wiki/Monetianthus
    Genera == 'Myriophyllum' ~ 'Haloragaceae', # https://en.wikipedia.org/wiki/Myriophyllum
    Genera == 'Myrtophyllum' ~ 'Salicaceae', # synonym of Azara; https://powo.science.kew.org/taxon/urn:lsid:ipni.org:names:3299617-4
    Genera == 'Nelumbites' ~ 'Nelumbonaceae', # https://en.wikipedia.org/wiki/Nelumbonaceae
    Genera == 'Nothofagus' ~ 'Nothofagaceae', # https://en.wikipedia.org/wiki/Nothofagus
    Genera == 'Nuphar' ~ 'Nymphaeaceae', # https://en.wikipedia.org/wiki/Nuphar
    Genera == 'Nyssidium' ~ 'Cercidiphyllaceae', # https://www.sciencedirect.com/science/article/abs/pii/S0024407400903358
    Genera == 'Pabiania' ~ 'Laurales',  # https://www.mindat.org/taxon-7347895.html
    Genera == 'Paliurus' ~ 'Rhamnaceae', # https://en.wikipedia.org/wiki/Paliurus
    Genera == 'Pandanophyllum' ~ 'Cyperaceae', # Synonyms of Mapania; in Wiki and also https://www.mindat.org/taxon-2731311.html
    Genera == 'Plataninium' ~ 'Platanaceae', # https://en.wikipedia.org/wiki/Platanaceae
    Genera == 'Platanophyllum' ~ 'Platanaceae', # http://www.fossilworks.org/cgi-bin/bridge.pl?a=taxonInfo&taxon_no=54639
    Genera == 'Populites' ~ 'Salicaceae', # https://www.mindat.org/taxon-3232917.html
    Genera == 'Populophyllum' ~ 'Salicaceae', # https://www.gbif.org/species/4900841
    Genera == 'Populus' ~ 'Salicaceae', # https://en.wikipedia.org/wiki/Populus#:~:text=Populus%20is%20a%20genus%20of,)%2C%20aspen%2C%20and%20cottonwood.
    Genera == 'Proteaephyllum' ~ 'Proteaceae', # http://www.fossilworks.org/cgi-bin/bridge.pl?a=taxonInfo&taxon_no=413271
    Genera == 'Pseudoprotophyllum' ~ 'Platanaceae', # https://www.mindat.org/taxon-4887183.html
    Genera == 'Quercophyllum' ~ 'Fagaceae', # http://www.fossilworks.org/cgi-bin/bridge.pl?a=taxonInfo&taxon_no=54750
    Genera == 'Quercus' ~ 'Fagaceae', # https://en.wikipedia.org/wiki/Oak
    Genera == 'Raoanthus' ~ 'Myrtaceae', # https://www.gbif.org/species/162808611
    Genera == 'Rhamnites' ~ 'Rhamnaceae', # https://species.wikimedia.org/wiki/Rhamnites
    Genera == 'Rogersia' ~ 'Saxifragaceae', # https://en.wikipedia.org/wiki/Rodgersia
    Genera == 'Sabinoxylon' ~ 'Fagaceae',  # One paper said it might be this class. https://www.researchgate.net/publication/222248171_Fossil_woods_from_the_late_Campanian-early_Maastrichtian_Olmos_Formation_Coahuila_Mexico
    Genera == 'Sahnianthus' ~ 'Lythraceae', # https://www.researchgate.net/publication/225738549_The_Geologic_History_of_the_Lythraceae
    Genera == 'Sahniocarpon' ~ 'Clusiaceae',  # https://www.jstor.org/stable/41712366?seq=8
    Genera == 'Sassafra' ~ 'Lauraceae', # https://en.wikipedia.org/wiki/Sassafras
    Genera == 'Sapindopsis' ~ 'Platanaceae', # https://en.wikipedia.org/wiki/Sapindopsis
    Genera == 'Sapotacites' ~ 'Sapotaceae', # https://fr.wikipedia.org/wiki/Sapotaceae
    Genera == 'Sassafras' ~ 'Lauraceae', # https://en.wikipedia.org/wiki/Sassafras
    Genera == 'Scheffleraephyllum' ~ 'Araliaceae', # https://fr.wikipedia.org/wiki/Araliaceae
    Genera == 'Schisandra' ~ 'Schisandraceae', # https://en.wikipedia.org/wiki/Schisandra
    Genera == 'Smilax' ~ 'Smilacaceae', # https://en.wikipedia.org/wiki/Smilax
    Genera == 'Sterculia' ~ 'Malvaceae', # https://en.wikipedia.org/wiki/Sterculia
    Genera == 'Ternstroemites' ~ 'Theaceae', # https://www.mindat.org/taxon-4887400.html
    Genera == 'Trapa' ~ 'Lythraceae', # https://en.wikipedia.org/wiki/Water_caltrop
    Genera == 'Trochodendrocarpus' ~ 'Tetracentraceae', # https://www.gbif.org/species/7471799
    Genera == 'Trochodendroides' ~ 'Cercidiphyllaceae', # https://www.researchgate.net/publication/322887708_The_genus_Trochodendroides_Cercidiphyllaceae_in_the_Cretaceous_floras_of_North-East_of_Russia
    Genera == 'Umbelliferae' ~ 'Apiaceae',  # https://en.wikipedia.org/wiki/Apiaceae
    Genera == 'Viburniphyllum' ~ 'Caprifoliaceae', # https://www.gbif.org/species/11169197/metrics
    Genera == 'Vitiphyllum' ~ 'Vitaceae', # BUT DOUBFUL; https://www.gbif.org/species/7237435
    Genera == 'Vitis' ~ 'Vitaceae', # https://en.wikipedia.org/wiki/Vitis
    Genera == 'Zizyphoides' ~ 'Trochodendraceae', # https://acpa.botany.pl/pdf-144381-71210?filename=Typification%20of%20the%20genus.pdf
    Genera == 'Zizyphus' ~ 'Rhamnaceae', # typo, Ziziphus? https://en.wikipedia.org/wiki/Ziziphus
    
    
    Genera == 'Aristolochiaephyllum' ~ 'Aristolochiaceae', # ??? This is only what I guess, from the name.
    Genera == 'Chloranthoid' ~ 'Chloranthaceae', # ??? Chloranthoid ?=? Chloranthus ???; https://en.wikipedia.org/wiki/Chloranthus
    Genera == 'Crataegites' ~ 'Rosaceae', # ??? Crataegites ?=? Crataegus ???; https://en.wikipedia.org/wiki/Crataegus
    Genera == 'Dalbergites' ~ 'Fabaceae', # ??? Dalbergites ?=? Dalbergia ???; https://en.wikipedia.org/wiki/Dalbergia
    
    
    TRUE ~ Family # 
  ))



### cf. 



### below found some stem of fm but can be assigned to ord.
# Virginianthus - stem Calycanthaceae - Laurales



### below found some assigned to order but not fm.
# Saportanthus - Laurales but no fm 
# Eydeia - Cornales but no fm https://www.researchgate.net/publication/307578467_Anatomically_preserved_fossil_cornalean_fruits_from_the_Upper_Cretaceous_of_Hokkaido_Eydeia_hokkaidoensis_gen_et_sp_nov
# Obamacarpa - Cornales but no fm https://www.researchgate.net/publication/307578467_Anatomically_preserved_fossil_cornalean_fruits_from_the_Upper_Cretaceous_of_Hokkaido_Eydeia_hokkaidoensis_gen_et_sp_nov
# Edencarpa - Cornales but no fm https://www.researchgate.net/publication/307578467_Anatomically_preserved_fossil_cornalean_fruits_from_the_Upper_Cretaceous_of_Hokkaido_Eydeia_hokkaidoensis_gen_et_sp_nov
# Suciacarpa - Cornales but no fm; https://en.wikipedia.org/wiki/Suciacarpa
# Lovellea - Laurales https://www.sciencedirect.com/science/article/abs/pii/S0195667108001092?via%3Dihub
# Schenkeriphyllum - Magnoliales https://www.sciencedirect.com/science/article/abs/pii/S003466671200200X
# Archaefagacea - Fagales https://www.researchgate.net/publication/240563889_Fossil_Flowers_of_Fagales_from_the_Kamikitaba_Locality_Early_Coniacian_Late_Cretaceous_of_Northeastern_Japan
# Crassidenticulum - Laurales https://paleobiodb.org/classic/checkTaxonInfo?taxon_no=56277&is_real_user=1
# Longstrethia - Laurales https://www.mindat.org/taxon-7347896.html
# Soepadmoa - Fagales https://www.researchgate.net/publication/325770238_A_late_Cretaceous_fagalean_inflorescence_preserved_in_amber_from_New_Jersey
# Lusistemon - Buxales https://www.tandfonline.com/doi/full/10.1080/00173130701526507
# Symphaenale - Nymphaeales https://www.researchgate.net/publication/249158498_Fossil_Seeds_of_Nymphaeales_from_the_Tamayama_Formation_Futaba_Group_Late_Cretaceous_Early_Santonian_of_Northeastern_Honshu_Japan
# Teixeiraea - Ranunculales https://www.jstor.org/stable/pdf/23655238.pdf?casa_token=933ltkCPMZgAAAAA:u0cwAPzEQx2QkjwIcXMCCJANzVWp3YY9NwCabM9FRWSP_r_pq5ekHyzWyhfO2uwLErzAxKwxvziF6c8wjvyzriaRN6avANxZE9O4bSUZTQeox5GZAZ8f
# Endressianthus - Fagales https://www.jstor.org/stable/pdf/10.1086/376875.pdf
# Paradinandra - Ericales https://en.wikipedia.org/wiki/Paradinandra
# Archamamelis - Hamamelidales #'The fossils probably represent a separate phylogenetic line either within the Hamamelidales,less certainly within the Hamamelidaceae.' https://www.academia.edu/78416735/Archamamelis_hamamelidalean_flowers_from_the_Upper_Cretaceous_of_Sweden?uc-sb-sw=33517898
# Scandianthus - Saxifragales https://www.jstor.org/stable/42758591?seq=3
# Phenanthera - Caryophyllales https://paleobiodb.org/classic/checkTaxonInfo?taxon_no=412541&is_real_user=1



### below found order, but fm is also extinct
# Montsechia - Montsechiaceae - Ceratophyllales https://en.wikipedia.org/wiki/Montsechia
# Archaeomnium - Mniaceae http://www.fossilworks.org/cgi-bin/bridge.pl?action=taxonInfo&taxon_no=405349



### below found some records that their family was deleted in APG system but since they're extinct genus, no idea where to put now
# Iliciphyllum was in Iliciaceae (might be Illiciaceae?); APG changed see https://en.wikipedia.org/wiki/Illiciaceae; record see https://www.researchgate.net/publication/263074827_Paleocene_flora_from_Seymour_Island_Antarctica_Revision_of_Dusen's_1908_angiosperm_taxa


######################## please choose ONLY option 1 OR 2 ##########################

################
### option 1 ###
################

# all unnamed families treated as the SAME
# so they all named 'UNNAMED_FAM' (later will be assigned to 'UNNAMED_ORD' FOR ALL)
# and when calculating the whole angiosperms using orders as units
# it will only count once whenever how many times those obscure species appear

df_genera_family_revised_1 <- df_genera_family_revised %>% 
  mutate(Family = ifelse(is.na(Family), 'UNNAMED_FAM', Family))

# save for check
write.xlsx(df_genera_family_revised_1, file = "df_genera_family_revised_1.xlsx") # 

#
rm( missing_fm, number_missing_fm, unique_first_fields)





################
### option 2 ###
################

# all unnamed families treated as DIFFERENT
# so they all named 'UNNAMED_FAM_1' to ‘UNNAMED_FAM_n' (later will be assigned to 'UNNAMED_ORD_1' to 'UNNAMED_ORD_n', accordingly)
# and when calculating the whole angiosperms using orders as units
# it will count as many as those obscure species appear (every obscure species treated as independent order)


# 
#empty_family_count <- sum(is.na(df_genera_family_revised$Family))

# 
#fill_values <- paste0("UNNAMED_FM_", seq(1, empty_family_count))

# 
#df_genera_family_revised_2 <- df_genera_family_revised %>% 
#  mutate(Family = replace(Family, is.na(Family), fill_values))

# save for check
#write.xlsx(df_genera_family_revised_2, file = "df_genera_family_revised_2.xlsx") # 

#
#rm( empty_family_count, fill_values)




######################## single choice section ends here  ##########################




### step 3 ###
# adding family info according to Genspec info

# if choose option 1

rm (df_age_diff_gt_20,df_age_revised,df_max_smaller_than_min,df_missing_both_age,
    df_missing_max_age,df_quote_mark)


# 
combined_table <- anti_join(combined_table, df_missing_fm, by = c("ID", "source")) # from 24429 to 23118 records


# 
df_missing_fm <- df_missing_fm %>%
  mutate(Genera = str_extract(Genspec, "^[^\\s]+"))

# 
df_missing_fm_revised <- left_join(df_missing_fm, df_genera_family_revised_1, by = c("Genera" = "Genera"))

# 
df_missing_fm_revised <- df_missing_fm_revised %>%
  mutate(Family = Family.y) %>%
  select(-Family.x, -Family.y, -Genera) # 

# 
df_missing_fm_revised <- df_missing_fm_revised %>%
  mutate(Family = ifelse(is.na(Family), 'UNNAMED_FAM', Family))



# 
combined_table <- bind_rows(combined_table, df_missing_fm_revised)

# 
combined_table <- combined_table %>%
  arrange( source, ID)





# 
# 
family_has_na <- any(is.na(combined_table$Family))
# 
if (family_has_na) {
  cat("Combined table's Family column contains NA.\n")
} else {
  cat("Combined table's Family column does not contain NA.\n")
}


# 
rm( df_genera_family, df_genera_family_revised, df_genera_family_revised_1,
    df_unique_Gen, df_missing_fm, df_missing_fm_revised,
    family_has_na)



###################
# 6. match order #
##################

# loading the family_order_clade file
path_taxonomic = "./family_order_clade.xlsx" 
taxonomic <- read_excel(path_taxonomic)

# 
combined_table <- left_join(combined_table, taxonomic, by = "Family")
# 
combined_table <- combined_table %>% 
  select( source, ID, Formation, Age_min, Age_max, Specific_epithet, Genspec, Family, Order, Clade_1, Clade_2, RefLink)

# ATTENTION: FOUND SOME RECORDS WITHOUT ORDER NAME
# check those without Order (can't match according to Family name)
missing_ord <- is.na(combined_table$Order)
# 
number_missing_ord <- sum(missing_ord) # 1754 records
# 
df_missing_ord <- combined_table[missing_ord,]
# save for check ##
df_missing_ord_select <- df_missing_ord %>%
  select( source, ID, Formation, Age_min, Age_max, Specific_epithet, Genspec, Family, Order, Clade_1, Clade_2)
  
  
write.xlsx(df_missing_ord_select, file = "df_missing_ord.xlsx") # 



#
rm( path_taxonomic, missing_ord, number_missing_ord)

# revised data
# base on literally our best guess

### A. revised fm ###
### 1. Those with one potential Family name ###
df_missing_ord_revised <- df_missing_ord %>%
  mutate(
         Family = if_else(Family == "?Malvaceae", "Malvaceae", Family),
         Family = if_else(Family == "Anacardiaceae?", "Anacardiaceae", Family),
         Family = if_else(Family == "Lauraceae?", "Lauraceae", Family),
         Family = if_else(Family == "Apocynaceae?", "Apocynaceae", Family),
         Family = if_else(Family == "Araliaceae?", "Araliaceae", Family),
         Family = if_else(Family == "Betulaceae?", "Betulaceae", Family),
         Family = if_else(Family == "Clusiaceae?", "Clusiaceae", Family),
         Family = if_else(Family == "Cunoniaceae?", "Cunoniaceae", Family),
         Family = if_else(Family == "Euphorbiaceae?", "Euphorbiaceae", Family),
         Family = if_else(Family == "Fagaceae?", "Fagaceae", Family),
         Family = if_else(Family == "Grossulariaceae?", "Grossulariaceae", Family),
         Family = if_else(Family == "Hamamelidaceae?", "Hamamelidaceae", Family),
         Family = if_else(Family == "Icacinaceae?", "Icacinaceae", Family),
         Family = if_else(Family == "Juglandaceae?", "Juglandaceae", Family),
         Family = if_else(Family == "Lauraceae?", "Lauraceae", Family),
         Family = if_else(Family == "Malvaceae?", "Malvaceae", Family),
         Family = if_else(Family == "Menispermaceae?", "Menispermaceae", Family),
         Family = if_else(Family == "Myrtaceae?", "Myrtaceae", Family),
         Family = if_else(Family == "Olacaceae?", "Olacaceae", Family),
         Family = if_else(Family == "Phyllanthaceae?", "Phyllanthaceae", Family),
         Family = if_else(Family == "Platanaceae?", "Platanaceae", Family),
         Family = if_else(Family == "Siparunaceae?", "Siparunaceae", Family),
         Family = if_else(Family == "Sterculiaceae?", "Malvaceae", Family), # according to https://en.wikipedia.org/wiki/Sterculiaceae
         Family = if_else(Family == "Winteraceae?", "Winteraceae", Family),
         
         Family = if_else(Family == "aff. A. quinquelobatus", "DELETE", Family), # ?
         Family = if_else(Family == "aff. Combretaceae", "Combretaceae", Family),
         Family = if_else(Family == "aff. Dicotylophyllum sp.", "Dicotylophyllum", Family), # FOSSIL GEN.
         Family = if_else(Family == "aff. M. antarcticum", "DELETE", Family), # ?
         Family = if_else(Family == "aff. Ranunculaceae", "Ranunculaceae", Family),
         
         Family = if_else(Family == "cf.  Dilleniaceae", "Dilleniaceae", Family),
         Family = if_else(Family == "cf.  N. magelhaenica", "DELETE", Family), # ?
         Family = if_else(Family == "cf. Amborel1aceae", "Amborellaceae", Family),
         Family = if_else(Family == "cf. Atherospermataceae", "Atherospermataceae", Family),
         Family = if_else(Family == "cf. Betulaceae", "Betulaceae", Family),
         Family = if_else(Family == "cf. Calycanthaceae", "Calycanthaceae", Family),
         Family = if_else(Family == "cf. Chloranthaceae", "Chloranthaceae", Family),
         Family = if_else(Family == "cf. Dilleniaceae", "Dilleniaceae", Family),
         Family = if_else(Family == "cf. Gomortegaceae", "Gomortegaceae", Family),
         Family = if_else(Family == "cf. Gyrocarpaceae", "Hernandiaceae", Family), # APG4, according to https://en.wikipedia.org/wiki/Hernandiaceae
         Family = if_else(Family == "cf. Hamamelidaceae", "Hamamelidaceae", Family),
         Family = if_else(Family == "cf. Hernandiaceae", "Hernandiaceae", Family),
         Family = if_else(Family == "cf. Hortoniaceae", "Hortoniaceae", Family),
         Family = if_else(Family == "cf. Lauraceae", "Lauraceae", Family),
         Family = if_else(Family == "cf. Monimiaceae", "Monimiaceae", Family),
         Family = if_else(Family == "cf. Nelumbonaceae", "Nelumbonaceae", Family),
         Family = if_else(Family == "cf. Nymphaeaceae", "Nymphaeaceae", Family),
         Family = if_else(Family == "cf. Paracryphiaceae", "Paracryphiaceae", Family),
         Family = if_else(Family == "cf. Platanaceae", "Platanaceae", Family),
         Family = if_else(Family == "cf. Quillajaceae", "Quillajaceae", Family),
         Family = if_else(Family == "cf. Schisandraceae", "Schisandraceae", Family),
         Family = if_else(Family == "cf. Trochodendraceae", "Trochodendraceae", Family),
         Family = if_else(Family == "cf. Winteraceae", "Winteraceae", Family),
         
         Family = if_else(Family == "ARALIACEAE", "Araliaceae", Family),
         Family = if_else(Family == "ASTERACEAE", "Asteraceae", Family),
         Family = if_else(Family == "BORAGINACEAE", "Boraginaceae", Family),
         Family = if_else(Family == "BRASSICACEAE", "Brassicaceae", Family),
         Family = if_else(Family == "CAPPARACEAE", "Capparaceae", Family),
         Family = if_else(Family == "CELASTRACEAE", "Celastraceae", Family),
         Family = if_else(Family == "CONNARACEAE", "Connaraceae", Family),
         Family = if_else(Family == "ERICACEAE", "Ericaceae", Family),
         Family = if_else(Family == "HAMAMELIDACEAE", "Hamamelidaceae", Family),
         Family = if_else(Family == "JUGLANDACEAE", "Juglandaceae", Family),
         Family = if_else(Family == "JUNCAGINACEAE", "Juncaginaceae", Family),
         Family = if_else(Family == "MYRTACEAE", "Myrtaceae", Family),
         Family = if_else(Family == "NYCTAGINACEAE", "Nyctaginaceae", Family),
         Family = if_else(Family == "POACEAE", "Poaceae", Family),
         Family = if_else(Family == "SIMAROUBACEAE", "Simaroubaceae", Family),
         Family = if_else(Family == "VITACEAE", "Vitaceae", Family),
         
         Family = if_else(Family == "Magnoliidae, similar to Eupomatiaceae and Calycanthaceae", "DELETE", Family), # obscure position; two fm not from same order
         Family = if_else(Family == "Probably Nehnnbonaceae", "DELETE", Family), # ? CAN'T FIND
         Family = if_else(Family == "Proto-cyperaceous plants", "DELETE", Family), # Proto- means similar but still not cyperaceous? I guess?
         
         )

### 2. Those with multiple potential Family names #

df_missing_ord_revised <- df_missing_ord_revised %>%
  mutate(
         Family = if_else(Family == "Anacardiaceae?Burseraceae? Lauraceae?", "DELETE", Family), # first two Sapindales, last Laurales
         Family = if_else(Family == "Euphorbiaceae? Lauraceae?", "DELETE", Family), 
         Family = if_else(Family == "Fagaceae?Pentaphylacaceae?", "DELETE", Family),
         Family = if_else(Family == "Mastixiaceae? Symplocaceae?", "DELETE", Family),
         Family = if_else(Family == "Nyssaceae? Cornaceae?", "Nyssaceae", Family), # both from Cornales
         Family = if_else(Family == "Phyllanthaceae? Lauraceae?", "DELETE", Family),
         Family = if_else(Family == "Platanaceae? Icacinaceae？", "DELETE", Family),
         Family = if_else(Family == "Ranunculaceae? Paeoniaceae?", "DELETE", Family),
         
         Family = if_else(Family == "aff. Elaeocarpus sp. and Sloanea sp.", "Elaeocarpaceae", Family), # both from Elaeocarpaceae, Oxalidales
         
         Family = if_else(Family == "Atherospermataceae + Gomortega (Gomortegaceae)", "Atherospermataceae", Family), # both from Laurales
         Family = if_else(Family == "Juglandaceae-Myricaceae", "Juglandaceae", Family), # both from Fagales
         Family = if_else(Family == "Monimiaceae-Lauraceae-Hernandiaceae", "Monimiaceae", Family), # both from Laurales
         Family = if_else(Family == "Ranunculoid—paeonioid", "DELETE", Family), # ?
         Family = if_else(Family == "Saururaceae, Aristolochiaceae, and Piperaceae", "Saururaceae", Family), # all from Piperales
         
         Family = if_else(Family == "cf. Circaeaster, Chloranthaceae, and Piperales", "DELETE", Family), # ?
         Family = if_else(Family == "cf. Achariaceae, Salicaceae", "Achariaceae", Family), # both from Malpighiales
         Family = if_else(Family == "cf. Chloranthaceae and Piperales", "DELETE", Family), # not same order
         Family = if_else(Family == "cf. Hydrangeaceae，Saxifragaceae", "DELETE", Family), # not same order
         Family = if_else(Family == "cf. Malpighiales, Myrtales, and Oxalidales", "DELETE", Family), # Order
         Family = if_else(Family == "cf. Nymphaeaceae, Illiciaceae", "DELETE", Family), # not same order
         Family = if_else(Family == "cf. Ranunculaceae,Buxaceae, and Myrothamnaceae", "DELETE", Family) # not from same order
         )


### 3. revised fm, including rejected, obscure, and re-placed ###

df_missing_ord_revised <- df_missing_ord_revised %>%
  mutate(
         Family = if_else(Family == "Amfelidaceae", "DELETE", Family), # only googled 1 result, obscure position
         Family = if_else(Family == "Archaefructaceae", "DELETE", Family), # FOSSIL FM.
         Family = if_else(Family == "Corylaceae", "Betulaceae", Family), # synonym according to https://www.mindat.org/taxon-3659021.html
         Family = if_else(Family == "Degeneríaceae", "DELETE", Family), # only googled 1 result, obscure position
         Family = if_else(Family == "Epacridaceae", "Ericaceae", Family), # revised to Ericaceae, https://en.wikipedia.org/wiki/Epacris
         Family = if_else(Family == "Eriospermaceae", "Asparagaceae", Family), # APG3 & 4, https://www.pacificbulbsociety.org/pbswiki/index.php/Eriospermaceae
         Family = if_else(Family == "Fagofolia", "DELETE", Family), # can't find any information
         
         Family = if_else(Family == "Hemerocallidaceae", "Asphodelaceae", Family),
         Family = if_else(Family == "Hortoniaceae", "Monimiaceae", Family), # synonym according to https://www.gbif.org/species/2494
         
         Family = if_else(Family == "Juglandaceous", "Juglandaceae", Family),
         Family = if_else(Family == "Laxmanniaceae", "Asparagaceae", Family), # APG3, https://fr.wikipedia.org/wiki/Laxmanniaceae
         Family = if_else(Family == "Leguminofolia", "DELETE", Family), # only googled 2 results, obscure position
         Family = if_else(Family == "Maloideae", "Rosaceae", Family), # quick search online
         Family = if_else(Family == "Mastixiaceae", "Nyssaceae", Family), # https://en.wikipedia.org/wiki/Nyssaceae
         Family = if_else(Family == "Platanoid", "Sapindaceae", Family), # I think... https://en.wikipedia.org/wiki/Acer_platanoides
         Family = if_else(Family == "Rhamnaceaee", "Rhamnaceae", Family), # typo
         Family = if_else(Family == "Saurauiaceae", "DELETE", Family), # rejected fm.
         Family = if_else(Family == "Sparganiaceae", "Typhaceae", Family), # APG3, https://en.wikipedia.org/wiki/Sparganiaceae
         Family = if_else(Family == "Stenomaceae", "DELETE", Family), # only googled 5 results, obscure position
         Family = if_else(Family == "Trapaceae", "Lythraceae", Family), # fm changed according to APG3; both from Myrtales
         Family = if_else(Family == "Ulmaceous morphotype", "DELETE", Family), # ?
         Family = if_else(Family == "Vitaccae", "Vitaceae", Family), # might be a typo?
         
         )


# Four more records need to be revised by key (source + ID) due to the unknown format
# Cretaceous 437   # Saururaceae, Aristolochiaceae, and Piperaceae              # set Family == 'Saururaceae'
# Cretaceous 668   # cf. Circaeaster, Chloranthaceae, and Piperales             # delete data
# Cretaceous 804   # Magnoliidae, similar to Eupomatiaceae and Calycanthaceae   # delete data
# Cretaceous 1404  # cf. Ranunculaceae, Buxaceae, and Myrothamnaceae            # delete data
df_missing_ord_revised <- df_missing_ord_revised %>%
  mutate(
    Family = if_else(source == 'Cretaceous' & ID == "437",  "Saururaceae", Family), # only googled 1 result, obscure position
    Family = if_else(source == 'Cretaceous' & ID == "668",  "DELETE", Family),
    Family = if_else(source == 'Cretaceous' & ID == "804",  "DELETE", Family),
    Family = if_else(source == 'Cretaceous' & ID == "1404", "DELETE", Family)
    )



#################################################################

### 4. Refill the Order & delete those named 'DELETE' ###

# Refill the Order for revised Family names
df_missing_ord_revised <- df_missing_ord_revised %>%
  left_join(taxonomic, by = "Family") %>%
  select(-ends_with(".x"), -family) %>% # delete those column from the original df
  rename_with(~ gsub("\\.y$", "", .x), ends_with(".y")) # rename those end with '.y'


### B. revised ord ###
### 5. Those not in the list ###

df_missing_ord_revised <- df_missing_ord_revised %>%
  mutate(Order = case_when(
    Family == "Aizoaceae" ~ "Caryophyllales",
    Family == "Bombacaceae" ~ "Malvales",
    Family == "Byblidaceae " ~ "Lamiales", # ATTENTION: HERE IS A SPACE ALONG WITH THE FM NAME
    Family == "Chloranthaceae" ~ "Chloranthales", # APG4
    Family == "Cistaceae" ~ "Malvales",
    Family == "Corynocarpaceae" ~ "Cucurbitales",
    Family == "Eupomatiaceae" ~ "Magnoliales",
    Family == "Geraniaceae" ~ "Geraniales",
    Family == "Illiciaceae" ~ "Austrobaileyales",
    Family == "Iridaceae" ~ "Asparagales",
    Family == "Juncaginaceae" ~ "Alismatales",
    Family == "Nyssaceae" ~ "Cornales",
    Family == "Paracryphiaceae" ~ "Paracryphiales",
    Family == "Peraceae" ~ "Malpighiales",
    Family == "Rhamnaceae" ~ "Rosales", 
    Family == "Sarcobataceae" ~ "Caryophyllales",
    Family == "Tamaricaceae" ~ "Caryophyllales",
    Family == "Tetramelaceae" ~ "Cucurbitales",
    Family == "Zosteraceae" ~ "Alismatales",
    
    # FOLLOWING ARE THOSE SHOWING THE FIRST TIME BECAUSE OF THE FM RE-ASSIGN
    Family == "Amborellaceae" ~ "Amborellales",
    Family == "Gomortegaceae" ~ "Laurales",
    Family == "Quillajaceae" ~ "Fabales",
    Family == "Siparunaceae" ~ "Laurales",
    
    # FOLLOWING ARE THOSE ADDITIONAL WHEN CALCULATING THE WHOLE ANGIOSPERMS #20240221
    Family == "UNNAMED_FAM" ~ "UNNAMED_ORD",
    Family == "cf. Ranunculales" ~ "Ranunculales",
    Family == "Laurales,Lauralean" ~ "Laurales",
    
    TRUE ~ Order # if Family doesn't match any above, keep original Order of the Family 
  ))


# Two more records need to be revised by key (source + ID) due to the unknown format
# Cenozoic 5415    # Byblidaceae                                      # set Order == 'Lamiales'
# Cretaceous 136   # Tetracentraceae                                  # set Order == 'Trochodendrales’

# Cretaceous 457   # similar to Dicotylophyllum latitrilobatum        # set Order == 'UNNAMED_ORD’
# Cretaceous 458   # similar to Dicotylophyllum latitrilobatum        # set Order == 'UNNAMED_ORD’
# Cretaceous 465   # Dicotylophyllum                                  # set Order == 'UNNAMED_ORD’

# Cretaceous 1275  # Laurales                                         # set Order == 'Laurales’
# Cretaceous 1279  # Magnoliales                                      # set Order == 'Magnoliales’



df_missing_ord_revised <- df_missing_ord_revised %>%
  mutate(Order = case_when(
    source == 'Cenozoic' & ID == "5415" ~ "Lamiales",
    source == 'Cretaceous' & ID == "136" ~ "Trochodendrales",
    source == 'Cretaceous' & ID == "457" ~ "UNNAMED_ORD",
    source == 'Cretaceous' & ID == "458" ~ "UNNAMED_ORD",
    source == 'Cretaceous' & ID == "465" ~ "UNNAMED_ORD",
    source == 'Cretaceous' & ID == "1275" ~ "Laurales",
    source == 'Cretaceous' & ID == "1279" ~ "Magnoliales",
      TRUE ~ Order), # 
    )

#################################################################

# Filter out records marked as 'DELETE' in the Family column
df_missing_ord_revised <- filter(df_missing_ord_revised, Family != "DELETE") # from 1754 to 1706 records

# extract the distinct information of order-clade1-clade2 
unique_clades_mapping <- taxonomic %>%
  select(Order, Clade_1, Clade_2) %>%
  distinct()  # 

# refill the Clade names into df 
df_missing_ord_revised <- df_missing_ord_revised %>%
  left_join(unique_clades_mapping, by = "Order") %>%
  select(source, ID, Formation, Age_min, Age_max, Specific_epithet, 
         Genspec, Family, Order, Clade_1.y, Clade_2.y, RefLink) %>%
  rename(Clade_1 = Clade_1.y, Clade_2 = Clade_2.y)


# still have four Orders that need to manually match Clade info
  # because they are not included in the taxonomic, but were assigned when revised the df
df_missing_ord_revised <- df_missing_ord_revised %>%
  mutate(
    Clade_1 = case_when(
      Order == "Amborellales" ~ "Basal_angiosperms",
      Order == "Chloranthales" ~ "Chloranthidae",
      Order == "Geraniales" ~ "Eudicots",
      Order == "Paracryphiales" ~ "Eudicots",
      Order == "UNNAMED_ORD" ~ "UNNAMED_CLD1", #20240221
      TRUE ~ Clade_1 # if Order not match any above, keep their original Clade_1 
    ),
    Clade_2 = case_when(
      Order == "Amborellales" ~ "NA",
      Order == "Chloranthales" ~ "NA",
      Order == "Geraniales" ~ "Rosids",
      Order == "Paracryphiales" ~ "Asterids",
      Order == "UNNAMED_ORD" ~ "UNNAMED_CLD2", #20240221
      TRUE ~ Clade_2 # if Order not match any above, keep their original Clade_2 
    )
  )

# save for check
df_missing_ord_revised_select <- df_missing_ord_revised %>%
  select( source, ID, Formation, Age_min, Age_max, Specific_epithet, Genspec, Family, Order, Clade_1, Clade_2)

write.xlsx(df_missing_ord_revised_select, file = "df_missing_ord_revised.xlsx")
# 
rm(df_missing_ord_select,df_missing_ord_revised_select,taxonomic,unique_clades_mapping)


### Taxonomic clearance (inlcuding block 5 & 6) should be finished till here ###

#################################################################


### combine revised into original df
### delete those need changed from combine_table, and refill those after change to it.
# 1. delete those without order info (delete those before block 6) 
combined_table <- anti_join(combined_table, df_missing_ord, by = c("ID", "source")) # 22675 records

# 2. add revised back (add those after block 6) 
combined_table <- rbind(combined_table, df_missing_ord_revised) # 24381 records 

# 3. 
combined_table <- combined_table %>% distinct() # still 24381 records

# 4. 
summary(combined_table$Order) # Length should be 24381 as well

# 5.
#write.xlsx(combined_table, file = "Combined_Table.xlsx")


######################################
# 7. unique records & add RandomAge #
#####################################

# delete the repeat occs based on family level (duplicates mean those have the same MinAge/MaxAge/reference ) 
combined_table_unique <- combined_table %>%
  distinct(Family, Age_min, Age_max, RefLink, .keep_all = TRUE) # 10050 records

# calculate the RandomAge
combined_table_unique$RandomAge <- runif(nrow(combined_table_unique), 
                                         min = combined_table_unique$Age_min, 
                                         max = combined_table_unique$Age_max)

# create table for the counts of species per time bin 
max(combined_table_unique$Age_max) # 139.8

rm(combined_table, df_missing_ord, df_missing_ord_revised)

##########################################
# 8. generating the input for Whole angio#
##########################################

# create time series 
time_sequence <- seq(2.5, 145, by = 2.5) # 

# get all unique orders 
unique_orders <- distinct(combined_table_unique, Order) %>% pull(Order)

# create a new dataframe 
# 
final_table <- matrix(0, 
                      nrow = length(time_sequence) + 1, 
                      ncol = 1) # 
rownames(final_table) <- c(0, time_sequence) # 
colnames(final_table) <- "angiosperms"

#


# 
for (i in 1:length(time_sequence)) {
  cat("Processing iteration", i, "\n")
  
  # 
  if (i == 1) {
    # 
    subset_data <- combined_table_unique %>%
      filter(RandomAge < time_sequence[i])
  } else {
    # 
    subset_data <- combined_table_unique %>%
      filter(RandomAge >= time_sequence[i-1] & RandomAge < time_sequence[i])
  }
  
  # 
  if (nrow(subset_data) == 0) {
    unique_orders <- 0
  } else {
    # 
    unique_orders <- subset_data %>%
      distinct(Order) %>%
      nrow()
  }
  
  # 
  final_table[i+1, "angiosperms"] <- unique_orders
}



# delete the first row which would be always 0 
final_table <- final_table[-1, ]
# transfer final_table as dataframe 
final_data_frame <- as.data.frame(final_table)
# 
final_table_df <- cbind(TimeBin = time_sequence, final_data_frame)
# 
colnames(final_table_df)[1] <- "time"
colnames(final_table_df)[2] <- "angiosperms"
#
rm(final_table,final_data_frame,filtered_records)
rm(end_time,i,order,start_time,time_sequence,unique_families_count,unique_orders)

#########################
# 9. save final inputs #
########################

# save 
write.table(final_table_df, file = "angiosperms_whole-0221.txt", quote=FALSE, row.names=FALSE, sep="\t")
write.xlsx(final_table_df, file = "angiosperms_whole-0221.xlsx")

combined_table_unique_select <- combined_table_unique %>%
  select( source, ID, Formation, Age_min, Age_max, RandomAge, Specific_epithet, Genspec, Family, Order, Clade_1, Clade_2)


write.xlsx(combined_table_unique_select, file = "combined_table_unique_final.xlsx") # 
