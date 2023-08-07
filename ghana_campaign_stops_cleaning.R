setwd("~/OneDrive - University of Bristol/Post-doc/Ghana Campaign Stops")

library(readxl)
library(dplyr)
library(stringr)
library(sf)
library(tidyr)
library(fuzzyjoin)
library(ggplot2)
library(fixest)

#read ghana constituency boundaries - CERSGIS map 
ghana_boundaries_a = st_read("GHA_adm2.shp") %>%
  st_make_valid() %>%
  st_simplify(., preserveTopology = F, dTolerance = 1000) %>%
  #pre-clean constituency names 
  dplyr::mutate(NAME_2 = stringr::str_replace(NAME_2, "-" ," ")) #replace "-" with space
              

#Noah Nathan boundaries - missing data for Tamale
#ghana_boundaries_b = st_read("Ghana_electoral_boundaries_2016/const2016_dissolve.shp") %>%
#  st_transform(., crs = st_crs(ghana_boundaries_a)) %>% #reproject coordinates. 
#  st_make_valid() %>%
#  st_simplify(., preserveTopology = F, dTolerance = 1000) 

#how come the geometries are so different - 
#which one would jeff prefer? Full with straight edges or incomplete with 'better' boundaries
plot(st_geometry(ghana_boundaries_a))
#plot(st_geometry(ghana_boundaries_b))


#read campaign stops 
campaign_stops = read_xlsx("2016-2020 CAMPAIGN STOPS (Responses).xlsx", sheet = 1) %>%
  dplyr::select(-c("...11", "...12", "...13", "...15")) %>%
  #clean activity strings - which ones are most relevant? What can be grouped
  # simplify to 4 event type: rally, courtesy, interaction, official business 
  # grouping other events into these 4 higher-level concepts. 
  # 
  dplyr::mutate(Activity = tolower(Activity),#convert all to lower case
                #address specific inconsistencies 
                Activity = ifelse(Activity == "durban", "durbar", Activity), 
                Activity = ifelse(Activity == "inspections", "inspection", Activity), 
                Activity = tidyr::replace_na(Activity, "rally"),
                #there are still some activity types not included which have been set to na
                activity_type = dplyr::case_when(Activity %in% c("rally", "mini-rally") ~ "rally", 
                                                 Activity %in% c("official", "sod cutting", "commission", 
                                                            "inauguration", "inspection") ~ "official_business", 
                                                 Activity %in% c("courtesy call", "durbar") ~ "courtesy_call",
                                                 Activity %in% c("meeting", "visit", "interaction", 
                                                                 "inauguration", "public ceremony", 
                                                                 "campaign launch", "whistle stop", 
                                                                 "drive through", "press conference", 
                                                                 "tour") ~ "interaction")) %>%
  drop_na(Constituency) %>% #drop stops with unknown constituency
  #pre-clean constituency names 
  dplyr::mutate(Constituency = stringr::str_replace(Constituency, "-" ," "), 
                Constituency = ifelse(Constituency == "Ajumako Enyan Esiam", "Ajumako Enyan-Esiam", Constituency),
                Constituency = ifelse(Constituency == "Akwapim South", "Akwapem South", Constituency),
                Constituency = ifelse(Constituency == "Asene Akroso Manso", "Asene Akroso-Manso", Constituency),
                Constituency = ifelse(Constituency == "Bortianor Ngleshie Amanfrom", "Bortianor Ngleshie Amanfro", Constituency),
                Constituency = ifelse(Constituency == "Damango", "Damongo", Constituency),
                Constituency = ifelse(Constituency == "Evalue Ajomoro Gwira", "Evalue Ajomoro-Gwira", Constituency),
                Constituency = ifelse(Constituency == "Asene Akroso Manso", "Asene Akroso-Manso", Constituency),
                Constituency = ifelse(Constituency == "Akropong", "Akrofuom", Constituency),
                Constituency = ifelse(Constituency == "Hemang Lower Denkyira", "Hermang Lower Denkyira", Constituency),
                Constituency = ifelse(Constituency == "Odododiodio", "Odododiodioo", Constituency)) #replace "-" with space


#fuzzy matching of constituency names between constituency boundaries and campaign stops

#create strings to match on
#match_key_campaign = as.data.frame(unique(campaign_stops$Constituency)) %>%
#  drop_na() %>%
#  dplyr::rename(match_key = "unique(campaign_stops$Constituency)") %>%
#  dplyr::arrange(match_key)

#match_key_constit = as.data.frame(unique(ghana_boundaries_a$NAME_2)) %>%
#  dplyr::rename(match_key = "unique(ghana_boundaries_a$NAME_2)") %>%
#  dplyr::arrange(match_key)

#calculate match distance
#constit_match = stringdist_join(match_key_campaign, match_key_constit,
#                               by = "match_key", #change to common name in both datasets
#                               mode = "left",
#                               ignore_case = FALSE, 
#                               method = "lv", 
#                               max_dist = 5, 
#                               distance_col = "dist") %>%
#  group_by(match_key.x) %>%
#  slice_min(order_by = dist, n = 1) %>%
#  dplyr::rename(campaign = match_key.x, constit = match_key.y)

#subset exact matches - we've now got 241 exact matches meaning 34 constituencies without stops. 
#constit_is_match = constit_match %>% dplyr::filter(dist == 0) 

#subset non exact matches - 13 which I beleive are non-matches
#constit_is_not_match = constit_match %>% dplyr::filter(dist > 0)

#create new variables 
# election = 2016 if string in date == 2016 else 2020
#incumbent if candiate = John Mahama & election == 2016 or candidate = Nana Akufo-Addo  and election = 2020

campaign_stops = campaign_stops %>%
  dplyr::mutate(election = ifelse(str_detect(Date, "2016"), 2016, 2020))
election_2016 = campaign_stops %>% dplyr::filter(election == 2016)
election_2020 = campaign_stops %>% dplyr::filter(election == 2020)
mahama = campaign_stops %>% dplyr::filter(Candidate == "John Mahama")
addo = campaign_stops %>% dplyr::filter(Candidate == "Nana Addo Dankwa Akuffo Addo")

sjPlot::tab_xtab(campaign_stops$activity_type, campaign_stops$election) 
sjPlot::tab_xtab(campaign_stops$activity_type, campaign_stops$Candidate)
#2016
sjPlot::tab_xtab(election_2016$activity_type, election_2016$Candidate)
#2020
sjPlot::tab_xtab(election_2020$activity_type, election_2020$Candidate)

sjPlot::tab_xtab(campaign_stops$`Region of Campaign Event`, campaign_stops$activity_type)
sjPlot::tab_xtab(mahama$`Region of Campaign Event`, mahama$activity_type)
sjPlot::tab_xtab(addo$`Region of Campaign Event`, addo$activity_type)

cross_cases(campaign_stops, activity_type, election)

long_data = campaign_stops %>%
  dplyr::mutate(election = ifelse(str_detect(Date, "2016"), 2016, 2020)) %>%
  filter(!is.na(activity_type)) %>%
  #calculate generic counts 
  dplyr::group_by(Constituency, Candidate, election) %>%
  summarise(total_stops = n(), 
            courtesy_call = sum(activity_type == "courtesy_call"), 
            rally = sum(activity_type == "rally"), 
            interaction = sum(activity_type == "interaction"), 
            official_business = sum(activity_type == "official_business"))

write.csv(long_data, file = "campaign_stops_clean.csv")

#assign geometry data from gahan boundaries

#code all campaign stops types in wide format
wide_data = campaign_stops %>%
  dplyr::mutate(election = ifelse(str_detect(Date, "2016"), 2016, 2020)) %>%
  filter(!is.na(activity_type)) %>%
  #calculate generic counts 
  dplyr::group_by(Constituency) %>%
  summarise(courtesy_call = sum(activity_type == "courtesy_call"), 
            rally = sum(activity_type == "rally"), 
            interaction = sum(activity_type == "interaction"), 
            official_business = sum(activity_type == "official_business"), 
            courtesy_call_2020 = sum(activity_type == "courtesy_call" & election == 2020), 
            rally_2020 = sum(activity_type == "rally" & election == 2020), 
            interaction_2020 = sum(activity_type == "interaction" & election == 2020), 
            official_business_2020 = sum(activity_type == "official_business" & election == 2020), 
            courtesy_call_2016 = sum(activity_type == "courtesy_call" & election == 2016), 
            rally_2016 = sum(activity_type == "rally" & election == 2016), 
            interaction_2016 = sum(activity_type == "interaction" & election == 2016), 
            official_business_2016 = sum(activity_type == "official_business" & election == 2016), 
            courtesy_call_Mahama  = sum(activity_type == "courtesy_call" & Candidate == "John Mahama"), 
            rally_Mahama  = sum(activity_type == "rally" & Candidate == "John Mahama"), 
            interaction_Mahama  = sum(activity_type == "interaction" & Candidate == "John Mahama"), 
            official_business_Mahama  = sum(activity_type == "official_business" & Candidate == "John Mahama"), 
            courtesy_call_Addo  = sum(activity_type == "courtesy_call" & Candidate == "Nana Addo Dankwa Akuffo Addo"), 
            rally_Addo   = sum(activity_type == "rally" & Candidate == "Nana Addo Dankwa Akuffo Addo"), 
            interaction_Addo = sum(activity_type == "interaction" & Candidate == "Nana Addo Dankwa Akuffo Addo"), 
            official_business_Addo   = sum(activity_type == "official_business" & Candidate == "Nana Addo Dankwa Akuffo Addo"), 
            courtesy_call_2020_Mahama = sum(activity_type == "courtesy_call" & election == 2020 & Candidate == "John Mahama"), 
            rally_2020_Mahama = sum(activity_type == "rally" & election == 2020 & Candidate == "John Mahama"), 
            interaction_2020_Mahama = sum(activity_type == "interaction" & election == 2020 & Candidate == "John Mahama"), 
            official_business_2020_Mahama = sum(activity_type == "official_business" & election == 2020 & Candidate == "John Mahama"), 
            courtesy_call_2016_Mahama = sum(activity_type == "courtesy_call" & election == 2016 & Candidate == "John Mahama"), 
            rally_2016_Mahama = sum(activity_type == "rally" & election == 2016 & Candidate == "John Mahama"), 
            interaction_2016_Mahama = sum(activity_type == "interaction" & election == 2016 & Candidate == "John Mahama"), 
            official_business_2016_Mahama = sum(activity_type == "official_business" & election == 2016 & Candidate == "John Mahama"), 
            courtesy_call_2020_Addo = sum(activity_type == "courtesy_call" & election == 2020 & Candidate == "Nana Addo Dankwa Akuffo Addo"), 
            rally_2020_Addo = sum(activity_type == "rally" & election == 2020 & Candidate == "Nana Addo Dankwa Akuffo Addo"), 
            interaction_2020_Addo = sum(activity_type == "interaction" & election == 2020 & Candidate == "Nana Addo Dankwa Akuffo Addo"), 
            official_business_2020_Addo = sum(activity_type == "official_business" & election == 2020 & Candidate == "Nana Addo Dankwa Akuffo Addo"), 
            courtesy_call_2016_Addo = sum(activity_type == "courtesy_call" & election == 2016 & Candidate == "Nana Addo Dankwa Akuffo Addo"), 
            rally_2016_Addo = sum(activity_type == "rally" & election == 2016 & Candidate == "Nana Addo Dankwa Akuffo Addo"), 
            interaction_2016_Addo = sum(activity_type == "interaction" & election == 2016 & Candidate == "Nana Addo Dankwa Akuffo Addo"), 
            official_business_2016_Addo = sum(activity_type == "official_business" & election == 2016 & Candidate == "Nana Addo Dankwa Akuffo Addo"))

#match to ghana boundaries

# map stop by type
constituency_stops = ghana_boundaries_a %>%
  dplyr::rename(Constituency = NAME_2) %>%
  dplyr::left_join(., wide_data, by = "Constituency") %>%
  replace(is.na(.), 0) %>%
  dplyr::select(-ID_0, -ISO, -NAME_0, -ID_1, -NAME_1, -NL_NAME_2, -VARNAME_2, -TYPE_2, -ENGTYPE_2, -ID_2)

figure1a = ggplot(constituency_stops) +
  geom_sf(size = 0.25, aes(fill = courtesy_call)) +
  scale_fill_gradient(name = "Courtesy call", low = "light grey", high = "black") +
  theme_bw() 
ggsave(figure1a, filename = "figure1a.png", 
       dpi = 400, width = 12, height = 10, 
       path = "~/OneDrive - University of Bristol/Post-doc/Ghana Campaign Stops")


figure1b = ggplot(constituency_stops) +
  geom_sf(size = 0.25, aes(fill = rally)) +
  scale_fill_gradient(name = "Rally", low = "light grey", high = "black") +
  theme_bw() 
ggsave(figure1b, filename = "figure1b.png", 
       dpi = 400, width = 12, height = 10, 
       path = "~/OneDrive - University of Bristol/Post-doc/Ghana Campaign Stops")

figure1c = ggplot(constituency_stops) +
  geom_sf(size = 0.25, aes(fill = interaction)) +
  scale_fill_gradient(name = "Interaction", low = "light grey", high = "black") +
  theme_bw() 
ggsave(figure1c, filename = "figure1c.png", 
       dpi = 400, width = 12, height = 10, 
       path = "~/OneDrive - University of Bristol/Post-doc/Ghana Campaign Stops")


figure1d = ggplot(constituency_stops) +
  geom_sf(size = 0.25, aes(fill = official_business)) +
  scale_fill_gradient(name = "Official business", low = " light grey", high = "black") +
  theme_bw()
ggsave(figure1d, filename = "figure1d.png", 
       dpi = 400, width = 12, height = 10, 
       path = "~/OneDrive - University of Bristol/Post-doc/Ghana Campaign Stops")



#map total by election and leader
constituency_stops = constituency_stops %>%
  dplyr::mutate("2016_total" = courtesy_call_2016 + rally_2016 + interaction_2016 + official_business_2016, 
                "2020_total" = courtesy_call_2020 + rally_2020 + interaction_2020 + official_business_2020, 
                "Mahama_2016_total" = courtesy_call_2016_Mahama + rally_2016_Mahama + interaction_2016_Mahama + official_business_2016_Mahama, 
                "Mahama_2020_total" = courtesy_call_2020_Mahama + rally_2020_Mahama + interaction_2020_Mahama + official_business_2020_Mahama,
                "Addo_2016_total" = courtesy_call_2016_Addo + rally_2016_Addo + interaction_2016_Addo + official_business_2016_Addo,
                "Addo_2020_total" = courtesy_call_2020_Addo + rally_2020_Addo + interaction_2020_Addo + official_business_2020_Addo)
  
figure2a = ggplot(constituency_stops) +
  geom_sf(size = 0.25, aes(fill = Mahama_2016_total)) +
  scale_fill_gradient(name = "Mahama 2016", low = "light grey", high = "black") +
  theme_bw() 
ggsave(figure2a, filename = "figure2a.png", 
       dpi = 400, width = 12, height = 10, 
       path = "~/OneDrive - University of Bristol/Post-doc/Ghana Campaign Stops")

figure2b = ggplot(constituency_stops) +
  geom_sf(size = 0.25, aes(fill = Mahama_2020_total)) +
  scale_fill_gradient(name = "Mahama 2020", low = "light grey", high = "black") +
  theme_bw() 
ggsave(figure2b, filename = "figure2b.png", 
       dpi = 400, width = 12, height = 10, 
       path = "~/OneDrive - University of Bristol/Post-doc/Ghana Campaign Stops")

figure2c = ggplot(constituency_stops) +
  geom_sf(size = 0.25, aes(fill = Addo_2016_total)) +
  scale_fill_gradient(name = "Addo 2016", low = "light grey", high = "black") +
  theme_bw() 
ggsave(figure2c, filename = "figure2c.png", 
       dpi = 400, width = 12, height = 10, 
       path = "~/OneDrive - University of Bristol/Post-doc/Ghana Campaign Stops")

figure2d = ggplot(constituency_stops) +
  geom_sf(size = 0.25, aes(fill = Addo_2020_total)) +
  scale_fill_gradient(name = "Addo 2020", low = "light grey", high = "black") +
  theme_bw() 
ggsave(figure2d, filename = "figure2d.png", 
       dpi = 400, width = 12, height = 10, 
       path = "~/OneDrive - University of Bristol/Post-doc/Ghana Campaign Stops")




#add election data

#read constituency data
constit = read_xlsx("2016-2020 CAMPAIGN STOPS (Responses).xlsx", sheet = 2) %>%
  dplyr::mutate(Constituency = stringr::str_replace(Constituency, "-" ," "), #replace "-" with space
                Constituency = ifelse(Constituency == "Ajumako Enyan-Essiam", "Ajumako Enyan-Esiam", Constituency), 
                Constituency = ifelse(Constituency == "Akwapim South", "Akwapem South", Constituency), 
                Constituency = ifelse(Constituency == "Asene Akroso Manso", "Asene Akroso-Manso", Constituency), 
                Constituency = ifelse(Constituency == "Bortianor Ngleshie-Amanfrom", "Bortianor Ngleshie Amanfro", Constituency), 
                Constituency = ifelse(Constituency == "Dade Kotopon", "Dadekotopon", Constituency), 
                Constituency = ifelse(Constituency == "Damango", "Damongo", Constituency), 
                Constituency = ifelse(Constituency == "Hemang Lower Denkyira", "Hermang Lower Denkyira", Constituency), 
                Constituency = ifelse(Constituency == "Kwesimintsim[9]", "Kwesimintsim", Constituency),
                Constituency = ifelse(Constituency == "Odotobri", "Odotobiri", Constituency), 
                Constituency = ifelse(Constituency == "Manso Edubia", "Manso Adubia", Constituency), 
                Constituency = ifelse(Constituency == "Odododiodio", "Odododiodioo", Constituency), 
                Constituency = ifelse(Constituency == "Bole", "Bole Bamboi", Constituency), 
                Constituency = ifelse(Constituency == "Evalue Gwira Evalue Gwira", "Evalue Ajomoro-Gwira", Constituency), 
                Constituency = ifelse(Constituency == "Lambussie", "Lambussie Karni", Constituency), 
                Constituency = ifelse(Constituency == "Lower Manya", "Lower Manya Krobo", Constituency), 
                Constituency = ifelse(Constituency == "Nalerigu", "Nalerigu Gambaga", Constituency), 
                Constituency = ifelse(Constituency == "Nsuta Kwamang", "Nsuta Kwamang-Beposo", Constituency), 
                Constituency = ifelse(Constituency == "Upper Manya Krobo", "Upper Manya", Constituency), 
                Constituency = ifelse(Constituency == "Weija", "Weija Gbawe", Constituency), 
                Constituency = ifelse(Constituency == "Ayawaso West", "Ayawaso West Wuogon", Constituency), 
                Constituency = ifelse(Constituency == "Komenda Edina-Eguafo-Abirem", "KEEA", Constituency), 
                Constituency = ifelse(Constituency == "Abokobi Madina", "Madina", Constituency),
                Constituency = ifelse(Constituency == "Akropong", "Akwapem North", Constituency), 
                Constituency = ifelse(Constituency == "Banda Ahenkro", "Banda", Constituency), 
                Constituency = ifelse(Constituency == "Upper Manya", "Upper Manya Krobo", Constituency)) #replace "-" with space

#fuzzy matching of constituency names between constituency boundaries and campaign stops

#create strings to match on
#match_key_stops = as.data.frame(unique(constituency_stops$Constituency)) %>%
#  drop_na() %>%
#  dplyr::rename(match_key = "unique(constituency_stops$Constituency)") %>%
#  dplyr::arrange(match_key)

#match_key_constit = as.data.frame(unique(constit$Constituency)) %>%
#  dplyr::rename(match_key = "unique(constit$Constituency)") %>%
#  dplyr::arrange(match_key)

#calculate match distance
#constit_match = stringdist_join(match_key_stops, match_key_constit,
#                               by = "match_key", #change to common name in both datasets
#                               mode = "left",
#                               ignore_case = FALSE, 
#                               method = "lv", 
#                               max_dist = 10, 
#                               distance_col = "dist") %>%
#  group_by(match_key.x) %>%
#  slice_min(order_by = dist, n = 1) %>%
#  dplyr::rename(stops = match_key.x, constit = match_key.y)

#subset exact matches - we've now got 272 exact matches meaning 24 constituencies without stops. 
#constit_is_match = constit_match %>% dplyr::filter(dist == 0) 

#subset non exact matches - 13 which I beleive are non-matches
#constit_is_not_match = constit_match %>% dplyr::filter(dist > 0)
#setdiff(match_key_stops$match_key, match_key_constit$match_key)


#merge constituency level data

model_data = constituency_stops %>%
  dplyr::left_join(constit, by = "Constituency") %>%
  dplyr::mutate(swing_constituency = ifelse(`2016 Margin` <15, 1, 0),
                core_constituency = ifelse(`2016 Margin` >15, 1, 0),
                all_stops_2020 = courtesy_call_2020 + rally_2020 + interaction_2020 + 
                official_business_2020, 
                Mahama_homeland = ifelse(Region == "Savannah", 1, 0), 
                Addo_homeland = ifelse(Region == "Eastern", 1, 0), 
                Addo_2020_stop = ifelse(Addo_2020_total > 0, 1, 0), 
                Mahama_2020_stop = ifelse(Mahama_2020_total > 0, 1, 0), 
                any_2020_stop = ifelse(all_stops_2020 > 0, 1, 0), 
                is_rally_2020 = ifelse(rally_2020 > 0, 1, 0), 
                is_courtesey_call_2020 = ifelse(courtesy_call_2020 > 0, 1, 0), 
                is_interaction_2020 = ifelse(interaction_2020 > 0, 1, 0), 
                is_official_business_2020 = ifelse(official_business_2020 > 0, 1, 0),
                is_official_business_2016 = ifelse(official_business_2016 > 0, 1, 0),
                is_incumbent_2016 = ifelse(`2016 Victor` == "NDC", 1, 0), 
                is_incumbent_2020 = ifelse(`2020 Victor` == "NPP", 1, 0), 
                constituency_change = ifelse(`2016 Victor` != `2020 Victor`, 1, 0)) %>%
  dplyr::rename(registered_voters_2020 = "Registered Voters 2020") 


#map of seat that changed between 2016 and 2020 
change = model_data %>% dplyr::filter(constituency_change == 1) %>%
  st_drop_geometry() %>%
  dplyr::select(Constituency, `2016 Victor`, `2020 Victor`)


unique(model_data)



#simple regressions 

#H1: marginal constituencies more likely to get visit? 
#model1 = fepois(all_stops_2020 ~ swing_constituency | Region, 
#               data = model_data)
model2 = fepois(all_stops_2020 ~ registered_voters_2020 | Region, 
                  data = model_data)
model3 = fepois(all_stops_2020 ~ swing_constituency + registered_voters_2020 | Region, 
                  data = model_data)

msummary(list(model2, model3), 
             stars = c('*' = .1,  '**' = .05, '***' = .01), coef_omit = "Region")

#run linear probability model - doesn't say much because most get a stop
model4 = feols(any_2020_stop ~ swing_constituency | Region, data = model_data)

model5 = feols(any_2020_stop ~ registered_voters_2020 | Region, 
                  data = model_data)
model6 = feols(any_2020_stop ~ swing_constituency + registered_voters_2020 | Region, 
                  data = model_data)



#H3: Campaigns are more likely to target core constituencies (2016_VoteMargin >15 and 2016_Victor) with rallies. 
#who won election in 2016? NPP (& in 2016) Mahama NDC .. 

npp = model_data %>%
  dplyr::filter(`2016 Victor` == "NPP")
ndc = model_data %>%
  dplyr::filter(`2016 Victor` == "NDC")

#filter swing and core constituencies
npp_core = npp %>% st_drop_geometry() %>%
  dplyr::filter(core_constituency == 1) %>%
  dplyr::select(Constituency)

ndc_core = ndc %>% st_drop_geometry() %>%
  dplyr::filter(core_constituency == 1) %>%
  dplyr::select(Constituency)

#swing
swing = model_data %>% st_drop_geometry() %>%
  dplyr::filter(swing_constituency == 1) %>%
  dplyr::select(Constituency)

appendix = bind_cols(npp_core, ndc_core, swing) %>%
  `colnames<-`(c("npp_core", "ndc_core", "swing"))

model7 = fepois(all_stops_2020 ~ core_constituency + registered_voters_2020 | Region, 
                  data = npp)

model8 = fepois(all_stops_2020 ~ core_constituency + registered_voters_2020 | Region, 
                data = ndc)

#LPM for is rally 2020
model9 = feols(is_rally_2020 ~ core_constituency + registered_voters_2020 | Region, 
                  data = npp)

model10 = feols(is_rally_2020 ~ core_constituency + registered_voters_2020 | Region, 
                  data = ndc)

msummary(list(model7, model8, model9, model10), 
         stars = c('*' = .1,  '**' = .05, '***' = .01), coef_omit = "Region")


#H4: Campaigns are more likely to target swing constituencies (2016_VoteMargin <15) with Official_Business, Courtesy, or Interaction. 
model11 = feols(is_rally_2020 ~ swing_constituency + registered_voters_2020 | Region, 
                data = ndc)

msummary(list(model11), 
         stars = c('*' = .1,  '**' = .05, '***' = .01), coef_omit = "Region")


#H5: campaigns are more likely to target core constituencies (2016_VoteMargins >15 + 2016_Victor) with Courtesy or Interaction.

model12 = feols(is_courtesey_call_2020 ~ core_constituency + registered_voters_2020 | Region, data = npp)
model13 = feols(is_courtesey_call_2020 ~ core_constituency + registered_voters_2020 | Region, data = ndc)

model14 = feols(is_interaction_2020 ~ core_constituency + registered_voters_2020 | Region, data = npp)
model15 = feols(is_interaction_2020 ~ core_constituency + registered_voters_2020 | Region, data = ndc)

msummary(list(model12, model13, model14, model15), 
         stars = c('*' = .1,  '**' = .05, '***' = .01), coef_omit = "Region")

#H6: Incumbents are more likely to target swing constituencies (2016_VoteMargin <15) with Official_Business. 

#just focus in the even type - incumbent is NPP Addo - subset for addo stops ... 

model16 = feols(is_official_business_2020 ~ is_incumbent_2020 + swing_constituency + 
                  is_incumbent_2020:swing_constituency + registered_voters_2020 | Region, 
                data = model_data)

model17 = feols(is_official_business_2016 ~ is_incumbent_2016 + swing_constituency + 
                  is_incumbent_2016:swing_constituency + registered_voters_2020 | Region, 
                data = model_data)

msummary(list(model16, model17), 
         stars = c('*' = .1,  '**' = .05, '***' = .01), coef_omit = "Region")

#run is for Mahama (2016) and Addo (2020) with the 2016 election results determining whether swing .. 
#create list of core constituencies for both parties .. based on 2016 vote margin 
#H7: break out for each election ... 

model18 = feols(is_official_business_2020 ~ is_incumbent_2020 + registered_voters_2020 | Region, 
                data = model_data)

model19 = feols(is_official_business_2016 ~ is_incumbent_2016 + registered_voters_2020 | Region, 
                data = model_data)

msummary(list(model18, model19), 
         stars = c('*' = .1,  '**' = .05, '***' = .01), coef_omit = "Region")


#H8: John Mahama is not more likely to visit his ethnic homeland. 
#model4 = fepois(Mahama_2020_total ~ Mahama_homeland, 
#                data = model_data)
model20 = fepois(Mahama_2020_total ~ Mahama_homeland + registered_voters_2020, 
                  data = model_data)
#check extensive margin 
model21 = feols(Mahama_2020_stop ~ Mahama_homeland + registered_voters_2020, 
                data = model_data, )


#H9: John Mahama is not more likely to visit his ethnic homeland. 
#model22 = fepois(Addo_2020_total ~ Addo_homeland, 
#                data = model_data)
model22 = fepois(Addo_2020_total ~ Addo_homeland + registered_voters_2020, 
                data = model_data)

#check extensive margin
model23 = feols(Addo_2020_stop ~ Addo_homeland + registered_voters_2020, 
            data = model_data, se = "hetero")

msummary(list(model20, model21, model22, model23), 
         stars = c('*' = .1,  '**' = .05, '***' = .01), coef_omit = "Region")




