

library(tidyverse)

jocas_cols <- cols(
  url = col_character(),
  date_firstSeenDay = col_date(format = ""),
  date_scraping = col_character(),
  site_name = col_character(),
  site_child = col_character(),
  scrapingFailure_status = col_logical(),
  ID_JOCAS = col_character(),
  date_firstDisappearedDay = col_date(format = ""),
  date_lastSeenDay = col_date(format = ""),
  MATCH_url_ID = col_character(),
  MATCH_url = col_logical(),
  MATCH_dedup_ID = col_character(),
  MATCH_dedup = col_logical(),
  MATCH_dedup_Tensions_ID = col_character(),
  MATCH_dedup_Tensions = col_logical(),
  date_sitePublicationDay = col_date(format = ""),
  job_title = col_character(),
  job_ROME_code = col_character(),
  job_qualification = col_double(),
  contractType = col_character(),
  contractDuration_min = col_double(),
  contractDuration_max = col_double(),
  contractDuration_period = col_character(),
  contractDuration_value = col_double(),
  workTime_hours = col_character(),
  workTime_category = col_character(),
  workTime_value = col_double(),
  description_job = col_character(),
  description_profil = col_character(),
  description_entreprise = col_character(),
  description_full = col_character(),
  location_label = col_character(),
  location_zipcode = col_double(),
  location_departement = col_character(),
  location_country = col_character(),
  salary_min = col_double(),
  salary_max = col_double(),
  salary_period = col_character(),
  salary_value = col_double(),
  salary_hourly_mean = col_double(),
  salary_hourly_min = col_double(),
  salary_hourly_max = col_double(),
  entreprise_nom = col_character(),
  entreprise_siren = col_character(),
  entrepriseSecteur_NAF88 = col_character(),
  entrepriseSecteur_NAF21 = col_character(),
  partner_name = col_character(),
  partner_status = col_logical(),
  teleworking_accepted = col_logical(),
  teleworking_type = col_character(),
  teleworking_mentioned = col_logical(),
  experience_min = col_double(),
  experience_max = col_double(),
  education_level = col_character(),
  education_field = col_character(),
  ROME_LOC_FIRM = col_logical()
)


#Data importation from 2022-01 to 2022-04

extract_data = function(site,date){
  here::here("data",site,date) %>% list.files() %>% 
    map_chr(~ here::here("data",site,date, .x)) %>% 
    map_dfr(read_delim, delim=";", col_types = jocas_cols) %>% 
    janitor::clean_names()
}

list_dates = c("2022-01","2022-02","2022-03","2022-04")
list_sites = c('apec','api_poleemploi','cadreo','keljob','leboncoin','meteojob','cadremploi',
               'regionsjob_centre','regionsjob_nord','regionsjob_est','regionsjob_ouest','regionsjob_paca',
               'regionsjob_paris','regionsjob_rhonealpes','regionsjob_sudouest')
list_df = list()

for (site in list_sites){
  data = as.data.frame(do.call(rbind, lapply(list_dates, function(x)extract_data(site,x))))
  list_df[[site]] = data
}

jocas = as.data.frame(do.call(rbind,list_df))

#Keep observation with salary>=SMIC in 2022

jocas_smic = jocas %>% filter(salary_min>=19237.40)
hist(log(jocas_smic$salary_min))
#1,730,680 observations left

rm(data, jocas)

write.csv(jocas_smic, "~/skills/jocas_2022.csv", fileEncoding = "UTF-8")


#### Descriptive statistics ####

library(ggplot2)
library(quanteda)

# Salary descriptive statistics

library(stargazer)

jocas_salary = as.data.frame(jocas_smic[,c('salary_min','site_name')])
jocas_salary %>% split(. $site_name) %>% walk(~ stargazer(., type = "text", title = .$site_name, digits=0))

# Salary distribution

ggplot(jocas_smic, aes(x = log(salary_min)))+
  geom_histogram(aes(y=..density..), position="identity", alpha=0.5, fill='#99CCFF')+
  geom_density(alpha=0.6, color='#003366')+
  labs(title="Salary distribution from January 2022 to April 2022", x ='Log of salary', y = "Density")+
  xlim(c(9,13))+
  theme_classic()

ggplot(jocas_smic, aes(x = log(salary_min), color=site_name, fill=site_name))+
  geom_histogram(aes(y=..density..), position="identity", alpha=0.5)+
  geom_density(alpha=0.6)+
  labs(title="Salary distribution from January 2022 to April 2022", x ='Log of salary', y = "Density")+
  xlim(c(9,13))+
  scale_fill_brewer(palette = "RdBu")+
  scale_color_brewer(palette = "RdBu")+
  theme_classic()

sum(is.na(jocas[,'salary_min']))/nrow(jocas)*100 #70.77%
nrow(jocas[jocas$salary_min<19237.4,])/nrow(jocas)*100 #85.77%

# Number of ROME codes by source

jocas_rome = jocas_smic %>% group_by(site_name) %>% summarise(nb_rome=length(unique(job_rome_code)))

list_rome = unique(jocas_smic$job_rome_code) #525 ROME codes

ggplot(jocas_rome, aes(y=nb_rome, x=site_name, fill=site_name))+
  geom_bar(stat='identity', show.legend = F, width=0.5)+
  coord_flip()+
  geom_text(aes(label=nb_rome), hjust=0)+
  theme_classic()+
  scale_fill_brewer(palette = "RdBu")+
  labs(title = "Number of ROME codes by site", y="Number of ROME codes", x="Site name")

# Length of job description

jocas_token = jocas_smic %>% group_by(site_name) %>% summarise(length_description = mean(ntoken(description_full)))

ggplot(jocas_token, aes(y=length_description, x=site_name, fill=site_name))+
  geom_bar(stat='identity', show.legend = F, width=0.5)+
  coord_flip()+
  theme_classic()+
  scale_fill_brewer(palette = "RdBu")+
  labs(title = "Number of tokens in the job description by site", y="Number of tokens", x="Site name")


jocas_notna = jocas %>% filter(is.na(salary_min)==F)
nrow(jocas_notna[jocas_notna$salary_min<19237.4,])/nrow(jocas_notna)*100 #51.32

# Number of vacancies by source

jocas_count = jocas_smic %>% group_by(site_name) %>% summarise(nb_obs=n())

ggplot(jocas_count, aes(x="",y=nb_obs, fill=site_name))+
  geom_bar(stat='identity')+
  coord_polar('y', start=0)+
  labs(title = "Number of vacancies by site",x="",y="", fill="Site name")+
  scale_fill_brewer(palette = "RdBu")+
  geom_text(aes(label = scales::percent(nb_obs/sum(nb_obs), accuracy = 0.01)), position = position_stack(vjust=0.5)) +
  theme_void()

# Sector coverage

jocas_sector = jocas_smic %>% filter(is.na(jocas_smic$entreprise_secteur_naf21)==F) %>%
  group_by(entreprise_secteur_naf21) %>%
  summarise(nb_per_sector = n())
summary(as.factor(jocas_smic$entreprise_secteur_naf88))

jocas_sector$percent_per_sector = round(jocas_sector$nb_per_sector / sum(jocas_sector$nb_per_sector)*100,digits = 2)

#NAF nomemclature
nom_naf = readxl::read_excel('~/skills/Nomenclature_naf.xlsx')

jocas_sector$libellé = nom_naf$Libellé[match(jocas_sector$entreprise_secteur_naf21, nom_naf$Code)]

library(RColorBrewer)

colourCount = length(unique(jocas_sector$libellé))
getPalette = colorRampPalette(brewer.pal(11, "RdBu"))

ggplot(data=jocas_sector[1:21,], aes(x=percent_per_sector, y=reorder(libellé, percent_per_sector), fill=libellé))+
  geom_bar(stat="identity")+
  geom_text(
    aes(label = paste(percent_per_sector,"%",sep="")), 
    hjust = 0) +
  scale_fill_manual(values=getPalette(colourCount), guide='none')+
  scale_x_continuous(breaks=seq(0,100, by=10), limits=c(0,50))+
  labs(title = "Number of vacancies by sector",x="% of vacancies per sector",y="Sector")+
  theme_classic()

# Experience required
summary(jocas_smic$experience_min)
jocas_exp = jocas_smic %>% filter(experience_min<=10)
hist(jocas_exp$experience_min)
summary(jocas_exp$experience_min)

#Qualification required
summary(as.factor(jocas_smic$job_qualification))

#Examples of job vacancies
data_s = jocas_smic[sample(nrow(jocas_smic), size=5), ]
