# Data importation
setwd("C:/Users/eva-1/OneDrive - TAC ECONOMICS/Documents/ENSAE/Mémoire de recherche")
data_2019a = read.csv("PE_vacancies/eofcoltoexport_2019a.csv.gz")
data_2019b = read.csv("PE_vacancies/eofcoltoexport_2019b.csv.gz")

library(dplyr)

# Recoding of variables corresponding to salaries

  data = data_2019b #data_2019a then data_2019b
  
  data$dc_typesalaire = case_when(data$dn_salaireminimum <= 100 & data$dn_salaireminimum > 0 ~ "H", data$dn_salaireminimum <= 10000 & data$dn_salaireminimum > 100 ~ "M", data$dn_salaireminimum > 10000 ~ "A",data$dn_salaireminimum ==0 ~"X")
  
  data$dn_salairemaximum = data$dn_salairemaximum %>% na_if(0)
  data$dn_salaireminimum = data$dn_salaireminimum %>% na_if(0)
  
  data$salmin_year = case_when(data$dc_typesalaire=="H" ~ data$dn_salaireminimum*data$dn_dureetravailhebdoheures*365.25/7,
                               data$dc_typesalaire=="M" ~ data$dn_salaireminimum*12,
                               data$dc_typesalaire=="X" ~ data$dn_salaireminimum)
  
  data$salmax_year =  case_when(data$dc_typesalaire=="H" ~ data$dn_salairemaximum*data$dn_dureetravailhebdoheures*365.25/7,
                                data$dc_typesalaire=="M" ~ data$dn_salairemaximum*12,
                                data$dc_typesalaire=="X" ~ data$dn_salairemaximum)
  
  data$salmin_etp = case_when(data$dc_typesalaire=="H" ~ data$dn_salaireminimum*35*365.25/7,
                              data$dc_typesalaire=="M" ~ data$dn_salaireminimum/data$dn_dureetravailhebdoheures*35*12,
                              data$dc_typesalaire=="A" ~ data$dn_salaireminimum,
                              data$dc_typesalaire=="X" ~ data$dn_salaireminimum)
  
  data$salmax_etp = case_when(data$dc_typesalaire=="H" ~ data$dn_salairemaximum*35*365.25/7,
                              data$dc_typesalaire=="M" ~ data$dn_salairemaximum/data$dn_dureetravailhebdoheures*35*12,
                              data$dc_typesalaire=="A" ~ data$dn_salairemaximum,
                              data$dc_typesalaire=="X" ~ data$dn_salairemaximum)

  data_2019b = data
  
# Recoding of skills

ref_spe = read.csv("referentiels/pr00_ppx005_ref_specificites.csv")

data = data_2019b

col_names=gsub("specificite","specificites",colnames(data))
colnames(data)=col_names
col_names = gsub("specificites_exigee","specificite_exigee",colnames(data))
colnames(data)=col_names
list_spe = grep("specificites",names(data), value = T)
data = data[,c('kc_offre','dd_datecreationreport','dc_intituleoffre','dc_descriptifoffre','dc_rome_id','salmin_year','salmax_year','salmin_etp','salmax_etp','dc_naf2','dc_communelieutravail',list_spe)]

for (i in list_spe){
  data[,i] = ref_spe$dc_lblspecificites[match(data[,i],ref_spe$kc_specificites)]
}

data_2019b = data

# Keeping only observations with stated minimum salary

summary(data_2019b$salmin_etp)
data_2019a_f = data_2019a %>% filter(is.na(salmin_etp)==F)
data_2019b_f = data_2019b %>% filter(is.na(salmin_etp)==F)

data_2019a_f$salmean_etp = case_when(is.na(data_2019a_f$salmax_etp)==F ~ (data_2019a_f$salmin_etp+data_2019a_f$salmax_etp)/2,
                             is.na(data_2019a_f$salmax_etp)==T ~ data_2019a_f$salmin_etp)

list_rome = unique(data_2019a_f$dc_rome_id) #531 ROME

summary(data_2019a_f$salmean_etp)

#SMIC 2019 = 18 254,64

data_2019a_f = data_2019a_f%>% filter(salmin_etp>=18254.64)
data_2019b_f = data_2019b_f%>% filter(salmin_etp>=18254.64)
summary(data_2019b_f$salmin_etp)

rm(data, data_2019a, data_2019b)

data_2019a_f$specificites <- apply(data_2019a_f[, grepl("specificites", names(data_2019a_f))], 1, paste, collapse = ".")
data_2019b_f$specificites <- apply(data_2019b_f[, grepl("specificites", names(data_2019b_f))], 1, paste, collapse = ".")

data_2019a_f = data_2019a_f[,c('kc_offre','dd_datecreationreport','dc_intituleoffre','dc_descriptifoffre','dc_rome_id','specificites','salmin_etp','salmax_etp','dc_communelieutravail','dc_naf2')]
data_2019b_f = data_2019b_f[,c('kc_offre','dd_datecreationreport','dc_intituleoffre','dc_descriptifoffre','dc_rome_id','specificites','salmin_etp','salmax_etp','dc_communelieutravail','dc_naf2')]

# Export data

write.table(data_2019a_f, "data_2019a_full.csv", sep = ';', fileEncoding = 'latin1')
write.table(data_2019b_f, "data_2019b_full.csv", sep = ';', fileEncoding = 'latin1')

##################### Descriptive statistics ###################################

#Full dataset
data_f = rbind(data_2019a_f, data_2019b_f)
rm(data_2019a_f, data_2019b_f)

#Salary distribution of PE data (2019)
library(ggplot2)
data_f$salmin_log = log(data_f$salmin_etp)

ggplot(data_f, aes(x = salmin_log))+
  geom_histogram(aes(y=..density..), position="identity", alpha=0.5, fill='#99CCFF')+
  geom_density(alpha=0.6, color='#003366')+
  labs(x ='Log of salary', y = "Density")+
  xlim(c(9,11))+
  theme_classic()

list_rome = unique(data_f$dc_rome_id) #531 ROME

#Length of skills requirement

library(quanteda)
data_f$specificites = gsub("NA.","", data_f$specificites) 
data_f$length_specificites = ntoken(data_f$specificites)
data_f$length_description = ntoken(data_f$dc_descriptifoffre)
summary(data_f$length_specificites)
summary(data_f$length_description)

test = data_f %>% filter(specificites!="NA")
summary(test$length_specificites)
nrow(test)/nrow(data_f)*100

#Sector coverage

pe_sector = data_f %>% filter(is.na(data_f$dc_naf2)==F) %>%
  group_by(dc_naf2) %>%
  summarise(nb_per_sector = n())

ref_naf = read.csv('referentiels/pr00_ppx005_ref_nafrev2.csv')
ref_naf_en = readxl::read_excel('referentiels/ref_naf21_en.xlsx')

pe_sector$code_naf21 = ref_naf$dc_codenafa21[match(pe_sector$dc_naf2, ref_naf$kc_nafrev2)]
pe_sector$libellé_naf21 = ref_naf_en$lbl_naf21_en[match(pe_sector$code_naf21, ref_naf_en$code_naf21)]

pe_sector = pe_sector %>%
  group_by(libellé_naf21) %>%
  summarise(nb_per_sector = n())

pe_sector$percent_per_sector = round(pe_sector$nb_per_sector / sum(pe_sector$nb_per_sector)*100,digits = 2)

library(RColorBrewer)

colourCount = length(unique(pe_sector$libellé_naf21))
getPalette = colorRampPalette(brewer.pal(11, "RdBu"))

#Number of vacancies by sector

plot_sector = ggplot(data=pe_sector, aes(x=percent_per_sector, y=reorder(libellé_naf21, percent_per_sector), fill=libellé_naf21))+
  geom_bar(stat="identity")+
  geom_text(
    aes(label = paste(percent_per_sector,"%",sep="")), 
    hjust = 0) +
  scale_fill_manual(values=getPalette(colourCount), guide='none')+
  scale_x_continuous(breaks=seq(0,100, by=10), limits=c(0,50))+
  labs(x="% of vacancies per sector",y="Sector")+
  theme_classic()

ggsave("Number of vacancies by sector PE data.png", plot = plot_sector, width=25,height=12, units='cm' )

#Example of job vacancies

data_s = data_f[sample(nrow(data_f), size=5), ]

data_s[,'dc_descriptifoffre']

data_s[,'specificites']
