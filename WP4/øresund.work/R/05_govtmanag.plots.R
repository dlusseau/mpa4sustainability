

# Clear work space -------------------------------------------------------------
rm(list = ls())
Sys.setenv(LANG = "en") # change the language to english 

# Load libraries ---------------------------------------------------------------

library("dplyr")
library("ggplot2")
library("lubridate")
library("patchwork")
library("kableExtra")
library("stringr")

# Load data --------------------------------------------------------------------


DK.metadata <- read.csv(file ="C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/02.DKdocmetadata.clean.csv")
SE.metadata <- read.csv(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/02.SEdocmetadata.clean.csv")

# Plot Legislation queries over time (not used in the report, just for some slides) ---------------

dk.labs <- c("fiskeri" ="fiskeri (n=1011)", "jagt"="jagt (n=346)", "søtrafik"="søtrafik (n=8)")

DK.metadata %>%
  group_by(search.term) %>%
  summarise(n=n_distinct(url))

DKplot <- 
  DK.metadata %>%
  group_by(search.term, År) %>%
  summarise(n=n_distinct(url)) %>%
  ungroup() %>%
  mutate(search.term = str_replace_all(search.term, "sotrafik", "søtrafik")) %>%
  mutate(search.term = as.factor(search.term)) %>%
  ggplot(aes(fill=search.term, y=n, x=År)) + 
  geom_bar(position="stack", stat="identity") + 
  scale_x_continuous(breaks=seq(1886,2024,8), expand = c(0.02, .5)) +
  scale_y_continuous(breaks=seq(0,140,20), expand = c(0.02, .5)) + 
  theme_bw() +
  facet_wrap(~search.term,   nrow = 3,
             labeller = labeller(search.term = dk.labs))+
  ylab("Number of legislations") + 
  xlab(" ")+
  theme(legend.position = "bottom")+ 
  theme(axis.text.x = element_text(angle=45,hjust=1),
        strip.text.x = element_text(face="bold", size = 12),
        title = element_text(face="bold", size = 12),
        legend.position = "none") + 
  labs(title = "Danish Legislation")

se.labs <- c("fiske" ="fiske (n=105)", "jakt"="jakt (n=35)", "sjöfart"="sjöfart (n=125)")

SE.metadata %>%
  group_by(search.term) %>%
  summarise(n=n_distinct(doc.id))

SEplot <- 
  SE.metadata %>%
  mutate(year=year(datum)) %>%
  mutate(search.term = as.factor(search.term)) %>%
  group_by(search.term, year) %>%
  summarise(n=n_distinct(doc.id)) %>%
  ungroup() %>%
  mutate(search.term = str_replace_all(search.term, "sjofart", "sjöfart")) %>%
  ggplot(aes(fill=search.term, y=n, x=year)) + 
  geom_bar(position="stack", stat="identity")  + 
  scale_x_continuous(breaks=seq(1798,2024,8), expand = c(0.001, .5)) +
  scale_y_continuous(breaks=seq(0,20,5), expand = c(0.001, .5)) +
  theme_bw() +
  facet_wrap(~search.term,   nrow = 3,
             labeller = labeller(search.term = se.labs))+
  ylab(" ") + 
  xlab(" ")+
  theme(legend.position = "bottom") + 
  theme(axis.text.x = element_text(angle=45,hjust=1),
        strip.text.x = element_text(face="bold", size = 12),
        title = element_text(face="bold", size = 12),
        legend.position = "none") + 
  labs(title = "Swedish Legislation")


DKplot + SEplot + 
  plot_annotation(tag_levels = 'A')




unique(DK.metadata$Ressort)
# "Ministeriet for Fødevarer, Landbrug og Fiskeri"  Ministry of Food, Agriculture and Fisheries
# "Erhvervsministeriet"                         The Ministry of Business and Industry           
# "Udenrigsministeriet"                         Ministry of Foreign Affairs   
# "Transportministeriet"                        Ministry of Transport
# "Miljøministeriet"                            Ministry of the Environment    
# "Statsministeriet"                            The Prime Minister's Office
# "Uddannelses- og Forskningsministeriet"       Ministry of Education and Research
# "Beskæftigelsesministeriet"                   Ministry of Employment
# "Klima-, Energi- og Forsyningsministeriet"    Ministry of Climate, Energy and Supply 
# "Justitsministeriet"                          Ministry of Justice
# "Skatteministeriet"                           Ministry of Taxation
# "Forsvarsministeriet"                         Ministry of Defence
# "Kulturministeriet"                           Ministry of Culture
# "Sundhedsministeriet"                         Ministry of Health
# "Indenrigs- og Boligministeriet"              Ministry of the Interior and Housing
# "Finansministeriet"                           Ministry of Finance
# "Børne- og Undervisningsministeriet"          Ministry of Children and Education
# "Kirkeministeriet"           Ministry of the Church
# "Folketinget"                   the Danish parliament

dep.leg.dek <- 
  DK.metadata %>%
  mutate(ministry.english = case_when( Ressort == "Ministeriet for Fødevarer, Landbrug og Fiskeri" ~ "Ministry of Food, Agriculture and Fisheries",
                                       Ressort == "Erhvervsministeriet" ~ "The Ministry of Business and Industry",
                                       Ressort == "Udenrigsministeriet" ~ "Ministry of Foreign Affairs",
                                       Ressort == "Transportministeriet" ~ "Ministry of Transport",
                                       Ressort == "Miljøministeriet" ~ "Ministry of the Environment",
                                       Ressort == "Statsministeriet" ~ "The Prime Minister's Office",
                                       Ressort == "Uddannelses- og Forskningsministeriet" ~ "Ministry of Education and Research",
                                       Ressort == "Børne- og Undervisningsministeriet" ~ "Ministry of Children and Education" ,
                                       Ressort == "Beskæftigelsesministeriet" ~ "Ministry of Labour" ,
                                       Ressort == "Klima-, Energi- og Forsyningsministeriet" ~ "Ministry of Climate, Energy and Supply" ,
                                       Ressort == "Justitsministeriet" ~ "Ministry of Justice" ,
                                       Ressort == "Forsvarsministeriet" ~ "Ministry of Defence" ,
                                       Ressort == "Skatteministeriet" ~ "Ministry of Taxation" ,
                                       Ressort == "Kulturministeriet" ~ "Ministry of Culture" ,
                                       Ressort == "Sundhedsministeriet" ~ "Ministry of Health" ,
                                       Ressort == "Indenrigs- og Boligministeriet" ~ "Ministry of the Interior and Housing" ,
                                       Ressort == "Finansministeriet" ~ "Ministry of Finance" ,
                                       Ressort == "Kirkeministeriet" ~ "Ministry of the Church" ,
                                       Ressort == "Folketinget" ~ "The Danish parliament",
                                       Ressort == "NA" ~ "NA")) %>%
  mutate(country = "Denmark")  %>%
  group_by(search.term, ministry.english, country) %>%
  summarise(n=n_distinct(url)) %>%
  ungroup() %>%
  mutate(ministry.english = as.factor(ministry.english)) 

xx <- 
  SE.metadata %>%
  mutate(organ.cut = str_extract(organ,"[:alpha:]+")) %>%
  mutate(organ.cut = as.character(organ.cut))

 unique(xx$organ.cut)
 #
 #[1] "Infrastrukturdepartementet"  "Justitiedepartementet"       "Finansdepartementet"         "Näringsdepartementet"        "Försvarsdepartementet"       NA                           
 #[7] "Statsrådsberedningen"        "Miljödepartementet"          "Kulturdepartementet"         "Utrikesdepartementet"        "Arbetsmarknadsdepartementet" "Landsbygdsdepartementet"    
 #[13] "Fiskeristyrelsen"            "Socialdepartementet"         "Utbildningsdepartementet"    "riksb"   

 # "Infrastrukturdepartementet"       Ministry of Infrastructure
 # "Justitiedepartementet"         Ministry of Justice
 # "Finansdepartementet"            "Ministry of Finance"
 # "Näringsdepartementet"        Ministry of Commerce
 # "Försvarsdepartementet"          "Ministry of Defence"
 # "Statsrådsberedningen"       The Cabinet Committee
 #  "Miljödepartementet"         Ministry of the Environment
 # "Kulturdepartementet"          Ministry of Culture
 # "Utrikesdepartementet"        Ministry of Foreign Affairs
# "Arbetsmarknadsdepartementet"    "Ministry of Labour"
 # "Landsbygdsdepartementet"     Ministry of Rural Affairs
 # "Fiskeristyrelsen"               "The Fisheries Board"
 # "Socialdepartementet"        Ministry of Social Affairs
 # "Utbildningsdepartementet"   Ministry of Education
 # "riksb"              ??            
 
 
# "Civildepartementet"             "Ministry of Civil Affairs"
# "Industridepartementet"           Ministry of Industry
# "Inrikesdepartementet"           Ministry of the Interior
# "Jordbruksdepartementet"         Ministry of Agriculture
# "Kammarkollegiet"             Chamber college
# "Kommunikationsdepartementet"   Ministry of Communications
#  "Miljö- och samhällsbyggnadsdepartementet	-->    Ministry of the Environment and Community Development            
# NA  

 unique(SE.metadata$organ)
 #[1] "Infrastrukturdepartementet RST TM" 
 # "Infrastrukturdepartementet RST US"
 
 # "Finansdepartementet S3"  
 # [7] "Finansdepartementet S4"     
 
 #[3] "Justitiedepartementet L3" The unit for property law and association law (L1)
 #[5] "Justitiedepartementet L6" The Basic Law Unit (L6)
          
 # "Näringsdepartementet RSN"         
 #       "Försvarsdepartementet"            
 #[9] ""                                  "Statsrådsberedningen"             
 #[11] "Miljödepartementet"                "Kulturdepartementet"              
 #[13] "Justitiedepartementet L1"          "Justitiedepartementet"            
 #[15] "Finansdepartementet"               "Finansdepartementet S2"           
 #[17] "Justitiedepartementet L4"          "Justitiedepartementet BIRS"       
 #[19] "Infrastrukturdepartementet RSED E" "Justitiedepartementet DÅ"         
 #[21] "Finansdepartementet FPM"           "Finansdepartementet SPN BB"       
 #[23] "Utrikesdepartementet"              "Arbetsmarknadsdepartementet ARM"  
 #[25] "Justitiedepartementet L2"          "Finansdepartementet ESA"          
 #[27] "Finansdepartementet S1"            "Näringsdepartementet RSL"         
 #[29] "Utrikesdepartementet UDH"          "Näringsdepartementet"             
 #[31] "Landsbygdsdepartementet"           "Fiskeristyrelsen"                 
 #[33] "Socialdepartementet"               "Socialdepartementet /Lo"          
 #[35] "Utbildningsdepartementet"          "riksb"                            
 #[37] "Finansdepartementet SFÖ"           "Arbetsmarknadsdepartementet AA"   
 #[39] "Justitiedepartementet L5"    

 
 SE.metadata %>%
  mutate(organ.cut = str_extract(organ,"[:alpha:]+")) %>%
  mutate(organ.cut = as.character(organ.cut)) %>%
  mutate(ministry.english = case_when( # 2023 updated departments for Sweden
    organ.cut == "Arbetsmarknadsdepartementet" ~ "Ministry of Labour",
    organ.cut == "Finansdepartementet" ~ "Ministry of Finance",
    organ.cut == "Försvarsdepartementet" ~ "Ministry of Defence",
    organ.cut == "Justitiedepartementet" ~ "Ministry of Justice",
    organ.cut == "Kulturdepartementet" ~ "Ministry of Culture",
    organ.cut == "Socialdepartementet" ~ "Ministry of Health & Social Affairs" ,
    organ.cut == "Utbildningsdepartementet" ~ "Ministry of Education",
    organ.cut == "Utrikesdepartementet" ~ "Ministry of Foreign Affairs",
    organ.cut == "Fiskeristyrelsen" ~  "Fisheries Agency",
    organ.cut == "Statsrådsberedningen" ~ "Prime Minister's Office",
    organ.cut == "NA" ~ "NA",
    organ.cut == "riksb" ~ "riksb",
    #these two are now together:
    organ.cut == "Landsbygdsdepartementet" | organ.cut == "Infrastrukturdepartementet" ~ "Ministry of Rural Affairs & Infrastructure",
    #these three are now together:
    organ.cut == "Miljödepartementet" | organ.cut == "Näringsdepartementet"  ~ "Ministry of the Climate and Enterprise" ))  %>%
  mutate(country = "Sweden") %>%
  group_by(search.term, ministry.english, country) %>%
  summarise(n=n_distinct(doc.id)) %>%
  rbind(dep.leg.dek) %>%
  mutate(ministry.english = as.factor(ministry.english)) %>%
  ungroup() %>%
  mutate(search.term = case_when( search.term == "fiske"  ~ "Fisheries" ,
                                  search.term == "fiskeri"  ~ "Fisheries",
                                  search.term == "jagt"  ~ "Hunting",
                                  search.term == "jakt" ~ "Hunting",
                                  search.term == "sotrafik"  ~ "Maritime traffic",
                                  search.term == "sjofart"~ "Maritime traffic" )) %>%
  group_by(country, search.term) %>%
  mutate(total.n = sum(n)) %>%
  mutate('Proportion of legislation' = n/total.n) %>%
  ggplot(aes( y=`Proportion of legislation`, x=ministry.english,  fill = country)) + 
  geom_bar(position="dodge", stat="identity") +
  facet_wrap(~search.term, ncol = 1) + 
  scale_fill_manual(values = c("#d1050c", "#004B87")) +
  theme(axis.text.x = element_text(angle=45,hjust=1,size = 20),
        axis.text.y = element_text(size = 20),
        title = element_text( size = 20),
        legend.position = "none",
        strip.text.x = element_text(size = 20),
        axis.title.x=element_blank())

ggsave("C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/Results/ministry.plots.png",
width = 30,
height = 14.1,
units = c( "in"))

SE.metadata %>%
  mutate(organ.cut = str_extract(organ,"[:alpha:]+")) %>%
  mutate(organ.cut = as.character(organ.cut)) %>%
  mutate(ministry.english = case_when(    organ.cut == "Arbetsmarknadsdepartementet" ~ "Ministry of Labour",
                                          organ.cut == "Finansdepartementet" ~ "Ministry of Finance",
                                          organ.cut == "Försvarsdepartementet" ~ "Ministry of Defence",
                                          organ.cut == "Justitiedepartementet" ~ "Ministry of Justice",
                                          organ.cut == "Kulturdepartementet" ~ "Ministry of Culture",
                                          organ.cut == "Socialdepartementet" ~ "Ministry of Social Affairs" ,
                                          organ.cut == "Utbildningsdepartementet" ~ "Ministry of Education",
                                          organ.cut == "Utrikesdepartementet" ~ "Ministry of Foreign Affairs",
                                          organ.cut == "Fiskeristyrelsen" ~  "Fisheries Agency",
                                          organ.cut == "Statsrådsberedningen" ~ "Prime Minister's Office",
                                          organ.cut == "NA" ~ "NA",
                                          organ.cut == "riksb" ~ "riksb",
                                          #these two are now together:
                                          organ.cut == "Landsbygdsdepartementet" | organ.cut == "Infrastrukturdepartementet" ~ "Ministry of Rural Affairs & Infrastructure",
                                          #these three are now together:
                                          organ.cut == "Miljödepartementet" | organ.cut == "Näringsdepartementet"  ~ "Ministry of the Climate and Business" )) %>%
  mutate(country = "Sweden") %>%
  group_by(search.term, ministry.english, country) %>%
  summarise(n=n_distinct(doc.id)) %>%
  rbind(dep.leg.dek) %>%
  mutate(ministry.english = as.factor(ministry.english)) %>%
  ungroup() %>%
  mutate(search.term = case_when( search.term == "fiske"  ~ "fisheries" ,
                                  search.term == "fiskeri"  ~ "fisheries",
                                  search.term == "jagt"  ~ "hunting",
                                  search.term == "jakt" ~ "hunting",
                                  search.term == "sotrafik"  ~ "maritime traffic",
                                  search.term == "sjofart"~ "maritime traffic" )) %>%
  group_by(search.term, country) %>%
  summarise(n=sum(n))


SE.metadata %>%
  mutate(organ.cut = str_extract(organ,"[:alpha:]+")) %>%
  mutate(organ.cut = as.character(organ.cut)) %>%
  mutate(ministry.english = case_when(    organ.cut == "Arbetsmarknadsdepartementet" ~ "Ministry of Labour",
                                          organ.cut == "Finansdepartementet" ~ "Ministry of Finance",
                                          organ.cut == "Försvarsdepartementet" ~ "Ministry of Defence",
                                          organ.cut == "Justitiedepartementet" ~ "Ministry of Justice",
                                          organ.cut == "Kulturdepartementet" ~ "Ministry of Culture",
                                          organ.cut == "Socialdepartementet" ~ "Ministry of Social Affairs" ,
                                          organ.cut == "Utbildningsdepartementet" ~ "Ministry of Education",
                                          organ.cut == "Utrikesdepartementet" ~ "Ministry of Foreign Affairs",
                                          organ.cut == "Fiskeristyrelsen" ~  "Fisheries Agency",
                                          organ.cut == "Statsrådsberedningen" ~ "Prime Minister's Office",
                                          organ.cut == "NA" ~ "NA",
                                          organ.cut == "riksb" ~ "riksb",
                                          #these two are now together:
                                          organ.cut == "Landsbygdsdepartementet" | organ.cut == "Infrastrukturdepartementet" ~ "Ministry of Rural Affairs & Infrastructure",
                                          #these three are now together:
                                          organ.cut == "Miljödepartementet" | organ.cut == "Näringsdepartementet"  ~ "Ministry of the Climate and Business" )) %>%
  mutate(country = "Sweden") %>%
  group_by(search.term, ministry.english, country) %>%
  summarise(n=n_distinct(doc.id)) %>%
  rbind(dep.leg.dek) %>%
  mutate(ministry.english = as.factor(ministry.english)) %>%
  ungroup() %>%
  mutate(search.term = case_when( search.term == "fiske"  ~ "Fisheries" ,
                                  search.term == "fiskeri"  ~ "Fisheries",
                                  search.term == "jagt"  ~ "Hunting",
                                  search.term == "jakt" ~ "Hunting",
                                  search.term == "sotrafik"  ~ "Maritime traffic",
                                  search.term == "sjofart"~ "Maritime traffic" )) %>%
  group_by(country, search.term) %>%
  mutate(total.n = sum(n)) %>%
  mutate('Proplegislation' = n/total.n) %>%
  group_by(search.term, country) %>%
  summarise(n=n_distinct(ministry.english))

SE.metadata %>%
  mutate(organ.cut = str_extract(organ,"[:alpha:]+")) %>%
  mutate(organ.cut = as.character(organ.cut)) %>%
  mutate(ministry.english = case_when(    organ.cut == "Arbetsmarknadsdepartementet" ~ "Ministry of Labour",
                                          organ.cut == "Finansdepartementet" ~ "Ministry of Finance",
                                          organ.cut == "Försvarsdepartementet" ~ "Ministry of Defence",
                                          organ.cut == "Justitiedepartementet" ~ "Ministry of Justice",
                                          organ.cut == "Kulturdepartementet" ~ "Ministry of Culture",
                                          organ.cut == "Socialdepartementet" ~ "Ministry of Social Affairs" ,
                                          organ.cut == "Utbildningsdepartementet" ~ "Ministry of Education",
                                          organ.cut == "Utrikesdepartementet" ~ "Ministry of Foreign Affairs",
                                          organ.cut == "Fiskeristyrelsen" ~  "Fisheries Agency",
                                          organ.cut == "Statsrådsberedningen" ~ "Prime Minister's Office",
                                          organ.cut == "NA" ~ "NA",
                                          organ.cut == "riksb" ~ "riksb",
                                          #these two are now together:
                                          organ.cut == "Landsbygdsdepartementet" | organ.cut == "Infrastrukturdepartementet" ~ "Ministry of Rural Affairs & Infrastructure",
                                          #these three are now together:
                                          organ.cut == "Miljödepartementet" | organ.cut == "Näringsdepartementet"  ~ "Ministry of the Climate and Business" )) %>%
  mutate(country = "Sweden") %>%
  group_by(search.term, ministry.english, country) %>%
  summarise(n=n_distinct(doc.id)) %>%
  rbind(dep.leg.dek) %>%
  mutate(ministry.english = as.factor(ministry.english)) %>%
  ungroup() %>%
  mutate(search.term = case_when( search.term == "fiske"  ~ "Fisheries" ,
                                  search.term == "fiskeri"  ~ "Fisheries",
                                  search.term == "jagt"  ~ "Hunting",
                                  search.term == "jakt" ~ "Hunting",
                                  search.term == "sotrafik"  ~ "Maritime traffic",
                                  search.term == "sjofart"~ "Maritime traffic" )) %>%
  group_by(country, search.term) %>%
  mutate(total.n = sum(n)) %>%
  mutate('Proplegislation' = n/total.n) %>%
  group_by(search.term, country) %>%
  slice_max(Proplegislation, n=1) %>%
  select(-n,-total.n)%>%
  kable(., "latex")
  




# now the governing authority for DK 
unique(DK.metadata$AdministrerendeMyndighed)
DK.metadata %>%
  mutate(country = "Denmark")  %>%
  group_by(search.term, AdministrerendeMyndighed, country) %>%
  summarise(n=n_distinct(url)) %>%
  ungroup() %>%
  group_by(country, search.term) %>%
  mutate(total.n = sum(n)) %>%
  mutate('Proportion of legislation' = n/total.n) %>%
  mutate(search.term = case_when( search.term == "fiskeri"  ~ "Fisheries",
                                  search.term == "jagt"  ~ "Hunting",
                                  search.term == "sotrafik"  ~ "Maritime traffic")) %>%
  group_by(country, search.term) %>%
  filter(!is.na(AdministrerendeMyndighed)) %>% # remove NAs for the plot
  ggplot(aes( y=`Proportion of legislation`, x=AdministrerendeMyndighed,  fill = country)) + 
  geom_bar(position="dodge", stat="identity") +
  facet_wrap(~search.term, ncol = 1) + 
  scale_fill_manual(values = c("#d1050c", "#004B87")) +
  theme(axis.text.x = element_text(angle=80,hjust=1,size = 18),
        axis.text.y = element_text(size = 20),
        title = element_text( size = 20),
        legend.position = "none",
        strip.text.x = element_text(size = 20),
        axis.title.x=element_blank())
ggsave("C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/Results/DK authority.plots.png", 
       width = 55, height =45, units = "cm",
       limitsize = FALSE)

unique(SE.metadata$organ)

SE.metadata %>%
  mutate(country = "Sweden")  %>%
  group_by(search.term, organ, country) %>%
  summarise(n=n_distinct(doc.id)) %>%
  ungroup() %>%
  group_by(country, search.term) %>%
  mutate(total.n = sum(n)) %>%
  mutate('Proportion of legislation' = n/total.n) %>%
  mutate(search.term = case_when( search.term == "fiske"  ~ "Fisheries",
                                  search.term == "jakt"  ~ "Hunting",
                                  search.term == "sjofart"  ~ "Maritime traffic")) %>%
  mutate(organ = case_when(organ == "" ~ "NA",
                           TRUE ~ organ)) %>%
  group_by(country,organ, search.term) %>%
  ggplot(aes( y=`Proportion of legislation`, x=organ,  fill = country)) + 
  geom_bar(position="dodge", stat="identity") +
  facet_wrap(~search.term, ncol = 1) + 
  scale_fill_manual(values = c( "#004B87")) +
  theme(axis.text.x = element_text(angle=80,hjust=1,size = 22),
        axis.text.y = element_text(size = 20),
        title = element_text( size = 20),
        legend.position = "none",
        strip.text.x = element_text(size = 20),
        axis.title.x=element_blank())

ggsave("C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/Results/SEauthority.plots.png", 
       width = 65, height =45, units = "cm",
       limitsize = FALSE)
