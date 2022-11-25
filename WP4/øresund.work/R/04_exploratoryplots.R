

# Clear work space -------------------------------------------------------------
rm(list = ls())
Sys.setenv(LANG = "en") # change the language to english 

# Load libraries ---------------------------------------------------------------

library("dplyr")
library("ggplot2")
library("lubridate")
library("patchwork")

# Define functions -------------------------------------------------------------

# No defined function for this script

# Load data --------------------------------------------------------------------


DK.metadata <- read.csv(file ="C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/02.DKdocmetadata.clean.csv")
SE.metadata <- read.csv(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/data/02.SEdocmetadata.clean.csv")

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

se.labs <- c("fiske" ="fiske (n=257)", "jakt"="jakt (n=93)", "sjöfart"="sjöfart (n=199)")

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
                                       Ressort == "Uddannelses- og Forskningsministeriet" |  Ressort == "Børne- og Undervisningsministeriet" ~ "Ministry of Education" ,
                                       Ressort == "Beskæftigelsesministeriet" ~ "Ministry of Labour" ,
                                       Ressort == "Klima-, Energi- og Forsyningsministeriet" ~ "Ministry of Climate, Energy and Supply" ,
                                       Ressort == "Justitsministeriet" ~ "Ministry of Justice" ,
                                       Ressort == "Forsvarsministeriet" ~ "Ministry of Defence" ,
                                       Ressort == "Skatteministeriet" ~ "Ministry of Taxation" ,
                                       Ressort == "Kulturministeriet" ~ "Ministry of Culture" ,
                                       Ressort == "Sundhedsministeriet" ~ "Ministry of Health" ,
                                       Ressort == "Indenrigs- og Boligministeriet" ~ "Ministry of the Interior and Housing" ,
                                       Ressort == "Finansministeriet" ~ "Ministry of Finance" ,
                                      # Ressort == "Børne- og Undervisningsministeriet" ~ "Ministry of Children and Education" ,
                                       Ressort == "Kirkeministeriet" ~ "Ministry of the Church" ,
                                       Ressort == "Folketinget" ~ "The Danish parliament",
                                       Ressort == "NA" ~ "NA")) %>%
  mutate(country = "Denmark")  %>%
  group_by(search.term, ministry.english, country) %>%
  summarise(n=n_distinct(url)) %>%
  ungroup() %>%
  mutate(ministry.english = as.factor(ministry.english)) 


unique(dep.leg$organ.cut)

# "Arbetsmarknadsdepartementet"    "Ministry of Labour"
# "Civildepartementet"             "Ministry of Civil Affairs"
# "Finansdepartementet"            "Ministry of Finance"
# "Fiskeristyrelsen"               "The Fisheries Board"
# "Försvarsdepartementet"          "Ministry of Defence"
# "Industridepartementet"           Ministry of Industry
# "Infrastrukturdepartementet"       Ministry of Infrastructure
# "Inrikesdepartementet"           Ministry of the Interior
# "Jordbruksdepartementet"         Ministry of Agriculture
# "Justitiedepartementet"         Ministry of Justice
# "Kammarkollegiet"             Chamber college
# "Kommunikationsdepartementet"   Ministry of Communications
# "Kulturdepartementet"          Ministry of Culture
# "Landsbygdsdepartementet"     Ministry of Rural Affairs
#  "Miljö- och samhällsbyggnadsdepartementet	-->    Ministry of the Environment and Community Development            
#  "Miljödepartementet"         Ministry of the Environment
# "Näringsdepartementet"        Ministry of Commerce
# "riksb"              ??            
# "Socialdepartementet"        Ministry of Social Affairs
# "Utbildningsdepartementet"   Ministry of Education
# "Utrikesdepartementet"        Ministry of Foreign Affairs
# "Statsrådsberedningen"       The Cabinet Committee
# NA  

SE.metadata %>%
  mutate(organ.cut = str_extract(organ,"[:alpha:]+")) %>%
  mutate(organ.cut = as.character(organ.cut)) %>%
  mutate(ministry.english = case_when( organ.cut == "Arbetsmarknadsdepartementet" ~ "Ministry of Labour",
                                       organ.cut == "Civildepartementet" ~ "Ministry of Civil Affairs",
                                       organ.cut == "Finansdepartementet" ~ "Ministry of Finance",
                                    #   organ.cut == "Fiskeristyrelsen" ~ "The Fisheries Board",
                                       organ.cut == "Försvarsdepartementet" ~ "Ministry of Defence",
                                       organ.cut == "Industridepartementet" ~ "The Ministry of Business and Industry", # sewden just industry this is the name of DK
                                       organ.cut == "Infrastrukturdepartementet" ~ "Ministry of Infrastructure",
                                       organ.cut == "Inrikesdepartementet" ~ "Ministry of the Interior",
                                       organ.cut == "Jordbruksdepartementet" | organ.cut == "Fiskeristyrelsen" ~ "Ministry of Food, Agriculture and Fisheries",
                                       organ.cut == "Justitiedepartementet" ~ "Ministry of Justice",
                                       organ.cut == "Kammarkollegiet" ~ "Chamber college",
                                       organ.cut == "Kommunikationsdepartementet" ~ "Ministry of Communications",
                                       organ.cut == "Kulturdepartementet" ~ "Ministry of Culture",
                                       organ.cut == "Landsbygdsdepartementet" ~ "Ministry of Rural Affairs",
                                     #  organ.cut == "Miljö" ~ "Ministry of the Environment and Community Development",
                                       organ.cut == "Miljödepartementet" | organ.cut == "Miljö"  ~ "Ministry of the Environment",
                                       organ.cut == "Näringsdepartementet" ~ "Ministry of Commerce",
                                       organ.cut == "Socialdepartementet" ~ "Ministry of Social Affairs",
                                       organ.cut == "Utbildningsdepartementet" ~ "Ministry of Education",
                                       organ.cut == "Utrikesdepartementet" ~ "Ministry of Foreign Affairs",
                                       organ.cut == "Statsrådsberedningen" ~ "The Cabinet Committee",
                                       organ.cut == "NA" ~ "NA",
                                       organ.cut == "riksb" ~ "riksb")) %>%
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
  theme(axis.text.x = element_text(angle=45,hjust=1,size = 12),
        axis.text.y = element_text(size = 12),
        title = element_text(face="bold", size = 12),
        legend.position = "none",
        axis.title.x=element_blank())

ggsave("C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/øresund.work/Results/ministry.plots.png")


SE.metadata %>%
  mutate(organ.cut = str_extract(organ,"[:alpha:]+")) %>%
  mutate(organ.cut = as.character(organ.cut)) %>%
  mutate(ministry.english = case_when( organ.cut == "Arbetsmarknadsdepartementet" ~ "Ministry of Labour",
                                       organ.cut == "Civildepartementet" ~ "Ministry of Civil Affairs",
                                       organ.cut == "Finansdepartementet" ~ "Ministry of Finance",
                                       #   organ.cut == "Fiskeristyrelsen" ~ "The Fisheries Board",
                                       organ.cut == "Försvarsdepartementet" ~ "Ministry of Defence",
                                       organ.cut == "Industridepartementet" ~ "The Ministry of Business and Industry", # sewden just industry this is the name of DK
                                       organ.cut == "Infrastrukturdepartementet" ~ "Ministry of Infrastructure",
                                       organ.cut == "Inrikesdepartementet" ~ "Ministry of the Interior",
                                       organ.cut == "Jordbruksdepartementet" | organ.cut == "Fiskeristyrelsen" ~ "Ministry of Food, Agriculture and Fisheries",
                                       organ.cut == "Justitiedepartementet" ~ "Ministry of Justice",
                                       organ.cut == "Kammarkollegiet" ~ "Chamber college",
                                       organ.cut == "Kommunikationsdepartementet" ~ "Ministry of Communications",
                                       organ.cut == "Kulturdepartementet" ~ "Ministry of Culture",
                                       organ.cut == "Landsbygdsdepartementet" ~ "Ministry of Rural Affairs",
                                       #  organ.cut == "Miljö" ~ "Ministry of the Environment and Community Development",
                                       organ.cut == "Miljödepartementet" | organ.cut == "Miljö"  ~ "Ministry of the Environment",
                                       organ.cut == "Näringsdepartementet" ~ "Ministry of Commerce",
                                       organ.cut == "Socialdepartementet" ~ "Ministry of Social Affairs",
                                       organ.cut == "Utbildningsdepartementet" ~ "Ministry of Education",
                                       organ.cut == "Utrikesdepartementet" ~ "Ministry of Foreign Affairs",
                                       organ.cut == "Statsrådsberedningen" ~ "The Cabinet Committee",
                                       organ.cut == "NA" ~ "NA",
                                       organ.cut == "riksb" ~ "riksb")) %>%
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
  
  