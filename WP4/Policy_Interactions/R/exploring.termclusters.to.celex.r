
library("dplyr")
library("knitr")

Q1.term.df <- read.csv(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/07.Q1.term.data.csv")
Q2.term.df <- read.csv(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/07.Q2.term.data.csv")

# Query 1 document data
mpa.policy.notext.df <- read.csv(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/01_MPApolicy.notextdf.csv")

Q1.policy.df <- 
  mpa.policy.notext.df %>%
  select(-work,-type)

# query 2 document data 
# EU mpa directives search: 
EU.mpa.termsearch<- read.csv(file = "C:/Users/aeljor/OneDrive - Danmarks Tekniske Universitet/Skrivebord/mpa4sustainability/WP4/Policy_Interactions/data/01_EUmpa.searchterm.CELEX2.csv")

n_distinct(EU.mpa.termsearch$CELEX)
#100
# lets join the new mpa search documents to their associated document data
Q2.policy.df<-
  EU.mpa.termsearch %>%
  filter(CELEX != "32006R1967R(01)") %>% #removing this celex bc it is a Corrigendum to a regulation that was already pulled and it is tech. not within the legal act types
  select(-work,-type) # we dont need this info anymore

n_distinct(Q2.policy.df$CELEX)
#99

#----------- Query 1 --------------------

### join term network data with the celexs
Q1.term.celex <- 
  Q1.policy.df %>%
  distinct(CELEX,labels)

Q1.eurovoc.clusters <-
  Q1.term.df %>%
  distinct(name, component)

Q1.cluster.profiles <- left_join(Q1.eurovoc.clusters,Q1.term.celex, by = c("name"="labels"))


Q1.cluster.profiles2 <-
  Q1.cluster.profiles %>%
  group_by(CELEX,component) %>%
  mutate(component.terms.n = n_distinct(name)) %>%
  ungroup() %>%
  group_by(CELEX) %>%
  mutate(total.terms = n_distinct(name)) %>%
  ungroup() %>%
  mutate(cluster.prop = component.terms.n/total.terms) %>%
  distinct(CELEX,component,cluster.prop)


Q1.cluster.profiles2 %>%
  group_by(component) %>%
  summarise( n = n_distinct(CELEX))
#  component     n
#         1     7
#         2    12
#         3     5
#         4     5
#         5     3
#         6     1

Q1.cluster.profiles2 %>%
  group_by(CELEX) %>%
  summarise( n = n_distinct(component)) %>%
  arrange(desc(n))

#   CELEX          n
# 32019D0867     5
# 32013R1380     4
# 32014R0508     3
# 32022R2343     3
# 31984D0132     2
# 32010D0814     2
# 32012D0091     2
# 32021R1139     2
# 32008L0056     1
# 32013D1386     1
# 32013L0030     1
# 32018R0120     1
# 32019R0124     1
# 32020R0123     1
# 32021D1796     1
# 32021R0092     1
# 32022R0109     1
# 32022R2473     1

Q1.cluster.profiles2 %>%
  filter(cluster.prop >=0.5) %>%
  arrange(desc(cluster.prop))
  

#component CELEX      cluster.prop
#         1 32008L0056        1    
#         1 32013D1386        1    
#         1 32013L0030        1    
#         1 32021D1796        1    
#         5 32022R2473        1    
#         2 32019R0124        1    
#         2 32022R0109        1    
#         2 32021R0092        1    
#         2 32018R0120        1    
#         2 32020R0123        1    
#         3 32021R1139        0.9  
#         4 32012D0091        0.857
#         1 31984D0132        0.833
#         2 32022R2343        0.778
#         4 32010D0814        0.75 
#         3 32014R0508        0.714
#         3 32013R1380        0.5 

#----------- Query 2 --------------------

### join term network data with the celexs
Q2.term.celex <- 
  Q2.policy.df %>%
  distinct(CELEX,labels)

Q2.eurovoc.clusters <-
  Q2.term.df %>%
  distinct(name, component)

Q2.cluster.profiles <- left_join(Q2.eurovoc.clusters,Q2.term.celex, by = c("name"="labels"))


Q2.cluster.profiles2 <-
  Q2.cluster.profiles %>%
  group_by(CELEX,component) %>%
  mutate(component.terms.n = n_distinct(name)) %>%
  ungroup() %>%
  group_by(CELEX) %>%
  mutate(total.terms = n_distinct(name)) %>%
  ungroup() %>%
  mutate(cluster.prop = component.terms.n/total.terms) %>%
  distinct(CELEX,component,cluster.prop)  %>%
  arrange(desc(CELEX)) %>%
  mutate(cluster.prop = round(cluster.prop, digits = 2))

Q2.cluster.profiles2%>%
  select(CELEX,component,cluster.prop) %>%
  kable(.,"latex")
  

Q2.cluster.profiles2 %>%
  group_by(component) %>%
  summarise( n = n_distinct(CELEX)) %>%
  kable(.,"latex")

Q2.cluster.profiles2 %>%
  group_by(CELEX) %>%
  summarise( n = n_distinct(component)) %>%
  arrange(desc(n))


Q2.term.search <- 
  Q2.policy.df %>%
  distinct(CELEX,search.term)

x <- Q2.term.search %>% 
  right_join(.,Q2.cluster.profiles2, by = c("CELEX"))

# lets combine the titles that overlap:
unique(x$search.term)

xz <- 
  x %>%
  mutate(new.title = case_when(search.term ==  "sites of community importance*?" ~ "habitats directive*?" ,
                               search.term == "site of community importance*?" ~ "habitats directive*?",
                               search.term == "special areas of conservation*?" ~ "habitats directive*?",
                               search.term == "special protection area*?" ~ "birds directive*?",
                               search.term == "marine protected area*?" ~ "ospar*?",
                               TRUE ~ search.term ))
unique(xz$new.title)

#"barcelona convention*?"
#
#"sites of community importance*?"
#"site of community importance*?" 
#"habitats directive*?"           
#"special areas of conservation*?"
#
#"birds directive*?"              
#"special protection area*?"
#
#"ospar*?"                        
#"marine protected area*?" 
#
#"world heritage site*?"          
#
# "helcom*?"                       
#
# "specially protected area*?" 

y <- 
  xz %>%
  group_by(component,new.title) %>%
  summarise(n = n_distinct(CELEX)) %>%
  mutate(total.n=sum(n)) %>%
  mutate(prop.celex = n/total.n)


y%>%
  filter(prop.celex >=0.25)


Q2.cluster.profiles2 %>%
  ungroup() %>%
  group_by(component)%>%
  summarise(n=n_distinct(CELEX))


xxx <-
  Q2.cluster.profiles2 %>%
  filter(cluster.prop >=0.5) %>%
  arrange(desc(cluster.prop))

Q2.cluster.profiles2 %>%
  filter(cluster.prop >=0.5 & 
           cluster.prop < 1) %>%
  arrange(desc(cluster.prop))


less.than.fifty <-
  anti_join(Q2.cluster.profiles2,xxx, by = c("CELEX"))
  
  
