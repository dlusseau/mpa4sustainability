
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


Q1.cluster.profiles2 %>%
  group_by(CELEX) %>%
  summarise( n = n_distinct(component)) %>%
  arrange(desc(n))

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

y <- 
  x %>%
  group_by(component,search.term) %>%
  summarise(n = n_distinct(CELEX))
  
