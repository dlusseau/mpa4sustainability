### R code to engage in topic modelling of EUR-LEX relevant text

library(stringr)
library(stm)
Q1.text<-readRDS("C:/Users/David/OneDrive - Danmarks Tekniske Universitet/MPA4Sustainability/WP4/05_Q1.preptext.stm")
Q2.text<-readRDS("C:/Users/David/OneDrive - Danmarks Tekniske Universitet/MPA4Sustainability/WP4/05_Q2.preptext.stm")


Q1.stm<-stm(Q1.text$documents,Q1.text$vocab,data=Q1.text$meta,K=0,init.type="Spectral")
save(Q1.stm,file="C:/Users/David/OneDrive - Danmarks Tekniske Universitet/MPA4Sustainability/WP4/Q1_stm.Rdata")

Q2.stm<-stm(Q2.text$documents,Q2.text$vocab,data=Q2.text$meta,K=0,init.type="Spectral")
save(Q2.stm,file="C:/Users/David/OneDrive - Danmarks Tekniske Universitet/MPA4Sustainability/WP4/Q2_stm.Rdata")

#predictions

library(stminsights)
library(ggraph)

labels<-labelTopics(Q1.stm, topics = 1:76, n =2) 

Q1.net <- get_network(model = Q1.stm,
                         method = 'simple',
                         labels = labels$prob,
                         cutoff = 0.05,
                         cutiso = FALSE)

ggraph(Q1.net, layout = 'fr') +
  geom_edge_link(
    aes(edge_width = weight),
    label_colour = '#fc8d62',
    edge_colour = '#377eb8') +
  geom_node_point(size = 4, colour = 'black')  +
  geom_node_label(
    aes(label = name, size = props),
    colour = 'black',  repel = TRUE, alpha = 0.85) +
  scale_size(range = c(2, 10), labels = scales::percent) +
  labs(size = 'Topic Proportion',  edge_width = 'Topic Correlation') +
  scale_edge_width(range = c(.1, 3)) +
  theme_graph()

#prep <- estimateEffect(1:39 ~ celex, Q1.stm,meta = Q1.text$meta, uncertainty = "Global") #?
#plot(prep, covariate = "lockdown", topics = c(22,25,17,4),model = paris.stm, labeltype="lift", method = "pointestimate",n=5)


