install.packages("devtools")
library("devtools")
devtools::install_github("rfsaldanha/microdatasus")

dados <- fetch_datasus(year_start = 2019, year_end = 2019, information_system = "SIM-DOINF")
dados <- process_sim(dados)

sim <- dados %>%
  select(TIPOBITO,IDADEdias,SEXO,LOCOCOR,IDADEMAE, ESCMAE) %>%
  na.omit()  

view(sim)

library(ggplot2)

sim %>%
  count(ESCMAE) %>%  
  ggplot(aes(x = ESCMAE, y = n,
             fill = ESCMAE,
             color = "ESCMAE",
             label = n))+
  geom_bar(stat = "identity")+
  geom_label(color = "black")+
  labs(title = "Anos de escolaridade da mãe - MG",
       subtitle = "ano de 2018",
       x = "",
       y = "",
       caption = "Elaborado por Observatório da Primeira Infância - UFJF com dados do DATASUS")+
  theme_minimal()+
  theme(legend.position = "none")
