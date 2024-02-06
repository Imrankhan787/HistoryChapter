
### Multilateral trade negotiation membership grew with each round

library(tidyverse)
library(readxl)
library(lubridate)

multilateral_tradeRounds <- read_xlsx(file.choose())

## The table is copied from word to excel. So three empty rows have to removed.

ggplot(data = multilateral_tradeRounds, mapping = aes(x = Year, y = Fraction_multilatral))+
  geom_point(size = 5)+
  geom_line()+
  geom_vline(xintercept = 1994, linetype = "solid",color = "red") +
  geom_area(data = subset(multilateral_tradeRounds, Year >= 1994), aes(y = 1), fill = "red", alpha = 0.15)+
  geom_area(data = subset(multilateral_tradeRounds, Year <= 1994), aes(y = 1), fill = "blue", alpha = 0.15)+
  geom_text(x = 2020, y = 0.77, size= 3.5, label = "Current WTO \nmembership: 164")+
  geom_text(x = 1998, y = 0.60, size =3.5,  label = "Uruguay Round:\n Countries participated :123")+
  geom_text(x = 1977, y = 0.60, size= 3.5, label = "Tokyo Round:\n Countries participated:102")+
  geom_text(x = 1967, y = 0.45, size= 3.5, label = "Kennedy Round:\n Countries participated:62")+
  geom_text(x = 1966, y = 0.20, size= 2.5, label = "Dillion Round:\n Countries participated:26")+
  geom_text(x = 1956, y = 0.27, size= 2.5, label = "Geneva Round:\n Countries participated:26")+
  geom_text(x = 1951, y = 0.52, size= 2.5, label = "Torquay Round:\n Countries participated:38")+
  geom_text(x = 1951, y = 0.15, size= 2.5, label = "Annecy Round:\n Countries participated:13")+
  geom_text(x = 1970, y = 0.80, size= 8, color= "light blue", label = "GATT era")+
  geom_text(x = 2006, y = 0.25, size= 8,color ="pink", label = "WTO era")+
  labs(
    title = "Growing fraction of countries cooperating multilaterally",
    caption = "Data source: WTO & UN",
    y = "Fraction of countries\n cooperating multilaterally"
  )+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.50, face="bold",size=18))
ggsave("growing_fraction.png")
