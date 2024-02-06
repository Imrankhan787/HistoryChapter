


### Multilateral trade negotiation membership grew with each round

library(tidyverse)
library(readxl)
library(lubridate)

multilateral_tradeRounds <- read_xlsx(file.choose())

## The table is copied from word to excel. So three empty rows have to removed.

multilateral_tradeRounds <- filter(multilateral_tradeRounds, !is.na(Year))

multilateral_tradeRounds$Year[5:8] <-c(1961, 1967, 1979, 1994)


multilateral_tradeRounds$Year<- year(str_c(multilateral_tradeRounds$Year, "01", "01",sep = "-"))

ggplot(data = multilateral_tradeRounds, mapping = aes(x = Year, y = Countries))+
  geom_point(size = 8)+
  geom_smooth(se = FALSE)+
  geom_vline(xintercept = 1994, linetype = "solid",color = "red") +
  geom_area(data = subset(multilateral_tradeRounds, Year >= 1994), aes(y = 170), fill = "red", alpha = 0.15)+
  geom_area(data = subset(multilateral_tradeRounds, Year <= 1994), aes(y = 170), fill = "blue", alpha = 0.15)+
  geom_text(x = 2020, y = 150, size= 3.5, label = "Current WTO \nmembership: 164")+
  geom_text(x = 1998, y = 110, size =3.5,  label = "Uruguay Round:\n Countries participated :123")+
  geom_text(x = 1974, y = 115, size= 3.5, label = "Tokyo Round:\n Countries participated:102")+
  geom_text(x = 1974, y = 115, size= 3.5, label = "Tokyo Round:\n Countries participated:102")+
  geom_text(x = 1967, y = 75, size= 3.5, label = "Kennedy Round:\n Countries participated:62")+
  geom_text(x = 1966, y = 24, size= 2.5, label = "Dillion Round:\n Countries participated:26")+
  geom_text(x = 1956, y = 36, size= 2.5, label = "Geneva Round:\n Countries participated:26")+
  geom_text(x = 1951, y = 50, size= 2.5, label = "Torquay Round:\n Countries participated:38")+
  geom_text(x = 1954, y = 9, size= 2.5, label = "Annecy Round:\n Countries participated:13")+
  geom_text(x = 1970, y = 150, size= 8, color= "light blue", label = "GATT era")+
  geom_text(x = 2006, y = 70, size= 8,color ="pink", label = "WTO era")+
  labs(
    title = "Progressive growth in membership of multilateral trade rounds",
    caption = "Data source: WTO",
    y = "Number of countries"
  )+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.50, face="bold",size=18))
ggsave("traderounds.png")
  
