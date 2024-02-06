

library(tidyverse)
library(readxl)


bit_dataset <- read_xlsx(file.choose())

bit_dataset <- filter(bit_dataset, !is.na(NO.))

bit_dataset <- filter(bit_dataset, str_detect(bit_dataset$`SHORT TITLE`, "BIT"))

unique(bit_dataset$STATUS)   ## remove the terminated BITs

bit_dataset<- filter(bit_dataset, STATUS !="Terminated")

bit_dataset <- mutate(bit_dataset, Year = str_extract(bit_dataset$`SHORT TITLE`, pattern = "\\d{4}"))

bit_dataset <- mutate(bit_dataset, agreement = 1)

cumulative_BITs <- group_by(bit_dataset, Year) %>%
                   summarise(bits_by_year = sum(agreement)) %>%
                   mutate(cumulative_bits_by_year = cumsum(bits_by_year))


ggplot(data = cumulative_BITs, mapping = aes(x = Year, y =  cumulative_bits_by_year) )+
  geom_point(size = 3)+theme_bw()+
  labs(
    title = "Cumulative BITs across time",
    x = "Year",
    y = "Cumulative BITs",
    caption = "Data source: UN investment policy hub") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
    plot.title = element_text(hjust = 0.50, face="bold",size=18))+
  scale_x_discrete(breaks = c(1960, 1970, 1980,1990, 2000, 2010, 2020, 2030),
                   labels = c(1960, 1970, 1980,1990, 2000, 2010, 2020, 2030))
  ggsave("cumulative_bits.png")


