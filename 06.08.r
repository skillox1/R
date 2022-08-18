library(gapminder)
library(tidyverse)
library(ggthemes)


data(gapminder)


View(gapminder)

#Gráfico 1

gapminder %>% 
  filter(year == 2007) %>%
  mutate(pop_mi= pop/10^6) %>% 
  select(country,pop_mi) %>% 
  arrange(desc(pop_mi)) %>%  
  top_n(5) %>% 
  ggplot(aes(x=reorder(country,-pop_mi), y=pop_mi)) +
  geom_col(fill="orange", width = .7) +  
  theme_hc()+
  #theme_economist()
  scale_y_continuous(limits=c(0,1500)) +
  #scale_y_continuous(trans = "log") 
  labs(title="China é o país mais populoso do mundo", subtitle = "(milhões de pessoas 2007)",
       y = "", x = "", caption = "Fonte: Gapminder")+
  theme(axis.ticks.x = element_blank())
#axis.text.x = element_text(size=10)     
  
#Gráfico 2

gapminder %>% 
  filter(year == 2007, continent!= "Oceania") %>% 
  mutate(cat_expec = ifelse(lifeExp >=75,"+75","-75")) %>% 
  group_by(continent,cat_expec) %>%  
  summarise(avg_gdp = mean(gdpPercap, na.rm = TRUE)) %>% 
  ggplot()+
  aes(x=reorder(continent,avg_gdp), avg_gdp, fill = cat_expec )+
  geom_col(position="dodge") +              
  theme_economist() + 
  labs(title="Pib e Expectativa de Vida", subtitle = "(pib per capita medio 2007)",
     y = "", x = "", caption = "Fonte: Gapminder", fill = "Expectativa de Vida")+
  theme(axis.ticks.x = element_blank(),legend.position="left")       

# Gráfico 3

gapminder %>% 
  filter(year==2007) %>% 
  select(country,gdpPercap) %>% 
  top_n(20) %>% 
  ggplot(aes(x = gdpPercap, y = reorder(country,gdpPercap)))+
  geom_point()+
  theme_hc()+
  labs(title="Noruega Lidera ranking do PIB per Capita(2007)",
       y = "", x = "", caption = "Fonte: Gapminder")+
  theme(axis.ticks.x = element_blank(),legend.position="left")   

# Gráfico 4

gapminder %>% 
  filter(country %in% c ("Brazil","France","Belgium")) %>% 
  select(country,year,lifeExp) %>% 
  ggplot(aes(x = year,y = lifeExp,color = country))+
  geom_line(size = 1.5)+
  scale_x_continuous(breaks = seq(1952,2007,by = 5))+
  geom_hline(yintercept = gapminder$lifeExp[gapminder$country == "Brazil" & gapminder$year == 2007])+
  theme_hc()  
        

  