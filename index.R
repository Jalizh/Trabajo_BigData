library(formattable)
library(gganimate)
library(ggplot2)
library(ggridges)
library(ggthemes)
library(gridExtra)
library(gt)
library(plotly)
library(readxl)
library(tidyverse)
library(treemap)


# Importamos los datos 
ue_pob <- read_excel("./datos/pob_ue.xlsx") %>%
  rename(y_2010 = "2010",
         y_2011 = "2011",
         y_2012 = "2012",
         y_2013 = "2013",
         y_2014 = "2014",
         y_2015 = "2015",
         y_2016 = "2016",
         y_2017 = "2017",
         y_2018 = "2018",
         y_2019 = "2019",
         y_2020 = "2020", 
         y_2021 = "2021")

# 1 Tabla básica 
ue_pob_t <- ue_pob %>% filter( Países != 'UE 27 (desde 2020)' & Países != 'UE 28')
formattable(ue_pob_t, list(
  y_2010 = color_tile("white", "orange"), 
  y_2011 = color_tile("white", "orange"), 
  y_2012 = color_tile("white", "orange"), 
  y_2013 = color_tile("white", "orange"), 
  y_2014 = color_tile("white", "orange"), 
  y_2015 = color_tile("white", "orange"), 
  y_2016 = color_tile("white", "orange"), 
  y_2017 = color_tile("white", "orange"), 
  y_2018 = color_tile("white", "orange"), 
  y_2019 = color_tile("white", "orange"), 
  y_2020 = color_tile("white", "orange"), 
  y_2021 = color_tile("white", "orange")
))

# Arreglamos los datos a formato long
df <- ue_pob %>%
  select(!y_2021) %>% 
  pivot_longer(cols = 2:12, names_to = "year", values_to = "pop")


# 2 Países por poblaciones en 2020 
g1 <- df %>% 
  filter(year == "y_2020" & Países != 'UE 27 (desde 2020)' & Países != 'UE 28') %>%
  mutate(pob = pop/1000000) 

g11 <- g1 %>% 
  ggplot( aes(x=Países, y=pob)) +
  geom_bar(stat="identity", fill="#f68060", alpha=.6, width=.4) +
  coord_flip() +
  xlab("") +
  theme_bw() + theme(axis.title = element_text(colour = "brown1"),
    plot.background = element_rect(fill = "bisque")) +labs(title = "Países UE por población",
    x = NULL, subtitle = "Unidad en millones ")

g12 <- ggplot(g1, aes(Países, pob, fill = pob)) +
  geom_col() + coord_polar() +
  scale_y_continuous(
    limits = c(0, 90),
    expand = c(0, 0)
  ) + 
  scale_fill_gradientn(
    "1 = 1 Millón de personas",
    colours = c( "#6C5B7B","#C06C84","#F67280","#F8B195")
  ) +
  guides(
    fill = guide_colorsteps(
      barwidth = 15, barheight = .5, title.position = "top", title.hjust = .5
    )
  ) +
  theme(
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(color = "gray12", size = 8),
    legend.position = "bottom",
  )+labs(title = "Países UE pob ",
         subtitle = "Datos de Eurostat ") + theme(plot.background = element_rect(fill = "antiquewhite"))
grid.arrange(g11, g12, ncol = 2)



# 3 Países por poblaciones en 2020 en porcentaje 
g2 <- df %>% 
  filter(year == "y_2020" & Países != 'UE 27 (desde 2020)' & Países != 'UE 28') %>% 
  arrange(desc(pop)) %>% 
  mutate(porcentaje = pop/sum(pop)*100)

g21 <-ggplot(g2, aes(x="", y=porcentaje, fill=Países)) + 
  geom_bar(width = 0.5,stat ="identity", color = "white") +
  coord_polar(theta = "y",direction = -1) +
  theme_void() + 
  ggtitle("Población de cada país sobre el total") +
  theme_minimal() + theme(plot.background = element_rect(fill = "antiquewhite")) + 
  geom_text(aes(label = paste0(round(porcentaje), "%")), position = position_stack(vjust = 0.5), size = 2)
g21

g22 <- treemap(g2,
    index="Países",
    vSize="porcentaje",
    type="index",                         
    title="Población países UE",
    palette="Spectral",
    border.col=c("white"), 
    border.lwds=3, 
    fontface.labels=1,
    bg.labels=c("transparent"),              
    align.labels=c("center", "center"),                                  
    overlap.labels=0.5)


# 3 Países por crecimiento de población desde 2010 
g3 <- df %>% 
  group_by(Países) %>% 
  mutate(crecimiento = pop - lag(pop)) %>% 
  mutate(crecimiento_en_porcentaje = (pop - lag(pop))/pop*100) %>%
  filter(Países != 'UE 27 (desde 2020)' & Países != 'UE 28') %>% 
  drop_na() 
  

g31  <- g3 %>% ggplot(aes(year, crecimiento_en_porcentaje, color = Países, group = Países)) + 
  geom_point() + geom_line(size = 1, alpha = .8) + 
  ggtitle("Crecimiento de la Población en porcentajes",
          subtitle = "El crecimiento es respecto al año anterior") +
  labs(x = "Años", y = "%", caption = "Eurostat") + theme(axis.text.x = element_text(angle = 40),
    plot.background = element_rect(fill = "antiquewhite"))
ggplotly(g31)


g32 <- ggplot(g3, aes(x =crecimiento_en_porcentaje , y = Países, fill = Países)) +
  geom_density_ridges(alpha=0.6, stat="binline", bins=20) +
  theme_ridges() + 
  theme(legend.position = "none") + theme(plot.background = element_rect(fill = "antiquewhite"))


# 4 Países por crecimiento de población 2010-2020
g4 <- ue_pob_t %>% 
  mutate(y_2021 = as.numeric(y_2021)) %>%
  mutate(crecimiento = (y_2021 - y_2010)/1000) %>% 
  mutate(crecimiento_porcentaje = (y_2021 - y_2010)/y_2010) %>%
  filter(Países != 'UE 27 (desde 2020)' & Países != 'UE 28') %>% 
  drop_na() %>% select(Países, crecimiento, crecimiento_porcentaje)

g41 <- ggplot(g4, aes(Países, crecimiento))+
  geom_bar(stat='identity', position = 'dodge', fill="#f68060")+coord_flip() +
theme_bw() + theme(axis.title = element_text(colour = "brown1"), 
plot.background = element_rect(fill = "bisque")) +labs(title = "Crecimiento de la población entre 2021 y 2010",
 x = NULL, subtitle = "Unidad en miles de personas ")
ggplotly(g41)

g42<- ggplot(g4, aes(crecimiento, crecimiento_porcentaje)) +
  geom_hex(color = "white") + theme(plot.background = element_rect(fill = "antiquewhite")) +labs(title = "Gráfico sobre crecimiento de la población 2010-2021 ",
    x = NULL, y = NULL, subtitle = "Eje x : crecimiento nominal, unidades en miles de personas 
Eje y : Tasa de crecimiento ",
    caption = "Eurostat")
