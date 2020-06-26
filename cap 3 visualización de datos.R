
# cap 3 -------------------------------------------------------------------
# basado en lo que presentó Patri
library(tidyverse)
library(gapminder)
library(DT)

str(gapminder)
datatable(gapminder)


p <- ggplot(data = gapminder)
p

p <- ggplot(data = gapminder,mapping = aes(x = gdpPercap,y = lifeExp, color=continent))
p
p + geom_point(size=3)


p <- ggplot(data = gapminder,mapping = aes(x = gdpPercap,y = lifeExp,color = continent))+
  geom_point(size=3)+
  coord_cartesian()+
  scale_x_log10(labels= scales::dollar)

p

ggplot(data = gapminder,mapping = aes(x = gdpPercap,y = lifeExp,color = continent))+ 
  geom_point(size=3)+
  coord_cartesian()+
  scale_x_log10(labels= scales::dollar)+
  labs(x = "Ingreso (GDP) Per Capita", y = "Esperanza de vida en años",
       title = "Crecimiento económico y esperanza de vida",
       subtitle = "Los puntos se representan por año-país",
       caption = "DataSource: Gapminder - Link: https://www.gapminder.org", color=" ")+
  guides(color = guide_legend(override.aes = list(size = 5)))


# paletas de colores ------------------------------------------------------

# Existen muchos paquetes de paletas de colores y cada uno ofrece diferentes opciones, 
# por ejemplo el paquete **RColorBrewer** ofrece:
library(RColorBrewer)

# cualitativas
display.brewer.all(type = "qual")

# cuantitativas
display.brewer.all(type = "seq")

# divergentes
display.brewer.all(type = "div")
  
# ejemplo 
pSet2 <- ggplot(data = gapminder,mapping = aes(x = gdpPercap,y = lifeExp,color= continent))+ 
  geom_point(size=3)+
  scale_color_brewer(palette="Set2")+
  coord_cartesian()+
  scale_x_log10(labels= scales::dollar)+
  labs(x = "Ingreso (GDP) Per Cápita", y = "Esperanza de vida",
       title = "Crecimiento económico y esperanza de vida",
       subtitle = "Los puntos se representan por año-país",
       caption = "DataSource: Gapminder - Link: https://www.gapminder.org", color=" ")+
  guides(color = guide_legend(override.aes = list(size = 5)))
pSet2

pdark2 <- ggplot(data = gapminder,mapping = aes(x = gdpPercap,y = lifeExp,color = continent))+ 
  geom_point(size=3)+
  scale_color_brewer(palette="Dark2")+
  coord_cartesian()+
  scale_x_log10(labels= scales::dollar)+
  labs(x = "Ingreso (GDP) Per Cápita", y = "Esperanza de vida",
       title = "Crecimiento económico y esperanza de vida",
       subtitle = "Los puntos se representan por año-país",
       caption = "DataSource: Gapminder - Link: https://www.gapminder.org", color=" ")+
  guides(color = guide_legend(override.aes = list(size = 5)))
pdark2 

# Paleta propia?
# Utilizamos una escala manual de colores mediante la opción 
# `scale_color_manual(values= c("color1", "color2",..., "colorN"), 
# labels= c("nombre1", "nombre2",..., "nombreN"))`, 
# en la misma especificamos los colores elegidos y las etiquetas de cada uno de ellos. 

pmanual <- ggplot(data = gapminder,mapping = aes(x = gdpPercap,y = lifeExp,color = continent))+ 
  geom_point(size=3)+
  scale_color_manual(values = c("#41b6a6", "#f6e37c", "#f5a26b","#51b8df","#713580"), 
                     labels = c("Africa", "América","Asia",  "Europa", "Oceanía"))+
  coord_cartesian()+
  scale_x_log10(labels= scales::dollar)+
  labs(x = "Ingreso (GDP) Per Cápita", y = "Esperanza de vida",
       title = "Crecimiento económico y esperanza de vida",
       subtitle = "Los puntos se representan por año-país",
       caption = "DataSource: Gapminder - Link: https://www.gapminder.org", color=" ")+
  guides(color = guide_legend(override.aes = list(size = 5)))
pmanual


# personalizar el theme ---------------------------------------------------

# definiremos dos parámetros 
# **text_size** y **margin_size**, que luego utilizaremos para modificar el tamaño del título, 
# subtítulo y leyenda.
# 
# Con `legend.position="bottom"` cambiamos la posición de la leyenda correspondiente a 
# continente, en este caso, le indicamos a R que la leyenda estará ubicada en la parte 
# inferior del gráfico.
# 
# Con `legend.key= element_rect(fill='NA')` eliminamos el recuadro gris de las leyendas.
# 
# Con `legend.text= element_text(color="#2c204d", size= text_size)` especificamos el color 
# y el tamaño del texto de la leyenda.
# 
# Con `plot.title= element_text(size = text_size * 1.8,family=Garamond, hjust = 0.5,
# vjust = 1,colour = "#2c204d", ...)` especificamos el tamaño, fuente, ubicación y color del 
# título.
# 
# Con `plot.subtitle= element_text(size = text_size * 1.3, family=Garamond, hjust = 0.5,
# vjust = 1,colour = "#2c204d", ...)` especificamos el tamaño, fuente, ubicación y color del 
# título.
# 
# Con `plot.caption= element_text(size = 11,family ="Garamond", hjust = 1, vjust = 1, 
# colour = "#2c204d",...)` especificamos el tamaño, fuente, ubicación y color del título.

text_size <-16   # tamaño base del texto
margin_size <- text_size/2  #de los margenes

pTheme_customizado <- ggplot(data=gapminder, mapping=aes(x= gdpPercap, y= lifeExp, 
                                                         color= continent))+
  geom_point(size=3,alpha=0.6)+
  scale_x_log10(labels= scales::dollar)+
  scale_color_manual(values= c("#41b6a6", "#f6e37c","#f5a26b","#51b8df","#713580"), 
                     labels= c("Africa", "America", "Asia","Europa", "Oceanía"))+
  labs(x="Ingreso (PBI) Per Capita", y="Esperanza de vida en años",
       title ="Crecimiento económico y esperanza de vida", 
       subtitle ="Los puntos se representan por año-país" , 
       caption = "DataSource: Gapminder- Link: https://www.gapminder.org.", 
       color= '')+
  guides(color = guide_legend(override.aes = list(size = 5)))+
  theme_light()+
  theme(legend.position="bottom", legend.key= element_rect(fill='NA'),
        legend.text= element_text(color="#2c204d", size= text_size), 
        legend.justification = "center",
        plot.title = element_text(size=(text_size * 1.8),family ="Garamond",    
                                  hjust = 0.5,vjust = 1,
                                  colour = "#2c204d", #
                                  face = 'bold', 
                                  margin = margin(b = margin_size * 1.2)),
        plot.subtitle = element_text(size = text_size * 1.3,
                                     family ="Garamond", hjust = 0.5, vjust = 1, 
                                     colour = "#2c204d",
                                     margin = margin(b = margin_size * 0.9)),
        plot.caption = element_text(size = 11,family ="Garamond",
                                    hjust = 1, vjust = 1,
                                    colour = "#2c204d", 
                                    face='bold',
                                    margin = margin(t = margin_size * 0.9)),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "gray90", size = 0.5),
        panel.grid.minor = element_line(color = "gray90", size = 0.25))
pTheme_customizado



# guardar gráficos --------------------------------------------------------

# Utilizamos la sentencia `ggsave("nombre.extension, height= n, width= n, units=" ", 
# type= ' '")`, como argumento le pasamos el nombre del gráfico con la extensión deseada 
# (png, pdf, svg, entre otras), dimensiones del gráfico con `height` y `width` o con 
# `dpi`y unidades con `units` en "in","cm" o "mm").

ggsave(here("grafico_scatterplot1.png"), height = 8, width = 10, units = "in", type='cairo')


# agregar línea de modelo -------------------------------------------------

text_size <-16   # tamaño base del texto
margin_size <- text_size/2  #de los margenes
# Asignamos el gráfico `ggplot' al objeto p_capa_smooth
p_capa_smooth <- ggplot(data=gapminder, mapping=aes(x= gdpPercap, y= lifeExp))+
  geom_point(aes(color=continent),size=3,alpha=0.6)+
  geom_smooth(method = 'gam', col = "#2c204d", size = 0.7,
              fill = "gray60", alpha = 0.2)+
  scale_x_log10(labels= scales::dollar)+
  scale_color_manual(values= c("#41b6a6", "#f6e37c","#f5a26b","#51b8df","#713580"), labels= c("Africa", "America", "Asia","Europa", "Oceanía"))+
  labs(x="Ingreso (PBI) Per Capita", y="Esperanza de vida en años",
       title ="Crecimiento económico y esperanza de vida", subtitle ="Los puntos se representan por año-país" , caption = "DataSource: Gapminder- Link: https://www.gapminder.org.", color= '', fill="Población")+
  guides(color = guide_legend(override.aes = list(size = 5)))+
  theme_light()+
  theme(legend.position="bottom", legend.key= element_rect(fill='NA'),
        legend.text= element_text(color="#2c204d", 
                                  size= text_size), 
        legend.justification = "center",
        plot.title = element_text(size=(text_size * 1.8),family ="Garamond",    
                                  hjust = 0.5,vjust = 1,
                                  colour = "#2c204d", #
                                  face = 'bold', 
                                  margin = margin(b = margin_size * 1.2)),
        plot.subtitle = element_text(size = text_size * 1.3,
                                     family ="Garamond", hjust = 0.5, vjust = 1, 
                                     colour = "#2c204d",
                                     margin = margin(b = margin_size * 0.9)),
        plot.caption = element_text(size = 11,family ="Garamond",
                                    hjust = 1, vjust = 1,
                                    colour = "#2c204d", 
                                    face='bold',
                                    margin = margin(t = margin_size * 0.9)),
        panel.background = element_rect(fill = "white"),
        panel.grid.major = element_line(color = "gray90", size = 0.5),
        panel.grid.minor = element_line(color = "gray90", size = 0.25))

p_capa_smooth


# facet_wrap --------------------------------------------------------------

# separar datos según continente FACETAS

p_facet <- ggplot(data=gapminder, mapping=aes(x= gdpPercap, y= lifeExp, color=continent))+
  geom_point(size=3,alpha=0.6)+
  geom_smooth(method = 'gam', col = "#2c204d", size = 0.7,
              fill = "gray60", alpha = 0.2)+
  scale_x_log10(labels= scales::dollar)+
  scale_color_manual(values= c("#41b6a6", "#f6e37c","#f5a26b","#51b8df","#713580"), 
                     labels= c("Africa", "America", "Asia","Europa", "Oceanía"))+
  labs(x="Ingreso (PBI) Per Capita", y="Esperanza de vida en años",
       title ="Crecimiento económico y esperanza de vida", 
       subtitle ="Los puntos se representan por año-país" , 
       caption = "DataSource: Gapminder- Link: https://www.gapminder.org.", color= '')+
  guides(color = guide_legend(override.aes = list(size = 5)))+
  theme_bw()+
  theme(legend.position="bottom")+
  facet_wrap(~continent, ncol = 2, scales="free")
p_facet


# themes personalizados con otros paquetes --------------------------------

# hrbrthemes (https://github.com/hrbrmstr/hrbrthemes)
remotes::install_git("https://git.rud.is/hrbrmstr/hrbrthemes.git")
 library(hrbrthemes)

# para combinar gráficos
# (https://github.com/thomasp85/patchwork)

library(patchwork)
# Podemos elegir combinar los dos figuras así:

# `p1 + p2`, una figura a continuación de la otra
# `p1 / p2`, la primera figura en la parte superior y la segunda en la parte inferior.

graficos_combinados <- p/ p +         
  #combino ambos gráficos
  plot_annotation(title ="Crecimiento económico y esperanza de vida", 
                  subtitle ="Los puntos se representan por año-país" , 
                  caption = "DataSource: Gapminder- Link: https://www.gapminder.org.") &
  theme(plot.title = element_text(size = 12), 
        plot.subtitle = element_text(size = 10), 
        plot.caption = element_text(size = 10))  
# agrego título, subtítulo y caption
# defino el tamaño de las etiquetas
graficos_combinados

