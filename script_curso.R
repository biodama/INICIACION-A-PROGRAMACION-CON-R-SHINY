###################################################################
###################################################################
# Script ggplot2-rshiny
###################################################################
###################################################################

library(ggplot2)

ggplot(mpg, aes(x = displ, y = hwy, colour = class)) +
  geom_point()


require("data.table")
require("ggplot2")
require("shiny")
require("scales")       # formateo de ejes (%, €, fechas...)
require("RColorBrewer") # paletas de color
require("viridis")      # paletas de color gradientes
require("patchwork")    # combinar varios gráficos en uno
require("plotly")       #gráficos interactivos
require("ggiraph")      #gráficos interactivos

?ggplot

setwd("/Users/celiatalavan/Desktop/INICIACION-A-PROGRAMACION-CON-R-SHINY-main 2")
load("data/datos.cancer.RData")
load("data/datos.tabla.des.RData")
head(cancer)


g <- ggplot(cancer)
g


ggplot(cancer, aes(x = periodo, y = tasa)) +
  geom_point()

cancer2<-cancer[cancer$tumor%in%c("MAMA","PROSTATA","COLORRECTAL")]

ggplot(cancer2, aes(x = periodo, y = tasa, colour = tumor)) +
  geom_point()

ggplot(cancer2, aes(x = periodo, y = tasa, colour = tasa, shape=sexo)) +
  geom_point()

ggplot(cancer2, aes(x = periodo, y = tasa, colour = tumor, shape=sexo)) +
  geom_point()

ggplot(cancer2, aes(x = periodo, y = tasa, colour = tumor, linetype=sexo)) +
  geom_line()

ggplot(cancer2, aes(x = tumor, y = tasa, colour = sexo)) +
  geom_boxplot()


ggplot(cancer2, aes(x = tumor, y = tasa,fill=sexo)) +
  geom_boxplot()


ggplot(cancer2, aes(x = tumor, y = tasa)) +
  geom_boxplot(colour = "steelblue", fill = "white")



##### Ejercicio1 


datos_cc <- cancer %>%
  filter(sexo == "Mujeres",
         tumor %in% c("ESTOMAGO", "ESOFAGO", "HIGADO"))


ggplot(datos_cc, aes(x = tumor, y = tasa, colour = tumor)) +
  geom_boxplot() +
  labs(title = "Tasas de cáncer en mujeres",
       x = "Tipo de tumor", y = "Tasa", fill = "Tipo de tumor")



#??? pregunta
ggplot(cancer2, aes(x = tumor, y = tasa,fill=sexo)) +
  geom_boxplot(colour = "steelblue", fill = "white")

mirar <- cancer[cancer$tumor %in% c("MAMA","PROSTATA"),]

ggplot(mirar, aes(x = periodo, y = tasa, linetype = sexo, colour = tumor)) +
  geom_line(linewidth = 1)

mirar <- cancer[(cancer$tumor == "MAMA" & cancer$sexo == "Mujeres") |
                  (cancer$tumor == "PROSTATA" & cancer$sexo == "Hombres"),]

ggplot(mirar, aes(x = periodo, y = tasa, linetype = sexo, colour = tumor)) +geom_line(linewidth = 1)


ggplot(mirar, aes(x = periodo, y = tasa, colour = tumor)) +
  geom_point(alpha =0.5, size = 3)


ggplot(mirar, aes(x = periodo, y = tasa, colour = tumor, shape = sexo, alpha = tasa)) +
  geom_point(size = 2)


ggplot(mirar, aes(x = periodo, y = tasa, colour = tumor)) +
  geom_line()

ggplot(mirar) +
  geom_line(data=cancer2,aes(x = periodo, y = tasa, colour = tumor))

ggplot(cancer2, aes(x = periodo, y = tasa)) +
  geom_line(aes(colour = tumor)) +     # el color solo se aplica a la línea
  geom_point(colour = "black", size = 1) 

ggplot(cancer2, aes(x = periodo, y = tasa, group =interaction(tumor,sexo),colour = tumor,shape=sexo)) +
  geom_point(color="black") + geom_line()



ggplot(mirar, aes(x = tasa)) +
  geom_histogram(bins = 5, fill = "steelblue", colour = "white")



ggplot(mirar, aes(x = tasa, fill = sexo)) +
  geom_density(alpha = 0.3)



ggplot(mirar, aes(x = tumor, y = tasa))+
  geom_violin(aes(fill=sexo)) +geom_boxplot(alpha=0.5,fill="white")

ggplot(cancer2, aes(x = tumor, y = tasa, colour = sexo)) +
  geom_point(width = 0.2, alpha = 0.6)


ggplot(cancer2, aes(x = tumor, y = tasa, colour = sexo)) +
  geom_jitter(width = 0.2, alpha = 0.6)

ggplot(mirar, aes(x = periodo, y = tasa, color = tumor, linetype = sexo)) +
  geom_smooth(se = T, method = "lm")  

ggplot(cancer2, aes(x = periodo, y = tasa, fill = tumor)) +
  geom_col(position = "dodge")



p1 <- ggplot(cancer2[cancer2$periodo%in%c(2010:2015)], aes(x = periodo, y = tasa, fill = tumor)) +
  geom_col(position = "stack")

p1

p2 <- ggplot(cancer2[cancer2$periodo%in%c(2010:2015)], aes(x = periodo, y = tasa, fill = tumor)) +
  geom_col(position = "dodge")
p2

p1 | p2


ggplot(cancer2, aes(x = periodo, y = tasa,colour=sexo)) +
  geom_line() +
  facet_wrap(~tumor)

ggplot(cancer2, aes(x = periodo, y = tasa,colour=tumor)) +
  geom_line() +
  facet_wrap(~sexo)


ggplot(mirar, aes(x = periodo, y = tasa)) +
  geom_line() +
  facet_wrap(~tumor,ncol=1) 

ggplot(cancer2, aes(x = periodo, y = tasa)) +
  geom_smooth(se = FALSE) +
  facet_grid(tumor ~ sexo)


cancer5<-cancer[cancer$tumor%in%c("HIGADO","ESTOMAGO"),]
table(cancer5$sexo)

ggplot(cancer5, aes(x = periodo, y = tasa)) +
  geom_smooth(se = FALSE) +
  facet_grid(tumor ~ sexo)

ggplot(cancer5, aes(x = periodo, y = tasa)) +
  geom_smooth(se = FALSE) +
  facet_grid(tumor ~ sexo, scales = "free_y")

g <- ggplot(cancer2, aes(x = periodo, y = tasa, fill = tumor)) +
  geom_col(position = "dodge")

g <- g + labs(x = "Periodo", y = "Tasa de mortalidad (defunciones/100000 hab)",
              fill = "Tumor", title = "Tasa de mortalidad por tipo de tumor",subtitle = "Gráfico elaborado para el curso",
              caption = "Fuente: CNE")
g

ggplot(mirar[mirar$sexo == "Hombres",], aes(x = periodo, y = tasa,colour = tumor)) +
  geom_line() + geom_point(colour = "black", size = 1) +
  geom_text(aes(label = round(tasa,1)),colour = "black",vjust = -0.7,size = 3)

library(ggrepel)
ggplot(mirar[mirar$sexo == "Hombres",], aes(x = periodo, y = tasa,colour = tumor)) +
  geom_line() + geom_point(colour = "black", size = 1) +
  geom_label_repel(aes(label = round(tasa,1)),colour = "black",size = 3)

ggplot(mirar, aes(x = periodo, y = tasa, color = tumor)) +
  geom_line() + scale_y_log10() +
  scale_color_manual(values = c("#BF4F22","#8E6FB0"))

ggplot(mirar, aes(x = periodo, y = tasa, color = tumor, group = tumor)) +
  geom_line() +
  scale_y_continuous(breaks = seq(15,30,2), limits = c(15,27))

ggplot(cancer2, aes(x = tumor, y = tasa, fill = tumor)) +
  geom_boxplot() + scale_fill_brewer(palette = "Spectral")


ggplot(cancer2, aes(x = periodo, y = tasa, colour = tasa)) +
  geom_point(size = 2) + scale_colour_viridis_c(option = "rocket")


ggplot(cancer2, aes(x = periodo, y = tasa/100, fill = tumor)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent)


ggplot(mirar, aes(x = periodo, y = tasa,colour = tumor)) +
  geom_line() + geom_point(size = 1) +
  scale_color_brewer(palette = "Accent")

ggplot(mirar, aes(x = periodo, y = tasa,color=sexo)) +
  geom_point()+geom_line()+scale_color_brewer(palette = "Dark2")

g + theme_minimal() +
  theme(legend.position = "bottom",
        axis.title = element_text(face = "bold"),
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
        panel.grid.minor = element_blank())

p1 <- ggplot(cancer2, aes(x = tumor, y = tasa)) + geom_boxplot() + coord_flip()
p2 <- ggplot(mirar, aes(x = tasa)) + geom_histogram(bins = 20)

(p1 / p2) + plot_annotation(title = "Distribución de la tasa de mortalidad",tag_levels = "A")

p<-ggplot(cancer2,aes(x = periodo,y = tasa,shape=sexo,colour = tumor)) +
  geom_line() +geom_point(size = 1)
p

cancer2$tumor <- factor(cancer2$tumor,levels = c("MAMA", "PROSTATA","COLORRECTAL"))

p<-ggplot(cancer2,aes(x = periodo,y = tasa,shape=sexo,colour = tumor)) +
  geom_line() +geom_point(size = 1)

p

load("data/peninsula.RData")

ggplot(peninsula) +
  geom_sf(aes(fill = NAME_1), alpha = 0.1, col = "grey80", show.legend = FALSE) +
  geom_sf_text(aes(label = NAME_2), size = 3) +
  theme_bw()

paro <- fread("data/paro.csv", encoding = "UTF-8")
paro[, `:=`(id, sub(" ", "0", format(Prov.id, width = 2)))]
Paro <- subset(paro, Año == 2012 & Trimestre == "I")
peninsula$id <- peninsula$CC_2
peninsula.paro <- merge(peninsula, Paro, by = "id")

head(peninsula.paro)

mapa <- ggplot(peninsula.paro) +
  geom_sf(aes(fill = Tasa.paro), colour = "grey80", size = 0.1) +
  facet_grid(~Sexo) +
  scale_fill_gradient("Tasa de paro", low = "aliceblue", high = "steelblue4") +
  theme_bw()

mapa

library(plotly)
library(ggiraph)

ggplotly(mapa)

mapa_tooltip <- ggplot(peninsula.paro,aes(fill = Tasa.paro,text = paste0(NAME_2, "<br>Tasa de paro: ", round(Tasa.paro,1), "%"))) +
  geom_sf(colour = "grey80", size = 0.1) +
  facet_grid(~Sexo) +
  scale_fill_viridis_c("Tasa de paro") +
  theme_minimal()

ggplotly(mapa_tooltip, tooltip = "text")





