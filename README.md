# Tarea2_FOR_LOOP

install.packages("tidyverse")
library(tidyverse)
install.packages("dplyr")
library(dplyr)

##Subir base de datos

##1.Se incopora en cada una de las bases de datos la variable “tamanio”

grandes_chile = grandes_chile %>%  mutate(tamanio = "grande")
grandes_colombia = grandes_colombia %>% mutate(tamanio = "grande")
grandes_peru = grandes_peru %>% mutate(tamanio = "grande")

medianas_chile = medianas_chile %>% mutate(tamanio = "mediana")
medianas_colombia = medianas_colombia %>% mutate(tamanio = "mediana")
medianas_peru = medianas_peru %>% mutate(tamanio = "mediana")

micro_chile = micro_chile %>% mutate(tamanio = "micro")
micro_colombia = micro_colombia %>% mutate(tamanio = "micro")
micro_peru = micro_peru %>% mutate(tamanio = "micro")

pequena_chile = pequena_chile %>% mutate(tamanio = "pequeña")
pequena_colombia = pequena_colombia %>% mutate(tamanio = "pequeña")
pequena_peru = pequena_peru %>% mutate(tamanio = "pequeña")



##2.Reuna todas las bases en una sola y defina de qué tipología


##Limpieza
names(grandes_chile)

grandes_chile <- rename(grandes_chile, porcentaje_mujeres = procentaje_mujeres)
names(grandes_colombia)
grandes_colombia <- rename(grandes_colombia, porcentaje_mujeres = procentaje_mujeres)
names(grandes_peru)
grandes_peru <- rename(grandes_peru, porcentaje_mujeres = procentaje_mujeres)

names(medianas_chile)
medianas_chile <- rename(medianas_chile, porcentaje_mujeres = procentaje_mujeres)

names(medianas_colombia)
names(medianas_peru)
medianas_peru <- rename(medianas_peru, porcentaje_mujeres = procentaje_mujeres)

names(micro_chile)
micro_chile <- rename(micro_chile, porcentaje_mujeres = procentaje_mujeres)

names(micro_colombia)
micro_colombia <- rename(micro_colombia, porcentaje_mujeres = procentaje_mujeres)
names(micro_peru)

names(pequena_peru)
pequena_peru <- rename(pequena_peru, porcentaje_mujeres = procentaje_mujeres)

rename(grandes_chile, procentaje_mujeres = "porcentaje_mujeres")
names(grandes_chile) = c("fecha", "pais", "ingresos", "costos", "procentaje_mujeres", 
  "exportaciones", "importaciones", "endeudamiento", "morosidad", "reservas", 
  "spread", "tasa_interes", "tamanio")
names(grandes_colombia) = c("fecha", "pais", "ingresos", "costos", "procentaje_mujeres", "exportaciones", "importaciones", "endeudamiento", "morosidad", "reservas", "spread", "tasa_interes", "tamanio")
names(grandes_peru) = c("fecha", "pais", "ingresos", "costos", "procentaje_mujeres", 
                        "exportaciones", "importaciones", "endeudamiento", "morosidad", "reservas", 
                        "spread", "tasa_interes", "tamanio")
names(medianas_chile) =c("fecha", "pais", "ingresos", "costos", "procentaje_mujeres", 
                         "exportaciones", "importaciones", "endeudamiento", "morosidad", "reservas", 
                         "spread", "tasa_interes", "tamanio")
names(medianas_colombia) = c("fecha", "pais", "ingresos", "costos", "procentaje_mujeres", 
                             "exportaciones", "importaciones", "endeudamiento", "morosidad", "reservas", 
                             "spread", "tasa_interes", "tamanio")
names(medianas_peru) = c("fecha", "pais", "ingresos", "costos", "procentaje_mujeres", 
                         "exportaciones", "importaciones", "endeudamiento", "morosidad", "reservas", 
                         "spread", "tasa_interes", "tamanio")
names(micro_chile) = c("fecha", "pais", "ingresos", "costos", "procentaje_mujeres", 
                       "exportaciones", "importaciones", "endeudamiento", "morosidad", "reservas", 
                       "spread", "tasa_interes", "tamanio")
names(micro_colombia) = c("fecha", "pais", "ingresos", "costos", "procentaje_mujeres", 
                           "exportaciones", "importaciones", "endeudamiento", "morosidad", "reservas", 
                           "spread", "tasa_interes", "tamanio")
names(micro_peru) = c("fecha", "pais", "ingresos", "costos", "procentaje_mujeres", 
                      "exportaciones", "importaciones", "endeudamiento", "morosidad", "reservas", 
                      "spread", "tasa_interes", "tamanio")
names(pequena_chile) =c("fecha", "pais", "ingresos", "costos", "procentaje_mujeres", 
                        "exportaciones", "importaciones", "endeudamiento", "morosidad", "reservas", 
                        "spread", "tasa_interes", "tamanio")
names(pequena_colombia) = c("fecha", "pais", "ingresos", "costos", "procentaje_mujeres", 
                            "exportaciones", "importaciones", "endeudamiento", "morosidad", "reservas", 
                            "spread", "tasa_interes", "tamanio")
names(pequena_peru) = c("fecha", "pais", "ingresos", "costos", "procentaje_mujeres", 
                        "exportaciones", "importaciones", "endeudamiento", "morosidad", "reservas", 
                        "spread", "tasa_interes", "tamanio")
##unión datos

bd_empresas <- rbind(grandes_chile,grandes_colombia,grandes_peru,medianas_chile,medianas_colombia,medianas_peru,micro_chile,micro_colombia,micro_peru,pequena_chile,pequena_colombia,pequena_peru)



##3.Determine a través del uso de condicionales y/o forcuántas obervaciones tiene Peru versus Chile.
observaciones  = "chile_vs_peru"
if(observaciones == "chile_vs_peru"){
chile = nrow(bd_empresas %>% filter(pais == "peru"))
grandes_chile = nrow(bd_empresas %>% filter(pais == "peru") %>% filter(tamanio == "grande"))
medianas_chile = nrow(bd_empresas %>% filter(pais == "peru") %>% filter(tamanio == "mediana"))
micro_chile = nrow(bd_empresas %>% filter(pais == "peru") %>% filter(tamanio == "micro"))
pequena_chile = rnorm(bd_empresas %>% filter(pais == "peru") %>% filter(tamanio == "pequeña"))
peru = nrow(bd_empresas %>% filter(pais == "peru"))
grandes_peru = nrow(bd_empresas %>% filter(pais == "peru") %>% filter(tamanio == "grande"))
medianas_peru = nrow(bd_empresas %>% filter(pais == "peru") %>% filter(tamanio == "mediano"))
micro_peru = nrow(bd_empresas %>% filter(pais == "peru") %>% filter(tamanio == "micro"))
pequena_peru = nrow(bd_empresas %>% filter(pais == "peru") %>% filter(tamanio == "pequeña"))}

print(paste("peru tiene", peru, "esta cantidad de observaciones",
            grandes_peru, "grandes", medianas_peru, "medianas", micro_peru, "micro", pequena_peru, "pequeñas empresa", 
            "chile esta formado por", grandes_chile, "grandes", medianas_chile, "mediana",
            micro_chile, "micro", pequena_chile, "y mequeñas empresa"))


##4.Determine  a  través  del  uso  de condicionales  y/o  for¿cuál  es  el  país  con  mayor ingresos de explotaciónpara los años que considera la muestra

bd_paises <- data_frame(bd_paises) c(grandes_chile, "ingresos", medianas_chile, "ingresos", micro_chile, "ingresos", pequena_chile, "ingresos",
              grandes_colombia, "ingresos", medianas_colombia, "ingresos", micro_colombia, "ingresos",  pequena_colombia, "ingresos",
              grandes_peru, "ingresos", medianas_peru, "ingresos", micro_peru, "ingresos", pequena_peru, "ingresos")
                    

maximo=bd_paises$ingresos[1]
for (i in 2:length(bd_paises$ingresos)){
  if(bd_paises$ingresos[i]>=maximo){
    maximo=bd_paises$ingresos[i]
  }
  else if(bd_paises$ingresos[i]<maximo){
    maximo=maximo
  }
}



##7.Gráfique algunas variables seleccionadas, las cuales puedan responder a una pregunta que se haga conrespecto a los datos.
##pregunta: '¿como varian por tamaño de empresa las tasas de interés en Colombia?


bd_empresas <- mutate(bd_empresas, ingreso_factor = factor(bd_empresas$ingreso, 
                labels = c("chile", "colombia")))
hist(bd_empresas$tasa_interes)
hist(grandes_colombia$tasa_interes)
hist(medianas_colombia$tasa_interes)
hist(micro_colombia$tasa_interes)
hist(pequena_colombia$tasa_interes)

##conclusión: cuando las empresas son grandes la tasa de interes tiente a 0,7, siendo muy parecida a 
##la frecuencia que tienen las medianas empresas, no es el caso de las micro, que su tasa es más baja
##teniendo mayor frecuencia en el 0.35, por otra lado las micro empresas tienen a una tasa del 0.10
