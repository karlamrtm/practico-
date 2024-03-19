#Karla Medina Practico 1#
10+5
10*5
a <-28
b <- 8
a+b
c <- a+b
sum(28,8)
round(10.14536)
install.packages("library")
install.packages("pacman")
pacman::p_load(dplyr,guaguas,ggplot2)
rm(list=ls())
options(scipen=999)
base <- guaguas
dim(base)
names(base)
head(base)
table(base$sexo)
filter(base, nombre=="Karla")
d <- filter(base, nombre=="Karla" & anio==1998)
sum(d$n)
datos <- filter(base, nombre=="Karla")
  ggplot(datos, aes(x = anio, y = n))+
  geom_line()+
    labs(x = "anio",y = "Numero de personas", tittle = "Numero de personas llamadas Karla por anio")
guaguas %>%
  filter(nombre %in% c("Karla", "Francisco"), anio >= 1960 & anio <= 2009) %>%
  ggplot(aes(anio, n, color = nombre))+
  geom_line()+
  labs(x = "anio", y = "total inscripciones", color = "nombre",
       title = "Inscripciones de Karla y Francisco entre 1960 - 2009") 
       
       