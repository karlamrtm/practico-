#Trabajo 4 - Karla Medina Retamales
#Preparación de los datos

install.packages("pacman")
install.packages("sjmisc", dependencies = TRUE)
pacman::p_load(dplyr, sjmisc, car, sjlabelled, stargazer, haven)
rm(list=ls())       # borrar todos los objetos en el espacio de trabajo
options(scipen=999) # valores sin notación científica

elsoc2016 <- read_dta("elsoc2016.dta")
dim(elsoc2016)
View(elsoc2016)

# Comprobar variables a utilizar
find_var(data = elsoc2016, "migrantes")
find_var(data = elsoc2016, "edad")
find_var(data = elsoc2016, "clase social")

proc_data <- elsoc2016 %>% select(c06_06,
                                  r12_03, 
                                  r12_04, 
                                  c34, 
                                  m01,
                                  m0_sexo,
                                  m0_edad)
View(proc_data)
names(proc_data)
sjlabelled::get_label(proc_data)

#Descriptivo general de variables elsoc2016#
frq(proc_data$c06_06)
frq(proc_data$r12_03)
frq(proc_data$r12_04)
frq(proc_data$c34)
frq(proc_data$m01)
frq(proc_data$m0_sexo)
frq(proc_data$m0_edad)

#Eliminar valores perdidos 
proc_data$c06_06 <- recode(proc_data$c06_06, "c(-999,-888,-777,-666)=NA")
proc_data$r12_03 <- recode(proc_data$r12_03, "c(-999,-888,-777,-666)=NA")
proc_data$r12_04 <- recode(proc_data$r12_04, "c(-999,-888,-777,-666)=NA")
proc_data$c34 <- recode(proc_data$c34, "c(-999,-888,-777,-666)=NA")
proc_data$m01 <- recode(proc_data$m01, "c(-999,-888,-777,-666)=NA")
proc_data <- proc_data %>% set_na(., na = c(-999, -888,-777,-666))

#Revisar cambios
frq(proc_data$c06_06)
frq(proc_data$r12_03)
frq(proc_data$r12_04)
frq(proc_data$c34)
frq(proc_data$m01)
frq(proc_data$m0_sexo)
frq(proc_data$m0_edad)

proc_data <- proc_data %>% set_na(., na = c(-2, -1))

#Recodificación de valores
proc_data$c06_06 <- recode(proc_data$c06_06, "1=0; 2=1; 3=2; 4=3; 5=4")
proc_data$r12_03 <- recode(proc_data$r12_03, "1=0; 2=1; 3=2; 4=3; 5=4")
proc_data$r12_04 <- recode(proc_data$r12_04, "1=0; 2=1; 3=2; 4=3; 5=4")
proc_data$c34 <- recode(proc_data$c34, "1=0; 2=1; 3=2; 4=3; 5=4")
proc_data$m01 <- recode(proc_data$m01,  "1=0; 2=1; 3=2; 4=3; 5=4; 6=5; 7=6; 8=7; 9=8; 10=9")
proc_data$m0_sexo <- recode(proc_data$m0_sexo, "1=0; 2=1")

##Etiquetado de variables##
proc_data <- proc_data %>% rename("conf_inmigr"=c06_06,
                                  "pier_ident_chi"=r12_03,
                                  "aumenta_desempleo"=r12_04,
                                  "iden_clase"=c34,
                                  "nivel_educacional"=m01,
                                  "sexo"=m0_sexo,
                                  "edad"=m0_edad)

proc_data$conf_inmigr <- set_label(x = proc_data$conf_inmigr,label = "Confianza en inmigrantes")
get_label(proc_data$conf_inmigr)

proc_data$pier_ident_chi<- set_label(x = proc_data$pier_ident_chi,label = "Chile pierde su identidad con la llegada de migrantes")
get_label(proc_data$pier_ident_chi)

proc_data$aumenta_desempleo <- set_label(x = proc_data$aumenta_desempleo,label = "Aumenta el desempleo con la llegada de inmigrantes")
get_label(proc_data$aumenta_desempleo)

proc_data$iden_clase <- set_label(x = proc_data$iden_clase,label = "Identificación con clase social subjetiva")
get_label(proc_data$iden_clase)

proc_data$nivel_educacional <- set_label(x = proc_data$nivel_educacional,label = "Nivel educacional")
get_label(proc_data$nivel_educacional)

proc_data$sexo <- set_label(x = proc_data$sexo,label = "Sexo")
get_label(proc_data$sexo)

proc_data$edad <- set_label(x = proc_data$edad,label = "Edad")
get_label(proc_data$edad)

frq(proc_data$conf_inmigr)
frq(proc_data$pier_ident_chi)
frq(proc_data$aumenta_desempleo)
frq(proc_data$iden_clase)
frq(proc_data$nivel_educacional)
frq(proc_data$sexo)
frq(proc_data$edad)

#Re-etiquetar valores 
proc_data$conf_inmigr <- set_labels(proc_data$conf_inmigr,
                                 labels=c( "Nada de confianza"=0,
                                           "Poca confianza"=1,
                                           "Algo de confianza"=2,
                                           "Bastante confianza"=3,
                                           "Mucha confianza"=4))

proc_data$pier_ident_chi <- set_labels(proc_data$pier_ident_chi,
                                       labels=c( " Totalmente en desacuerdo"=0,
                                                 "En desacuerdo"=1,
                                                 "Ni de acuerdo ni en desacuerdo"=2,
                                                 "De acuerdo"=3,
                                                 "Totalmente de acuerdo"=4))

proc_data$aumenta_desempleo <- set_labels(proc_data$aumenta_desempleo,
                                          labels=c( " Totalmente en desacuerdo"=0,
                                                    "En desacuerdo"=1,
                                                    "Ni de acuerdo ni en desacuerdo"=2,
                                                    "De acuerdo"=3,
                                                    "Totalmente de acuerdo"=4))

proc_data$iden_clase <- set_labels(proc_data$iden_clase,
                                     labels=c( " Totalmente en desacuerdo"=0,
                                               "En desacuerdo"=1,
                                               "Ni de acuerdo ni en desacuerdo"=2,
                                               "De acuerdo"=3,
                                               "Totalmente de acuerdo"=4))
proc_data$sexo <- set_labels(proc_data$sexo,
                                       labels=c( "Hombre"=0,
                                                 "Mujer"=1))
proc_data$nivel_educacional <- set_labels(proc_data$nivel_educacional,
                             labels=c( "Sin estudios"=0,
                                       "Educacion Basica o Preparatoria incompleta"=1,
                                       "Educacion Basica o Preparatoria completa"=2,
                                       "Educacion Media o Humanidades incompleta"=3,
                                       "Educacion Media o Humanidades completa"=4,
                                       "Tecnica Superior incompleta"=5,
                                       "Tecnica Superior completa"=6,
                                       "Universitaria incompleta"=7,
                                       "Universitaria completa "=8,
                                       "Estudios de posgrado (magister o doctorado)"=9))
frq(proc_data$conf_inmigr)
frq(proc_data$pier_ident_chi)
frq(proc_data$aumenta_desempleo)
frq(proc_data$iden_clase)
frq(proc_data$sexo)
frq(proc_data$edad)
frq(proc_data$nivel_educacional)

#Eliminar n.a o casos perdidos 
proc_data_original <-proc_data
dim(proc_data)
sum(is.na(proc_data))
proc_data <-na.omit(proc_data)
dim(proc_data)
proc_data <-sjlabelled::copy_labels(proc_data,proc_data_original)
frq(proc_data$edad)
frq(proc_data$nivel_educacional)

####recodificacion usando funcion 'recode' de la libreria car
#Recodificación de variable nivel educacional 
frq(proc_data$nivel_educacional)
proc_data$nivel_educacional <- car::recode(proc_data$nivel_educacional, "c(0,1,2)=1; c(3,4)=2; c(5,6,7,8)=3; c(9)=4")
proc_data$nivel_educacional <- factor(proc_data$nivel_educacional,
                             labels = c("Educacion Basica", "Educacion Media", "Educacion Superior", "Estudios Postgrado"),
                             levels = c(1, 2, 3, 4))
frq(proc_data$nivel_educacional)
proc_data$nivel_educacional <- set_label(x = proc_data$nivel_educacional,label = "Nivel educacional")
get_label(proc_data$nivel_educacional)

#Recodificación de variable sexo
frq(proc_data$sexo)

#Recodificación de variable edad
frq(proc_data$edad)
get_label(proc_data$edad)
proc_data$edad <- set_label(x = proc_data$edad,label = "Edad")

proc_data <-as.data.frame(proc_data)
stargazer(proc_data, type="text")

save(proc_data,file = "C:/Users/hp/Desktop/Rstudio/practico-1/elsoc2016.rdata")

proc_data %>% dplyr::group_by(conf_inmigr) %>% summarise(mean(pier_ident_chi, na.rm=TRUE))
proc_data %>% dplyr::group_by(conf_inmigr) %>% summarise(mean(aumenta_desempleo, na.rm=TRUE))
proc_data %>% dplyr::group_by(conf_inmigr) %>% summarise(mean(iden_clase, na.rm=TRUE))
proc_data %>% dplyr::group_by(conf_inmigr) %>% summarise(mean(nivel_educacional, na.rm=TRUE))
proc_data %>% dplyr::group_by(conf_inmigr) %>% summarise(mean(sexo, na.rm=TRUE))
proc_data %>% dplyr::group_by(conf_inmigr) %>% summarise(mean(edad, na.rm=TRUE))

frq(proc_data$nivel_educacional)
library(sjPlot)
installed.packages("strengejacke")
sjt.xtab(proc_data$conf_inmigr, proc_data$pier_ident_chi, encoding = "UTF-8")
sjt.xtab(proc_data$conf_inmigr, proc_data$aumenta_desempleo, encoding = "UTF-8")
sjt.xtab(proc_data$conf_inmigr, proc_data$iden_clase, encoding = "UTF-8")
sjt.xtab(proc_data$conf_inmigr, proc_data$nivel_educacional, encoding = "UTF-8")
sjt.xtab(proc_data$conf_inmigr, proc_data$sexo, encoding = "UTF-8")
sjt.xtab(proc_data$conf_inmigr, proc_data$edad, encoding = "UTF-8")
