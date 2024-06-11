##Practico 4##
pacman::p_load(sjlabelled,
               dplyr, #Manipulacion de datos
               stargazer, #Tablas
               sjmisc, # Tablas
               summarytools, # Tablas
               kableExtra, #Tablas
               sjPlot, #Tablas y gráficos
               corrplot, # Correlaciones
               sessioninfo, # Información de la sesión de trabajo
               ggplot2) # Para la mayoría de los gráficos

load("C:/Users/hp/Desktop/Rstudio/practico-1/elsoc2016.rdata") #Cargar base de datos
elsoc2016 <- read_dta("elsoc2016.dta")
names(proc_data) # Muestra los nombres de las variables de la base de datos
dim(proc_data) # Dimensiones

stargazer(proc_data,type = "text")

sjmisc::descr(proc_data)
sjmisc::descr(proc_data,
              show = c("label","range", "mean", "sd", "NA.prc", "n"))%>%
  kable(.,"markdown")

summarytools::dfSummary(proc_data, plain.ascii = FALSE)
view(dfSummary(proc_data, headings=FALSE))

#Para esta entrega eliminamos los casos perdidos por su cantidad
ggplot()
ggplot(proc_data, aes(x = conf_inmigr))

proc_data %>% ggplot(aes(x = conf_inmigr)) + 
  geom_bar()

proc_data %>% ggplot(aes(x = conf_inmigr)) + 
  geom_bar(fill = "coral")

proc_data %>% ggplot(aes(x = conf_inmigr)) + 
  geom_bar(fill = "coral")+
  labs(title = "Confianza en inmigrantes en el país",
       x = "Niveles de confianza en inmigrantes",
       y = "Grados de confianza")

# Crear el gráfico usando ggplot2#
graph1 <- proc_data %>% ggplot(aes(x = conf_inmigr)) + 
  geom_bar(fill = "coral")+
  labs(title = "Confianza en inmigrantes en el país",
       x = "Niveles de confianza en inmigrantes",
       y = "Grados de confianza") +
  theme_bw()

graph1

#Explicar que significa cada valor asociado a los grados de confianza y como ello se asocia a la percepción de la migración con la llegada de inmigrantes Hai/Per/Ven"

# y lo podemos guardar:
ggsave(graph1, file="files/img/graph1.png")

#Exploración de asociación entre variables categoricas# 
#sjt.xtab(proc_data$conf_inmigr, proc_data$pier_ident_chi, encoding = "UTF-8")
#sjt.xtab(proc_data$conf_inmigr, proc_data$aumenta_desempleo, encoding = "UTF-8")
#sjt.xtab(proc_data$conf_inmigr, proc_data$iden_clase, encoding = "UTF-8")
#sjt.xtab(proc_data$conf_inmigr, proc_data$nivel_educacional, encoding = "UTF-8")
#sjt.xtab(proc_data$conf_inmigr, proc_data$sexo, encoding = "UTF-8")
#sjt.xtab(proc_data$conf_inmigr, proc_data$edad, encoding = "UTF-8")

sjt.xtab(proc_data$conf_inmigr, proc_data$pier_ident_chi)
sjt.xtab(proc_data$conf_inmigr, proc_data$pier_ident_chi,
         show.col.prc=TRUE,
         show.summary=FALSE,
         encoding = "UTF-8")

sjt.xtab(proc_data$conf_inmigr, proc_data$aumenta_desempleo)
sjt.xtab(proc_data$conf_inmigr, proc_data$aumenta_desempleo,
         show.col.prc=TRUE,
         show.summary=FALSE,
         encoding = "UTF-8")

sjt.xtab(proc_data$conf_inmigr, proc_data$iden_clase)
sjt.xtab(proc_data$conf_inmigr, proc_data$iden_clase,
         show.col.prc=TRUE,
         show.summary=FALSE,
         encoding = "UTF-8")

sjt.xtab(proc_data$conf_inmigr, proc_data$nivel_educacional)
sjt.xtab(proc_data$conf_inmigr, proc_data$nivel_educacional,
         show.col.prc=TRUE,
         show.summary=FALSE,
         encoding = "UTF-8")

sjt.xtab(proc_data$pier_ident_chi, proc_data$sexo)
sjt.xtab(proc_data$pier_ident_chi, proc_data$sexo,
         show.col.prc=TRUE,
         show.summary=FALSE,
         encoding = "UTF-8")

sjt.xtab(proc_data$pier_ident_chi, proc_data$edad)
sjt.xtab(proc_data$pier_ident_chi, proc_data$edad,
         show.col.prc=TRUE,
         show.summary=FALSE,
         encoding = "UTF-8")

tapply(proc_data$conf_inmigr, proc_data$iden_clase, mean)
tapply(proc_data$conf_inmigr, proc_data$pier_ident_chi, mean)
tapply(proc_data$conf_inmigr, proc_data$aumenta_desempleo, mean)
tapply(proc_data$conf_inmigr, proc_data$nivel_educacional, mean)
tapply(proc_data$conf_inmigr, proc_data$sexo, mean)
tapply(proc_data$conf_inmigr, proc_data$edad, mean)
tapply(proc_data$conf_inmigr, proc_data$edad_groups, mean)



graph2 <- sjPlot::plot_stackfrq(dplyr::select(proc_data, conf_inmigr,
                                              pier_ident_chi,
                                              aumenta_desempleo,
                                              iden_clase),
                                title = "Grado de acuerdo con la llegada de inmigrantes en el pais") +
  theme(legend.position="bottom")

graph2

# Guardamos
ggsave(graph2, file="files/img/graph2.png")

datos <- proc_data %>% group_by(iden_clase) %>% 
  summarise(promedio = mean(conf_inmigr, na.rm = TRUE))

ggplot(datos, aes(x =iden_clase, y = promedio)) +
  geom_point() +
  labs(x = "Identificación de clase social subjetiva", y = "Confianza en inmigrantes") +
  theme_minimal()

graph3 <- ggplot(proc_data, aes(x = as.numeric(iden_clase))) +
  geom_histogram(binwidth=0.6, colour="black", fill="yellow") +
  theme_bw() +
  xlab("Identificación de clase social") +
  ylab("Cantidad")

graph3 

# Guardamos

ggsave(graph3, file="files/img/graph3.png")

summary(proc_data$edad)
proc_data <- proc_data %>% 
  mutate(edad_groups = case_when(edad >=16 & edad<=25 ~ "Entre 16 y 25 años",
                                 edad >=26 & edad<=39 ~ "Entre 26 y 39 años",
                                 edad >=40 & edad<=65 ~ "Entre 40 y 65 años",
                                 edad >65 ~ "Más de 65 años"))

table(proc_data$edad_groups)
frq(proc_data$edad)
frq(proc_data$edad_groups)

#Grafico 4
graph4 <- ggplot(proc_data, aes(x = as.numeric(edad))) +
  geom_histogram(binwidth=0.6, colour="black", fill="yellow") +
  theme_bw() +
  xlab("Edad") +
  ylab("Cantidad")

graph4 

# Guardamos

ggsave(graph4, file="files/img/graph4.png")


str(proc_data$edad)
str(proc_data$conf_inmigr)
str(proc_data$pier_ident_chi)
str(proc_data$aumenta_desempleo)
str(proc_data$iden_clase)
str(proc_data$edad_groups)
str(proc_data$sexo)
str(proc_data$nivel_educacional)