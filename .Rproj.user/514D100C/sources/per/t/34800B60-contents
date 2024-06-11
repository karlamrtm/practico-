#Correlaciones entre variables

load("C:/Users/hp/Desktop/Rstudio/practico-1/elsoc2016.rdata") #Cargar base de datos
pacman::p_load(dplyr, # Manipulacion datos
               sjmisc, # Descriptivos
               sjPlot, # Tablas
               sjlabelled, #etiquetas
               kableExtra, #Tablas
               GGally, # Correlaciones
               corrplot) # Correlaciones

options(scipen = 999) # para desactivar notacion cientifica


elsoc2016 <- read_dta("elsoc2016.dta")
dim(proc_data) # Dimensiones
names(proc_data) # Muestra los nombres de las variables de la base de datos
proc_datacor <- proc_data %>% select(conf_inmigr, pier_ident_chi, aumenta_desempleo, iden_clase, nivel_educacional, sexo, edad)
sjmisc::descr(proc_datacor,
              show = c("label","range", "mean", "sd", "NA.prc", "n")) %>%
  kable(.,"markdown")

M <- cor(proc_data_original, use = "complete.obs")
M

#Correlaciones 
sjPlot::tab_corr(proc_data_original, 
                 triangle = "lower")
sjPlot::tab_corr(proc_data_original, 
                 na.deletion = "pairwise", # espeficicamos tratamiento NA
                 triangle = "lower")
corrplot.mixed(M)

proc_data_original <- proc_data_original %>%
  rowwise() %>%
  mutate(confianza = sum(c(sexo, edad), na.rm = TRUE))

ggpairs(proc_data_original)

sjPlot::plot_scatter(proc_data_original, sexo, edad)

sjPlot::plot_scatter(proc_data_original, conf_inmigr, nivel_educacional)

sjPlot::plot_scatter(proc_data_original, conf_inmigr, iden_clase)

psych::alpha(dplyr::select(proc_data_original, conf_inmigr, pier_ident_chi, aumenta_desempleo, iden_clase, nivel_educacional, sexo, edad))

summary(elsoc2016$ola)

sjmisc::frq(elsoc2016$ola,
            out = "txt",
            show.na = T) %>% knitr::kable()

fit01<- lm(partpol~ing_pcap,data=elsoc)
fit02<- lm(partpol~quintile,data=elsoc)
fit03<- lm(partpol~quintilemiss,data=elsoc)

#Hasta acá quede no alcance a hacer los modelos de regresión#