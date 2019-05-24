# Paquetes y directorio ----
library(pacman)
p_load(tidyverse, # el universo tidy carga varios paquetes: dplyr, tidyr, ggplot, etc
       foreign, Hmisc, # para abrir
       ggalt, ggthemes, devtools, gridExtra,
       maps, mapdata, ggmap,
       grid, gridExtra, ggpubr) 

inp <- "ENADID/01_datos/"
out <- "ENADID/03_gráficas/"

# Función para obtener leyenda ----
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

# Datos 2014 ----
data_2014 <- read_csv(paste0(inp, "enadid2014.csv")) %>% 
  mutate(edad = as.integer(p5_2),
         grupos_edad = ifelse(edad<20, "Adolescentes\n(15-19 años)",
                              ifelse(edad>19 & edad<30, "Jóvenes\n(20-29 años)",
                                     ifelse(edad>29 & edad<40, "Adultas 1\n(30-39 años)",
                                            ifelse(edad>39, "Adultas 2\n(40-49 años)", "ZETA")))))

# * Conocimientos 2014----
anticon_2014 <- select(data_2014,
                       llave_muj, ent,
                       fac_per, upm_dis, estrato, # necesarias para estimaciones
                       edad, niv, # sociodem (edad, escolaridad)
                       starts_with("p8_1"), # conocimiento general de anticoncepción
                       starts_with("p8_2")) %>% 
  subset(edad<50) %>% 
  mutate(algun_anti = ifelse(p8_1_01>2 & 
                               p8_1_02>2 & 
                               p8_1_03>2 &
                               p8_1_04>2 & 
                               p8_1_05>2 & 
                               p8_1_06>2 & 
                               p8_1_07>2 & 
                               p8_1_08>2 & 
                               p8_1_09>2 & 
                               p8_1_10>2,
                             0,1),
         func_anti = ifelse(p8_2_03_1==1 & 
                              p8_2_03_2==1 &
                              p8_2_08_2==1 & 
                              p8_2_08_3==1,
                            1,0),
         grupos_edad = ifelse(edad<20, "Adolescentes\n(15-19 años)",
                              ifelse(edad>19 & edad<30, "Jóvenes\n(20-29 años)",
                                     ifelse(edad>29 & edad<40, "Adultas 1\n(30-39 años)",
                                            ifelse(edad>39, "Adultas 2\n(40-49 años)", "ZETA"))))) 

conocimientos_2014 <- anticon_2014 %>%
  select(fac_per, grupos_edad, fac_per, func_anti, algun_anti) %>%
  group_by(grupos_edad) %>%
  summarise(total_general = sum(algun_anti*fac_per, na.rm = T),
            total_funcional = sum(func_anti*fac_per, na.rm = T),
            fac_per = sum(fac_per, na.rm = T),
            porc_general = total_general/fac_per*100,
            porc_funcional = total_funcional/fac_per*100) %>%
  select(grupos_edad, starts_with("porc")) %>%
  mutate(dif = porc_general - porc_funcional) %>%
  gather(grupo,
         porc,
         porc_general:porc_funcional)

# * Plot 2014 (conocimientos) ----

con_2014_plot <- 
  ggplot(data= conocimientos_2014,
         aes(x=reorder(grupos_edad,-dif),
             y=porc,
             group=grupo)) +
  geom_bar(stat = "identity",position = "dodge",
           aes(fill=grupo)) + 
  scale_fill_manual("",
                    values = c("#92A370","#005024"),
                    labels = c("Funcional", "General")) +
  geom_text(aes(label = paste0(round(porc,2),"%")),
            size = 5, position= position_dodge(width = 1),
            vjust = -0.5) +
  ggtitle("2014")+ 
  xlab("") + ylab("") + 
  theme(axis.ticks.x = element_blank(),
        legend.position = "none",
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 20)) 


# Datos 2018 ----
m1 <- read_csv(paste0(inp, "enadid2018_mujer1.csv"))
m2 <- read_csv(paste0(inp, "enadid2018_mujer2.csv")) %>% 
  select(-c("upm","viv_sel","hogar", "n_ren", "llave_viv", "llave_hog",
            "ent","tam_loc","fac_per","t_loc_ur", "t_loc_ag1",
            "cond_act","p10_1_ag","p10_1_ag2","p3_14","niv",
            "gra","edad_1ag","c_limdisc","estrato","est_dis","upm_dis"))

m <- left_join(m1, m2, by = "llave_muj")
rm(m1,m2)

sociodem <- read_csv(paste0(inp, "enadid2018_sdem.csv")) %>% 
  subset(sexo==2) %>% 
  select(-c("upm","viv_sel","hogar", "n_ren", "llave_viv", "llave_hog",
            "ent","tam_loc","t_loc_ur", "t_loc_ag1",
            "cond_act","niv",
            "gra","edad_1ag","c_limdisc","estrato","est_dis","upm_dis")) %>% 
  mutate(llave_muj = llave_per) %>% 
  select(-"llave_per")

data <- left_join(m, sociodem, by = "llave_muj")
rm(m, sociodem)

write.csv(data,paste0(inp,"enadid_2018.csv"), 
          fileEncoding = "latin1",
          row.names = F)
rm(data)

data_2018 <- read_csv(paste0(inp,"enadid_2018.csv")) %>% 
  mutate(edad = as.integer(edad),
         grupos_edad = ifelse(edad<20, "Adolescentes\n(15-19 años)",
                              ifelse(edad>19 & edad<30, "Jóvenes\n(20-29 años)",
                                     ifelse(edad>29 & edad<40, "Adultas 1\n(30-39 años)",
                                            ifelse(edad>39, "Adultas 2\n(40-49 años)", "ZETA")))))

# * Conocimientos 2018----
anticon_2018 <- select(data_2018,
                  llave_muj, ent,
                  fac_per, upm_dis, estrato, # necesarias para estimaciones
                  p5_2_1, niv, # sociodem (edad, escolaridad)
                  starts_with("p8_1"), # conocimiento general de anticoncepción
                  starts_with("p8_2")) %>% 
  mutate(edad = p5_2_1) %>% 
  select(-"p5_2_1") %>% 
  subset(edad<50) %>% 
  mutate(algun_anti = ifelse(p8_1_01>2 & 
                             p8_1_02>2 & 
                             p8_1_03>2 &
                             p8_1_04>2 & 
                             p8_1_05>2 & 
                             p8_1_06>2 & 
                             p8_1_07>2 & 
                             p8_1_08>2 & 
                             p8_1_09>2 & 
                             p8_1_10>2,
                             0,1),
         func_anti = ifelse(p8_2_03_1==1 & 
                            p8_2_03_2==1 &
                            p8_2_08_2==1 & 
                            p8_2_08_3==1,
                            1,0),
         grupos_edad = ifelse(edad<20, "Adolescentes\n(15-19 años)",
                              ifelse(edad>19 & edad<30, "Jóvenes\n(20-29 años)",
                                     ifelse(edad>29 & edad<40, "Adultas 1\n(30-39 años)",
                                            ifelse(edad>39, "Adultas 2\n(40-49 años)", "ZETA"))))) 

conocimientos_2018 <- anticon_2018 %>%
  select(fac_per, grupos_edad, func_anti, algun_anti) %>%
  group_by(grupos_edad) %>%
  summarise(total_general = sum(algun_anti*fac_per, na.rm = T),
            total_funcional = sum(func_anti*fac_per, na.rm = T),
            fac_per = sum(fac_per, na.rm = T),
            porc_general = total_general/fac_per*100,
            porc_funcional = total_funcional/fac_per*100) %>%
  select(grupos_edad, starts_with("porc")) %>%
  mutate(dif = porc_general - porc_funcional) %>%
  gather(grupo,
         porc,
         porc_general:porc_funcional)

# * Plot 2018 (conocimientos) ----

con_2018_plot <- 
  ggplot(data= conocimientos_2018,
       aes(x=reorder(grupos_edad,-dif),
           y=porc,
           group=grupo)) +
  geom_bar(stat = "identity",position = "dodge",
           aes(fill=grupo)) + 
  scale_fill_manual("",
                    values = c("#92A370","#005024"),
                    labels = c("Funcional", "General")) +
  geom_text(aes(label = paste0(round(porc,2),"%")),
            size = 5, position= position_dodge(width = 1),
            vjust = -0.5) +
  ggtitle("2018")+ 
  xlab("") + ylab("") + 
  theme(axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "right",
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 20)) 

# * Plot conocimientos (2014-2018)----
conocimientos_plot <- 
  ggarrange(con_2014_plot, con_2018_plot, ncol= 2, nrow = 1, common.legend = TRUE, legend="right")

fiuf <- "Conocimiento general vs. conocimiento funcional de métodos anticonceptivos"
fiuffi <- "Fuente: elaboración de Gire con datos de la ENADID 2014 y 2018."
annotate_figure(
  conocimientos_plot,
  top = text_grob(str_wrap(fiuf, width = 55), size = 22),
  bottom = text_grob(fiuffi, size = 10, hjust = 0)
)
ggsave(paste0(out, "conocimientos_2014_2018.png"),
       height = 30, width = 30, units = "cm")

# Decisión de las mujeres (2018) ----
decision <- data_2018 %>% 
  select(fac_per, grupos_edad, p7_16) %>% 
  drop_na(p7_16) %>% 
  mutate(decision_ella = ifelse(p7_16==1 | p7_16==3,0,1)) %>% 
  group_by(grupos_edad) %>%
  summarise(total_decision = sum(decision_ella*fac_per, na.rm = T),
            fac_per = sum(fac_per, na.rm = T),
            porc_decision = total_decision/fac_per*100) %>%
  select(grupos_edad, starts_with("porc"))

# write.csv(decision, file = paste0(out,"decision_hijos.csv"), row.names = F)

# El 23.86% de las madres adolescentes declararon que la decisión de tener hijos 
# fue de otra persona; las madres jóvenes (20 a 29 años), 12.65%.

# Más hijos de los que deseaba (2014 y 2018)----
# * 2014----
mas_hijos_2014 <- data_2014 %>% 
  select(fac_per, grupos_edad, p7_16) %>% 
  mutate(p7_16 = ifelse(p7_16==9,NA,p7_16)) %>% 
  drop_na(p7_16) %>% 
  mutate(no_uso_anticon = ifelse(p7_16==1,1,0),
         no_con_anticon = ifelse(p7_16==2,1,0),
         fallo_anticon = ifelse(p7_16==3,1,0),
         pareja_mas = ifelse(p7_16==4,1,0),
         religion = ifelse(p7_16==5,1,0),
         otra = ifelse(p7_16==6,1,0)) %>% 
  group_by(grupos_edad) %>% 
  summarise(t_no_uso_anticon = sum(no_uso_anticon*fac_per, na.rm = T),
            t_no_con_anticon = sum(no_con_anticon*fac_per, na.rm = T),
            t_fallo_anticon = sum(fallo_anticon*fac_per, na.rm = T),
            t_pareja_mas = sum(pareja_mas*fac_per, na.rm = T),
            t_religion = sum(religion*fac_per, na.rm = T),
            t_otra = sum(otra*fac_per, na.rm = T),
            fac_per = sum(fac_per, na.rm = T),
            p_no_uso_anticon = t_no_uso_anticon/fac_per*100,
            p_no_con_anticon = t_no_con_anticon/fac_per*100,
            p_fallo_anticon = t_fallo_anticon/fac_per*100,
            p_pareja_mas = t_pareja_mas/fac_per*100,
            p_religion = t_religion/fac_per*100,
            p_otra = t_otra/fac_per*100) %>% 
  select(grupos_edad, starts_with("p_")) %>% 
  gather(razon,
         porcentaje,
         p_no_uso_anticon:p_otra) %>% 
  mutate(orden_met = case_when(str_detect(razon, "fallo") ~ "2",
                              str_detect(razon, "no_con") ~ "4",
                              str_detect(razon, "no_uso") ~ "1",
                              str_detect(razon, "otra") ~ "5",
                              str_detect(razon, "pareja") ~ "3",
                              str_detect(razon, "relig") ~ "6"),
         orden_met = as.numeric(orden_met),
         razon = case_when(str_detect(razon, "fallo") ~ "Falló el método\nanticonceptivo",
                           str_detect(razon, "no_con") ~ "No conocía métodos\nanticonceptivos",
                           str_detect(razon, "no_uso") ~ "No utilizó métodos\nanticonceptivos",
                           str_detect(razon, "otra") ~ "Otra",
                           str_detect(razon, "pareja") ~ "Pareja deseaba\nmás",
                           str_detect(razon, "relig") ~ "Razones religiosas"),
         orden_edad = case_when(str_detect(grupos_edad, "15-19") ~ "1",
                                str_detect(grupos_edad, "20-29") ~ "2",
                                str_detect(grupos_edad, "30-39") ~ "3",
                                str_detect(grupos_edad, "40-49") ~ "4"),
         orden_edad = as.numeric(orden_edad))

razones_mas_hijos_plot_2014 <- 
  ggplot(data = mas_hijos_2014,
         aes(x = reorder(grupos_edad, orden_edad),
             y = reorder(razon, -orden_met),
             label = paste0(round(porcentaje,2),"%"))) +
  geom_tile(mapping = aes(fill = porcentaje),
            col = "white") +
  scale_fill_gradient2("", low = "#d6e1d6", mid = "#5d8866", high = "#08431f",
                       midpoint = mean(mas_hijos_2014$porcentaje)) +
  geom_text(size = 5) + ggtitle("2014") + xlab("") + ylab("") + 
  theme(legend.position = "none",
        panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 20)) +
  coord_fixed()


# * 2018----
mas_hijos_2018 <- data_2018 %>% 
  select(fac_per, grupos_edad, p7_17) %>% 
  mutate(p7_17 = ifelse(p7_17==9,NA,p7_17)) %>% 
  drop_na(p7_17) %>% 
  mutate(no_uso_anticon = ifelse(p7_17==1,1,0),
         no_con_anticon = ifelse(p7_17==2,1,0),
         fallo_anticon = ifelse(p7_17==3,1,0),
         pareja_mas = ifelse(p7_17==4,1,0),
         religion = ifelse(p7_17==5,1,0),
         otra = ifelse(p7_17==6,1,0)) %>% 
  group_by(grupos_edad) %>% 
  summarise(t_no_uso_anticon = sum(no_uso_anticon*fac_per, na.rm = T),
            t_no_con_anticon = sum(no_con_anticon*fac_per, na.rm = T),
            t_fallo_anticon = sum(fallo_anticon*fac_per, na.rm = T),
            t_pareja_mas = sum(pareja_mas*fac_per, na.rm = T),
            t_religion = sum(religion*fac_per, na.rm = T),
            t_otra = sum(otra*fac_per, na.rm = T),
            fac_per = sum(fac_per, na.rm = T),
            p_no_uso_anticon = t_no_uso_anticon/fac_per*100,
            p_no_con_anticon = t_no_con_anticon/fac_per*100,
            p_fallo_anticon = t_fallo_anticon/fac_per*100,
            p_pareja_mas = t_pareja_mas/fac_per*100,
            p_religion = t_religion/fac_per*100,
            p_otra = t_otra/fac_per*100) %>% 
  select(grupos_edad, starts_with("p_")) %>% 
  gather(razon,
         porcentaje,
         p_no_uso_anticon:p_otra) %>% 
  mutate(orden_met = case_when(str_detect(razon, "fallo") ~ "2",
                               str_detect(razon, "no_con") ~ "4",
                               str_detect(razon, "no_uso") ~ "1",
                               str_detect(razon, "otra") ~ "5",
                               str_detect(razon, "pareja") ~ "3",
                               str_detect(razon, "relig") ~ "6"),
         orden_met = as.numeric(orden_met),
         razon = case_when(str_detect(razon, "fallo") ~ "Falló el método\nanticonceptivo",
                           str_detect(razon, "no_con") ~ "No conocía métodos\nanticonceptivos",
                           str_detect(razon, "no_uso") ~ "No utilizó métodos\nanticonceptivos",
                           str_detect(razon, "otra") ~ "Otra",
                           str_detect(razon, "pareja") ~ "Pareja deseaba\nmás",
                           str_detect(razon, "relig") ~ "Razones religiosas"),
         orden_edad = case_when(str_detect(grupos_edad, "15-19") ~ "1",
                                str_detect(grupos_edad, "20-29") ~ "2",
                                str_detect(grupos_edad, "30-39") ~ "3",
                                str_detect(grupos_edad, "40-49") ~ "4"),
         orden_edad = as.numeric(orden_edad))

razones_mas_hijos_plot_2018 <- 
  ggplot(data = mas_hijos_2018,
         aes(x = reorder(grupos_edad, orden_edad),
             y = reorder(razon, -orden_met),
             label = paste0(round(porcentaje,2),"%"))) +
  geom_tile(mapping = aes(fill = porcentaje),
            col = "white") +
  scale_fill_gradient2("", low = "#d6e1d6", mid = "#5d8866", high = "#08431f",
                       midpoint = mean(mas_hijos_2018$porcentaje)) +
  geom_text(size = 5) + ggtitle("2018") + xlab("") + ylab("") + 
  theme(legend.position = "none",
        panel.background = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 20)) +
  coord_fixed()

razones_plot <- 
  ggarrange(razones_mas_hijos_plot_2014, 
            razones_mas_hijos_plot_2018,ncol= 2, nrow = 1, 
            common.legend = TRUE, legend="right")

fiuf <- "Razón principal por la que ha tenido más hijas(os) de las que deseaba"
fiuffi <- "Fuente: elaboración de Gire con datos de la ENADID 2014 y 2018."
annotate_figure(
  razones_plot,
  top = text_grob(str_wrap(fiuf, width = 55), size = 22),
  bottom = text_grob(fiuffi, size = 10, hjust = 0)
)
ggsave(paste0(out, "razones_mas_hijos_2014_2018.png"),
       height = 30, width = 30, units = "cm")


# Salud pública integral----

# * 2014----
salud_publica_integral_2014 <- subset(data_2014,p8_10<5) %>% 
  select(fac_per, grupos_edad, starts_with("p8_13")) %>% 
  mutate(anticon_atencion_integral = ifelse(p8_13_1==1 &
                                            p8_13_2==1 &
                                            p8_13_3==1 &
                                            p8_13_4==1 &
                                            p8_13_5==1,1,0)) %>% 
  select(fac_per, grupos_edad, anticon_atencion_integral) %>% 
  dplyr::group_by(grupos_edad) %>% 
  summarise(t_anticon_atencion_integral = sum(anticon_atencion_integral*fac_per, na.rm = T),
            fac_per = sum(fac_per, na.rm = T),
            p_anticon_atencion_integral = t_anticon_atencion_integral/fac_per*100) %>% 
  select(grupos_edad, starts_with("p_")) %>% 
  `colnames<-` (c("grupos_edad", "porcentaje")) %>% 
  mutate(año = "2014")

# * 2018----
salud_publica_integral_2018 <- subset(data_2018,p8_13<5) %>% 
  select(fac_per, grupos_edad, starts_with("p8_16")) %>% 
  mutate(anticon_atencion_integral = ifelse(p8_16_1==1 &
                                            p8_16_2==1 &
                                            p8_16_3==1 &
                                            p8_16_4==1 &
                                            p8_16_5==1,1,0)) %>% 
  select(fac_per, grupos_edad, anticon_atencion_integral) %>% 
  dplyr::group_by(grupos_edad) %>% 
  summarise(t_anticon_atencion_integral = sum(anticon_atencion_integral*fac_per, na.rm = T),
            fac_per = sum(fac_per, na.rm = T),
            p_anticon_atencion_integral = t_anticon_atencion_integral/fac_per*100) %>% 
  select(grupos_edad, starts_with("p_")) %>% 
  `colnames<-` (c("grupos_edad", "porcentaje")) %>% 
  mutate(año = "2018")

salud_publica_integral <- bind_rows(salud_publica_integral_2014, 
                                    salud_publica_integral_2018) %>% 
  mutate(orden_edad = case_when(str_detect(grupos_edad, "15-19") ~ "1",
                                str_detect(grupos_edad, "20-29") ~ "2",
                                str_detect(grupos_edad, "30-39") ~ "3",
                                str_detect(grupos_edad, "40-49") ~ "4"),
         orden_edad = as.numeric(orden_edad))

rm(salud_publica_integral_2014, salud_publica_integral_2018)

# * Plot salud pública integral (2014 y 2018)----
fiuf <- "Atención integral al solicitar métodos anticonceptivos en una institución de salud pública"
fiuffi <- "Fuente: elaboración de Gire con datos de la ENADID 2014 y 2018."

salud_publica_integral_plot <- 
  ggplot(data= salud_publica_integral,
         aes(x=reorder(grupos_edad,orden_edad),
             y=porcentaje,
             group=año)) +
  geom_bar(stat = "identity",position = "dodge",
           aes(fill=año)) + 
  scale_fill_manual("",
                    values = c("#92A370","#005024"),
                    labels = c("2014", "2018")) +
  geom_text(aes(label = paste0(round(porcentaje,2),"%")),
            size = 5, position= position_dodge(width = 1),
            vjust = -0.5) +
  labs(title = str_wrap(fiuf, width = 45),
       caption = str_wrap(fiuffi, width = 65)) +
  xlab("") + ylab("") + 
  theme(axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.position = "right",
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 20),
        plot.caption = element_text(size = 10)) 

ggsave(paste0(out, "salud_publica_integral_2014_2018.png"),
       height = 30, width = 30, units = "cm")

# Usó un método 2018----
anticon_2018 <- select(data_2018,
                       llave_muj, ent,
                       fac_per, upm_dis, estrato, # necesarias para estimaciones
                       grupos_edad,edad, niv, # sociodem (edad, escolaridad)
                       starts_with("p8_4")) %>% 
  subset(edad<50) %>% 
  mutate(algun_anti = ifelse(p8_4_01>2 & 
                               p8_4_02>2 & 
                               p8_4_03>2 &
                               p8_4_04>2 & 
                               p8_4_05>2 & 
                               p8_4_06>2 & 
                               p8_4_07>2 & 
                               p8_4_08>2 & 
                               p8_4_09>2 & 
                               p8_4_10>2 & 
                               p8_4_11>2 &
                               p8_4_12>2 &
                               p8_4_13>2 &
                               p8_4_14>2,
                             0,1)) %>% 
  select(fac_per, grupos_edad, algun_anti) %>% 
  dplyr::group_by(grupos_edad) %>% 
  summarise(t_algun_anti = sum(algun_anti*fac_per, na.rm = T),
            fac_per = sum(fac_per, na.rm = T),
            p_algun_anti = t_algun_anti/fac_per*100) %>% 
  select(grupos_edad, starts_with("p_")) %>% 
  `colnames<-` (c("grupos_edad", "porcentaje"))


# hijos ideal 2018----
hijos <- data_2018 %>% 
  mutate(p7_14 = ifelse(p7_14=="99",NA,p7_14),
         no_es_ideal = ifelse(as.numeric(p5_9)<=as.numeric(p7_14),0,1)) %>% 
  #dplyr::group_by(grupos_edad) %>% 
  summarise(t_no_es_ideal = sum(no_es_ideal*fac_per, na.rm = T),
            fac_per = sum(fac_per, na.rm = T),
            p_no_es_ideal = t_no_es_ideal/fac_per*100)

hijos_ <- data_2014 %>% 
  mutate(p7_14 = ifelse(p7_14=="99",NA,p7_14),
         no_es_ideal = ifelse(as.numeric(p5_9)<=as.numeric(p7_14),0,1)) %>% 
  #dplyr::group_by(grupos_edad) %>% 
  summarise(t_no_es_ideal = sum(no_es_ideal*fac_per, na.rm = T),
            fac_per = sum(fac_per, na.rm = T),
            p_no_es_ideal = t_no_es_ideal/fac_per*100)  
