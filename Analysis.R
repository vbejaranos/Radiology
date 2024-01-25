library(tidyverse)
library(openxlsx)

rm(list = ls())

# PPP
PPP <- read.xlsx("Implicit deflator.xlsx") %>%
  slice(3) %>% select(-c("Country_Name","Country_Code","Indicator_Code")) %>%
  pivot_longer(cols = -Indicator_Name,names_to = 'year',values_to = 'PPP_USD') %>%
  mutate(year = as.numeric(substr(year,1,4))) %>% filter(!is.na(PPP_USD)) %>%
  select(-Indicator_Name)

# GD 
GD <- readRDS(file = 'GD of radiology for age, sex and region.rds')
GD_cupgr <- readRDS(file = 'GD by type of radiology, for age, sex and region.rds')
GD_tot <- readRDS(file = 'Total GD by age, sex and region.rds')
sexs <- data.frame(sexo = c('FEMENINO','MASCULINO'),
                   sex = c('Female','Male'))  
GD <- GD %>% mutate(age2 = as.numeric(gsub("[Dea ]","",substr(age,1,6)))) %>%
  mutate(agegr = case_when(age2 %in% 0:11~ "0-11",
                           age2 %in% 12:26~ "12-26",
                           age2 %in% 27:59~ "27-59",
                           age2 >= 60~ ">60",TRUE~NA)) %>%
  left_join(sexs, by = "sexo") %>% filter(sexo %in% sexs$sexo) %>%
  select(-sexo) %>% filter(age2 <= 100)
GD_cupgr <- GD_cupgr %>% mutate(age2 = as.numeric(gsub("[Dea ]","",substr(age,1,6)))) %>%
  mutate(agegr = case_when(age2 %in% 0:11~ "0-11",
                           age2 %in% 12:26~ "12-26",
                           age2 %in% 27:59~ "27-59",
                           age2 >= 60~ ">60",TRUE~NA)) %>%
  left_join(sexs, by = "sexo") %>% filter(sexo %in% sexs$sexo) %>%
  select(-sexo) %>% filter(age2 <= 100)
GD_tot <- GD_tot %>% mutate(age2 = as.numeric(gsub("[Dea ]","",substr(age,1,6)))) %>%
  mutate(agegr = case_when(age2 %in% 0:11~ "0-11",
                           age2 %in% 12:26~ "12-26",
                           age2 %in% 27:59~ "27-59",
                           age2 >= 60~ ">60",TRUE~NA)) %>%
  left_join(sexs, by = "sexo") %>% filter(sexo %in% sexs$sexo) %>%
  select(-sexo) %>% filter(age2 <= 100)

# Groups of CUPS
cupsgr <- data.frame(CUPS3 = as.character(c(870:873,874:878,879,881,882,883,886:887)),
                     type = c(rep('General radiology',4),
                              rep('Special and interventional radiology',5),
                              'Computed tomography','Ultrasound',
                              'Noninvasive vascular studies',
                              'Magnetic resonance imaging',
                              rep('Other imaging techniques and studies',2)))

# Regions
regions1 <- read.csv("Categorias_de_Ruralidad.csv",encoding = "UTF-8",
                     sep = ";")
regs <- regions1 %>% mutate(region = 
                              ifelse(Categoria_ %in% c("Ciudades y aglomeraciones","Intermedio"),
                                     "Urban","Rural"),
                            COD_MUNI = ifelse(nchar(Municipaly) == 5,Municipaly,
                                              formatC(Municipaly,width = 5,flag = "0"))) %>%
  select(COD_MUNI,Municipali,region)

# Results

# Table 1
tab1_1 <- GD %>% mutate(year = as.numeric(year)) %>% group_by(year,region,agegr) %>% 
  summarise(pu = sum(pu),vm = sum(vm),.groups = 'drop') %>% 
  left_join(PPP,by = 'year') %>% mutate(avg = (vm/pu)/PPP_USD) %>%
  group_by(agegr,region) %>%
  summarise(m = mean(avg),s = sd(avg),.groups = 'drop')
tab1_2 <- GD %>% mutate(year = as.numeric(year)) %>% group_by(year,agegr) %>% 
  summarise(pu = sum(pu),vm = sum(vm),.groups = 'drop') %>% 
  left_join(PPP,by = 'year') %>% mutate(avg = (vm/pu)/PPP_USD) %>%
  group_by(agegr) %>%
  summarise(m = mean(avg),s = sd(avg),.groups = 'drop') %>%
  mutate(region = 'Total')
tab1 <- bind_rows(tab1_1,tab1_2) %>% 
  pivot_wider(names_from = region,values_from = c(m,s))

# Figure 1A
f1 <- GD %>% mutate(year = as.numeric(year)) %>% group_by(year,age2,region,sex) %>% 
  summarise(pu = sum(pu),vm = sum(vm),.groups = 'drop') %>% 
  left_join(PPP,by = 'year') %>% mutate(avg = (vm/pu)/PPP_USD) %>%
  mutate(var = paste0(sex,'-',region))
f1 %>% ggplot(aes(x = age2,y = avg,col = var)) + geom_line(linewidth = 1) +
  facet_wrap(year~.,nrow = 3,ncol = 3,scales = 'free_y') +
  scale_x_continuous(breaks = seq(0,100,10)) +
  scale_y_continuous(n.breaks = 10) +
  labs(x = 'Simple age',y = 'Annual per capita expenditure on medical imaging (PPP US dollars)',
       col = '') +
  theme_minimal(base_size = 26) + theme(legend.position = 'bottom')

# Figure 1B
f1b <- GD %>% mutate(year = as.numeric(year)) %>% 
  group_by(year,region,sex) %>% 
  summarise(pu = sum(pu),vm = sum(vm),.groups = 'drop') %>% 
  left_join(PPP,by = 'year') %>% mutate(vm1 = vm/PPP_USD) %>%
  mutate(var = paste0(sex,'-',region)) %>%
  group_by(year) %>% mutate(tot = sum(vm1)) %>% ungroup %>%
  mutate(prop = vm1/tot)
f1b %>% ggplot(aes(x = year, y = prop, fill = var)) + 
  geom_area(stat = 'identity') +
  scale_y_continuous(n.breaks = 10,labels = scales::percent_format(scale = 100)) +
  scale_x_continuous(breaks = seq(2013,2021,)) +
  labs(x = 'Year',y = 'Distribution of total expenditure on medical imaging',
       fill = '') +
  theme_minimal(base_size = 26) + theme(legend.position = 'bottom')

# Figure 2
f2 <- GD %>% mutate(year = as.numeric(year)) %>% group_by(year,age2,region,sex) %>% 
  summarise(pu = sum(pu),vm = sum(vm),.groups = 'drop') %>% 
  left_join(PPP,by = 'year') %>% mutate(avg = (vm/pu)/PPP_USD) %>%
  mutate(var = paste0(sex,'-',region)) %>% 
  left_join(GD_tot %>% mutate(year = as.numeric(year)) %>% group_by(year,age2,region,sex) %>%
              summarise(pu = sum(pu),vm = sum(vm),.groups = 'drop') %>% 
              left_join(PPP,by = 'year') %>% mutate(tot = (vm/pu)/PPP_USD),
            by = c('year','age2','region','sex','PPP_USD')) %>% 
  mutate(prop = avg/tot*100) 
f2 %>%
  ggplot(aes(x = age2,y = prop,col = var)) + geom_line(linewidth = 1) +
  facet_wrap(year~.,nrow = 3,ncol = 3,scales = 'free_y') +
  scale_y_continuous(n.breaks = 10,labels = scales::percent_format(scale = 1)) +
  scale_x_continuous(breaks = seq(0,100,10)) +
  labs(x = 'Simple age',y = 'Percentage of per capita health care spending on medical imaging services',
       col = '') +
  theme_minimal(base_size = 26) + theme(legend.position = 'bottom')

# Figure 3
f3 <- GD %>% mutate(year = as.numeric(year)) %>% group_by(year,age2,region,sex) %>% 
  summarise(pu = sum(pu),aten = sum(aten),.groups = 'drop') %>% 
  mutate(freq = (aten/pu)) %>%
  mutate(var = paste0(sex,'-',region))
f3 %>%
  ggplot(aes(x = age2,y = freq,col = var)) + geom_line(linewidth = 1) +
  facet_wrap(year~.,nrow = 3,ncol = 3,scales = 'free_y') +
  #scale_y_continuous(n.breaks = 10,labels = scales::percent_format(scale = 1)) +
  scale_x_continuous(breaks = seq(0,100,10)) +
  labs(x = 'Simple age',y = 'Annual frequency on medical imaging services',
       col = '') +
  theme_minimal(base_size = 26) + theme(legend.position = 'bottom')

# Correlation
GD %>% mutate(year = as.numeric(year)) %>% group_by(year,age2) %>%
  summarise(pu = sum(pu),vm = sum(vm),.groups = 'drop') %>%
  left_join(PPP,by = 'year') %>% mutate(avg = (vm/pu)/PPP_USD) %>%
  group_by(age2) %>% summarise(m = mean(avg),.groups = 'drop') %>%
  mutate(cor(age2,m)) %>% arrange(m %>% desc)

GD %>% mutate(year = as.numeric(year)) %>% group_by(year,sex,region) %>%
  summarise(pu = sum(pu),vm = sum(vm),.groups = 'drop') %>%
  left_join(PPP,by = 'year') %>% mutate(avg = (vm/pu)/PPP_USD) %>% 
  group_by(sex,region) %>% summarise(m = mean(avg)) %>% arrange(region)

# Figures S1-S7
tabS <- GD_cupgr %>% mutate(year = as.numeric(year)) %>% 
  group_by(year,age2,region,sex,type) %>% 
  summarise(pu = sum(pu),vm = sum(vm),.groups = 'drop') %>% 
  left_join(PPP,by = 'year') %>% mutate(avg = (vm/pu)/PPP_USD) %>%
  mutate(var = paste0(sex,'-',region)) 

cod <- 'U'
for (cod in c('G','S','C','N','M','O','U')){
  limits_data <- tabS %>% filter(substr(type,1,1) == cod,age2 < 100) %>%
    group_by(year) %>% summarise(y_max = max(avg)+10)
  tabS %>% filter(substr(type,1,1) == cod) %>%
    left_join(limits_data, by = "year") %>%
    mutate(truncated_avg = ifelse(avg > y_max,NA,avg)) %>%
    ggplot(aes(x = age2, y = truncated_avg, col = var)) +
    geom_line(linewidth = 1) +
    facet_wrap(year~.,nrow = 3,ncol = 3,scales = 'free_y') +
      labs(x = 'Simple age',y = 'Annual per capita expenditure on medical imaging (PPP US dollars)',
           col = '') +
      scale_x_continuous(breaks = seq(0,100,10)) +
      scale_y_continuous(n.breaks = 10) +
      theme_minimal(base_size = 26) + theme(legend.position = 'bottom')
}

# Figures S8-S14
tabs8 <- GD_cupgr %>% mutate(year = as.numeric(year)) %>% 
  group_by(year,age2,region,sex,type) %>% 
  summarise(pu = sum(pu),aten = sum(aten),.groups = 'drop') %>% 
  mutate(freq = (aten/pu)) %>% mutate(var = paste0(sex,'-',region)) 

cod <- 'U'
for (cod in c('G','S','C','N','M','O','U')){
  tabs8 %>% filter(substr(type,1,1) == cod) %>%
    ggplot(aes(x = age2, y = freq, col = var)) +
    geom_line(linewidth = 1) +
    facet_wrap(year~.,nrow = 3,ncol = 3,scales = 'free_y') +
    labs(x = 'Simple age',y = 'Annual frequency on medical imaging',
         col = '') +
    scale_x_continuous(breaks = seq(0,100,10)) +
    scale_y_continuous(n.breaks = 10) +
    theme_minimal(base_size = 26) + theme(legend.position = 'bottom')
}

# Figures S15-S18
tabs15 <- GD_cupgr %>% mutate(year = as.numeric(year)) %>% 
  group_by(year,age2,region,sex,type) %>% 
  summarise(pu = sum(pu),vm = sum(vm),.groups = 'drop') %>% 
  left_join(PPP,by = 'year') %>% mutate(avg = (vm/pu)/PPP_USD) %>%
  full_join(expand.grid(type = cupsgr %>% group_by(type) %>% 
                          summarise(.groups = 'drop') %>% pull(type),
                        year = 2013:2021,sex = c('Female','Male'),
                        region = c('Rural','Urban'),
                        age2 = 0:100),
            by = c('type','year','sex','region','age2')) %>%
  replace_na(list(avg = 1e-10)) %>% 
  mutate(var = paste0(sex,'-',region)) %>% 
  group_by(year,age2,region,sex) %>% mutate(tot = sum(avg)) %>% ungroup %>%
  mutate(prop = avg/tot)

aux <- c("Female-Rural")
for (aux in c("Female-Urban","Male-Urban","Female-Rural","Male-Rural")){
  tabs15 %>% filter(var == aux) %>%
    ggplot(aes(x = age2, y = prop, fill = type)) + 
    geom_area(stat = 'identity') +
  facet_wrap(.~year,nrow = 3,ncol = 3) +
  scale_y_continuous(n.breaks = 10,labels = scales::percent_format(scale = 100)) +
    scale_x_continuous(breaks = seq(0,100,10)) +
    labs(x = 'Simple age',y = 'Distribution of per capita expenditure on medical imaging',
         fill = '') +
    theme_minimal(base_size = 26) + theme(legend.position = 'bottom')
}
  