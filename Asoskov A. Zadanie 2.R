# Асосков А.А. постройте картосхему средних высот стволов деревьев родов Ольха и Боярышник

# Устанавливаем нужные пакеты 
# install.packages('sf')
# install.packages('ggplot2')
# install.packages('readr')
# install.packages('dplyr')
# install.packages('tidyr')

# Подключаем установленные пакеты
library(sf)
library(ggplot2)
library(readr)
library(dplyr)
library(tidyr)

#считаем данные в переменные
greendb = read.csv('greendb.csv'); greendb
map = sf :: read_sf('moscow.geojson')

# Строим график с заливкой
ggplot(map) + geom_sf(aes(fill = NAME)) + theme(legend.position = 'none')

# Из столбца с названиями видов извлекаем род дерева. Затем создаём новый столбец Genus в исходных данных.
spec = greendb$species_ru
gen = stringr::str_split(spec, pattern=' ',simplify = TRUE)[,1]
data = greendb %>% mutate(Genus = gen)

# Рассчитываем среднюю высоту стволов
sv = data %>% group_by(adm_region, Genus) %>% 
  summarise(s_v = mean(height_m, na.rm = TRUE)) %>% 
  filter(Genus %in% c('Ольха','Боярышник'))
sv

# Приобразовываем данные
sv = pivot_wider(sv, names_from = Genus, values_from = s_v)

# Объединяем данные карты и средних высот
map = map %>% mutate(adm_region = NAME)
map=left_join(map, sv, by='adm_region')

# Строим карту для Ольхи
ggplot(map) + geom_sf(aes(fill=`Ольха`)) + ggtitle('Средняя высота Ольхи') 
+ theme()

# Строим карту для Боярышника
ggplot(map) + geom_sf(aes(fill=`Боярышник`)) + ggtitle('Средняя высота Боярышника') + theme()

