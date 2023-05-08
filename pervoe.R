#Работу радостно выполнил: Ахметжанов Д. М. Д-А 131

library(tidyverse)
library(dplyr)
library(rnoaa)
library(lubridate)
#скачиваем станции
#station_data = ghcnd_stations()

#station_data
#write.csv(station_data, file = "stations.csv")
station_data = read.csv("stations.csv")

####После получения списка станций, получим список станций ближайщих к столице региона и координами его столицы
buryatiya = data.frame(id = "BURYATIYA", latitude = 51.834811, longitude = 107.584545)

buryatiya_around = meteo_nearby_stations(lat_lon_df = buryatiya, station_data = station_data,
                                         limit = 12, var = c("PRCP", "TAVG"),
                                         year_min = 1999, year_max = 2003)

buryatiya_around #buryatiya_around  от список содержащий индификаторы метеостанций, отсортированные по их удаленности от Улан-Удэ
buryatiya_around = buryatiya_around[[1]]

#отфильтруем станции по расстоянию 
buryatiya_around = buryatiya_around %>% dplyr::filter(distance <=150)
buryatiya_id = buryatiya_around[["BURYATIYA"]][["id"]][1]
summary(buryatiya_id)

buryatiya_id = buryatiya_around$id
all_buryatiya_data = meteo_tidy_ghcnd(stationid = buryatiya_id)

#выбираем целиком первый объект из списка
buryatiya_table=buryatiya_around[[1]]
summary(buryatiya_table)
buryatiya_table

# в таблице buryatiya_table оказалось 10 объектов ранжированых по расстоянию от Улан-Удэ
#сформируем список необходимых станций
buryatiya_stations = buryatiya_table
str(buryatiya_stations)

#выводим индетификаторы отфильтрованных метиостанций
buryatiya_id = buryatiya_around$id

#создаем цикл, в котором бы скачивались нужные данные для всех метеостанций
#создадим объект, куда скачаем все данные всех метеостанций
all_buryatiya_data=meteo_tidy_ghcnd(stationid = buryatiya_id)
summary(all_buryatiya_data)

#Создаем объект куда скачаем все данные всех метеостанций(колличество)
all_buryatiya_meteodata = data.frame()
#Создаем цикл для метеостанций
stations_names = buryatiya_around$id
stations_names=stations_names[1:10]

for (sname in stations_names)
{one_meteo=meteo_tidy_ghcnd( stationid = sname,
                              date_min = "1999-01-01",
                              date_max = "2003-12-31")
  station_vars=names(one_meteo)
  if (!("tavg" %in% station_vars)){
    if(!("tmax"%in% station_vars)){
      next()
    }
    
    
    
    
    one_meteo=one_meteo %>% mutate(tavg=(tmax+tmin)/2)}
  one_meteo=one_meteo %>% select(id,date,tavg)
  one_meteo = one_meteo %>% mutate(tavg=tavg/10)
  all_buryatiya_meteodata=rbind(all_buryatiya_meteodata, one_meteo)}

#Записываем полученные результаты
write.csv(all_buryatiya_meteodata,"all_buryatiya_meteodata.csv")
#считываем данные 
all_buryatiya_meteodata=read.csv("all_buryatiya_meteodata.csv")
#смотрим, что получилось
str(all_buryatiya_meteodata)

#добавим год, месяц, день
all_buryatiya_meteodata = all_buryatiya_meteodata %>% mutate(year=year(date), 
                                                               month=month(date), 
                                                               day=day(date))
#Превратим NA в 0 и где tavg<5
all_buryatiya_meteodata[is.na(all_buryatiya_meteodata$tavg),"tavg"] = 0
all_buryatiya_meteodata[all_buryatiya_meteodata$tavg<5, "tavg"] = 0
summary(all_buryatiya_meteodata)

#Сгрупируем метеостанции по id, месяцам и годам и проссумируем температуру по этим группа, затем сгруппируем данные по месяцам и найдем среднее по месяцам для всех метеостанций
group_meteodata =all_buryatiya_meteodata %>% group_by(id,year,month)

sumT_group_meteodata = group_meteodata %>% summarise(tsum=sum(tavg))
groups_month=sumT_group_meteodata%>%group_by(month)


sumT_month=groups_month%>%summarise(St=mean(tsum))

##Подготовка к расчету по формуле урожая
#Ввод констант
y = 1.0 #коэффициент для экспозиции склона 
afi = c(0.00,0.00,0.00,32.11, 26.31,25.64,23.20,18.73,16.30,13.83,0.00,0.00)#константа, из табл.1

bfi = c(0.00, 0.00, 0.00, 11.30, 9.26, 9.03,8.16, 6.59, 5.73, 4.87, 0.00, 0.00)#константа, из табл.1

# month_ratio
# di = sum(tavg>7)/30
#di = sum(tavg>7)/30
tdi = all_buryatiya_data %>% mutate(date=ymd(date),
                                    year=year(date),
                                    month=(month(date))) %>% group_by(year,month) %>% 
                                    mutate (dm = n(),gd = case_when( tavg>=8~T, tavg<8~F))%>% 
                                    summarise(di = sum(gd/mean(dm,na.rm=T),na.rm=T))
tdi = tdi %>% filter(year > 1998 & year < 2004) %>% ungroup() %>% group_by(month) %>% 
                                                                            summarise(di = mean(di,na.rm=T))
tdi$di[0:3]=0
tdi$di[8] = 0.52
tdi$di[9:12]=0
di = tdi$di
#отношение числа i-го месяца, входящих в период вегетации культуры, к общему числу дней в месяце из табл.1
Kf = 300 #  коэффициент использования ФАР посевом
Qj = 1600 # калорийность урожая культуры
Lj = 2.2 #  коэффициент «Сумма частей основной и побочной продукции
Ej = 25 #   коэффициент «Стандартная влажность культуры

#Расчитаем Fi по месяцам 
sumT_month =sumT_month %>% mutate(Fi = afi+bfi*y*St)

#Расчитаем Yi
sumT_month = sumT_month %>% mutate( Yi = ((Fi*di)*Kf)/(Qj*Lj*(100-Ej)))

#Расчитаем урожай
Yield = (sum(sumT_month$Yi)) 
Yield 

#Результат 
