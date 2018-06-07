# ЧАСТЬ-1. формируем исходный датафрейм. 
	# шаг-1. вчитываем таблицу. делаем из нее исходный датафрейм.
MorDF <- read.csv("Morphology.csv", header=TRUE, sep = ",")
head(MorDF)
summary(MorDF)
	# шаг-2. из исходной таблицы берем только 2 нужных столбца. Один нужен как численный, другой - как символьный. Создаем из них исходный датафрейм MDF.
profile<- as.character(MorDF$profile)
tg_angle <- as.numeric(MorDF$tg_angle)
MDF<- data.frame(profile, tg_angle)
head(MDF)

# ЧАСТЬ-2. теперь изменяем исходный датафрем MDF. 
	# шаг-3. создаем новую колонку для названий профилей (здесь: по номерам рядов 1:25)
MDF$"profile name" <- rownames(MDF)  # create new column for car names
	# шаг-4. пересчитываем значение величины аргумента (по оси X) в нормализованное через разность среднего и ст. отклонения (mean / st.deviation)
MDF$norm_tg_angle <- round((MDF$tg_angle - mean(MDF$tg_angle))/sd(MDF$tg_angle), 2)  # compute normalized tg_angle
	# шаг-5. распределяем значения нормализованного аргумента на "выше" и "ниже" среднего
MDF$angle_type <- ifelse(MDF$norm_tg_angle < 0, "below", "above")  # above / below avg flag	
	# шаг-6. сортируем наш датафрейм
MDF <- MDF[order(MDF$norm_tg_angle), ]  # sort
	# шаг-7. значения по оси Y (здесь: названия профилей, см. шаг-1) конвертируем в факторные
MDF$"profile name" <- factor(MDF$"profile name", levels = MDF$"profile name")  # convert to factor to retain sorted order in plot.
class(MDF$profile name) # проверяем класс 
# [1] "factor" - должно получиться (все правильно, "факторный")
MDF # смотрим наш новый датафрейм (теперь 5 колонок vs в исходном были 2 колонки)

# ЧАСТЬ-3. рисуем 2 графика по датафрейму MDF, котрый сделали в части-2.

	# шаг-8. график в стиле "расходящиеся столбики" // Plotting Diverging Barcharts 
Diverging_Bars<- ggplot(MDF, aes(x = MDF$"profile name", y = MDF$norm_tg_angle, label = MDF$norm_tg_angle)) + 
  geom_bar(stat='identity', aes(fill = MDF$angle_type), width=.5) +
  xlab("Profiles, Nr.") +
  ylab(expression(tg*degree*(A/H))) +
  scale_fill_manual(name="(tg(A/H))", 
                    labels = c("Above Average", "Below Average"), 
                    values = c("above"="lawngreen", "below"="coral1")) + 
  labs(title= "Mariana Trench. Diverging Bars",
  	subtitle=expression(paste("Normalized steepness", tg*degree*(A/H), "vs profiles 1:25"))) + 
  coord_flip() +
  theme(plot.title = element_text(size = 10), 
    		legend.title = element_text(size=8), legend.text = element_text(colour="black", size = 8)) 
Diverging_Bars
   
	# шаг-9. график в стиле "леденец на палочке" // Plotting "Lollipop Chart"
Lollipop <- ggplot(MDF, aes(x = MDF$"profile name", y = MDF$norm_tg_angle, label = MDF$norm_tg_angle)) +    
	xlab("Profiles, Nr.") +
	ylab(expression(tg*degree*(A/H))) +
	geom_point(stat='identity', fill="black", size=6)  +   
	geom_segment(aes(y = 0, x = MDF$"profile name", yend = MDF$norm_tg_angle, xend = MDF$"profile name"), color = "black") +   
	geom_text(color="white", size=2) +   
	labs(title="Mariana Trench: Diverging Lollipop Chart",          
	subtitle=expression(paste("Normalized steepness", tg*degree*(A/H), "vs profiles 1:25"))) +
	ylim(-2.5, 2.5) +   
coord_flip() +
    theme(plot.title = element_text(size = 10), legend.title = element_text(size=8), legend.text = element_text(colour="black", size = 8)) 
Lollipop

	# шаг-10. размещаем оба графика на один лист. 
figure <-plot_grid(Diverging_Bars, Lollipop, labels = c("1", "2"), ncol = 2, nrow = 1)

	# шаг-11. добавляем к ним общий заголовок, подзаголовок и нижнюю сноску.
LollipopBar <- figure +						
	labs(title="马里亚纳海沟。剖面1-25。Mariana Trench, Profiles Nr.1-25.", 
	subtitle = "统计图表。地貌聚类分析, 条形图。Geomorphological Analysis: Normalised Steepness Angles",
	caption = "Statistics Processing and Graphs: \nR Programming. Data Source: QGIS") +
	theme(
		plot.margin = margin(5, 10, 20, 5),
		plot.title = element_text(margin = margin(t = 0, r = 20, b = 5, l = 0), family = "Kai", face = "bold", size = 12), # китайский шрифт "Кай"
		plot.subtitle = element_text(margin = margin(t = 0, r = 20, b = 4, l = 0), family = "Hei", face = "bold", size = 10), # китайский шрифт "Хэй"
		plot.caption = element_text(face = 2, size = 6),
		panel.background=ggplot2::element_rect(fill = "white"),
		legend.justification = "bottom", 
		legend.position = "bottom",
		legend.box.just = "right",
		legend.direction = "horizontal",
		legend.box = "horizontal",
		legend.box.background = element_rect(colour = "honeydew4",size=0.2),
		legend.background = element_rect(fill = "white"),
		legend.key.width = unit(1,"cm"),
		legend.key.height = unit(.5,"cm"),
		legend.spacing.x = unit(.2,"cm"),
		legend.spacing.y = unit(.1,"cm"),
		legend.text = element_text(colour="black", size=6, face=1),
		legend.title = element_text(colour="black", size=6, face=1))
LollipopBar
