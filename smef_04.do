* Семинар 4.

clear

set more off // отключить полный вывод результатов работы команд
cd /Users/polinapogorelova/Desktop/СМЭФ_Эконометрика/Эконометрика_24_25 // директория, в которой хранятся файлы
log using seminar3.log, text replace // начало записи результатов do-файла в seminar3.log

* Множественная регрессия
* Импорт данных
import excel "data_flats.xls", sheet("data") firstrow

gen price_sq = price/totsp // сгенерируем новую зависимую переменную "Стоимость 1 кв.м."
gen kittot = kitsp/livesp // сгенерируем новую объясняющую переменную "Доля кухни"
gen kittot2 = kittot^2

reg price_sq livesp kittot kittot2 metrdist dist
reg price_sq livesp kittot kittot2 metrdist dist, vce(robust) // оценивание модели линейной регрессии с помощью МНК + робастные стандартные ошибки в форме Уайта
eststo model1 // сохраняет копию активных результатов оценки модели регрессии для последующего сведения в таблицу
outreg2 using myreg.doc, replace ctitle(Model 1) bdec(2) sdec(3)  // экспорт результатов оценивания в виде таблицы в файл с форматом .doc (.xls)

ereturn list // вывод на экран результатов оценивания, непредставленных в основном блоке (имена и значения макросов и скаляров, а также имена и размеры матриц, хранящиеся в e()
matrix b = e(b) // матрица коэффициентов
matrix list b // вывод матрицы коэффициентов на экран
matrix cov = e(V) // сохраняем ковариационную матрицу коэффициентов
matrix list cov // вывод ковариационной матрицы на экран
estat vce // вывод на экран ковариационной матрицы оценок коэффициентов модели
 
predict resid_price, r // сохраняем остатки модели в переменную resid_price
hist resid_price, norm // гистограмма распределения остатков
twoway (hist resid_price) (kdensity resid_price)
qnorm resid_price
sktest resid_price // тест Харке-Бера на нормальность (для остатков модели)
swilk resid_price // тест Шапиро-Уилка на нормальность (4<=n<=2000)


predict price_sq_hat // генерирует прогнозные значения
rvfplot, yline(0) // график зависимости остатков от модельных значений (явные признаки гетероскедастичности)
twoway scatter price_sq_hat price_sq // график зависимости прогнозных значений цена квартиры за 1 кв.м. в зависимости от истинных значений переменной


* Тестирование гипотез о равенстве коэффициентов (или их линейной комбинации) некоторому значению
test metrdist = -0.05 // проверка гипотезы о равенстве коэффициента при переменной metrdist значению -0.05
test metrdist = dist // проверка гипотезы о равенстве коэффициентов при переменных metrdist и dist
test metrdist = dist = -0.05 // проверка гипотезы о равенстве коэффициентов при переменны metrdist и dist значению -0.05

reg price_sq totsp kittot kittot2 metrdist dist, beta // регрессия для стандартизированных показателей


* Фиктивные переменные
* i.namevar - способ задания дамми-переменной

gen metrdistwalk = metrdist*walk // создание перекрестной фиктивной переменной

reg price_sq livesp kittot kittot2 metrdist dist i.brick i.walk, vce(robust)

* Построим диаграммы рассеяния "стоимость 1 кв.м. – жилая площадь" в зависимости от кирпичности дома
twoway (scatter price_sq livesp if brick == 0, msymbol(S)) ///
  (scatter price_sq livesp if brick == 1, msymbol(Oh)), ///
  legend(label(1 non brick) label(2 brick))

twoway (scatter price_sq livesp) (lfit price_sq livesp),  by(brick, total)

* Тест Чоу о равенстве коэффициентов модели
* H0: коэффициенты при переменных в моделях, оцененных на разных группах наблюдений, равны
* H1: модели для двух групп наблюдений отличаются хотя бы одним коэффициентом

reg price_sq livesp kittot kittot2 metrdist dist i.brick, vce(robust) // оценивание модели на всей выборке
scalar r_all = e(rss) // сохранение суммы квадратов остатков модели
scalar n_all = e(N) // сохранение общего числа наблюдений

reg price_sq livesp kittot kittot2 metrdist dist i.brick if walk == 1, vce(robust) // оценивание модели для квартир в пешей доступности до метро
scalar r_walk = e(rss) // сохранение суммы квадратов остатков модели, оцененной для квартир, расположенных в пешей доступности от метро
scalar n_walk = e(N) // сохранение общего числа наблюдений
scalar k = e(df_m) + 1 // сохранение числа степеней свободы


reg price_sq livesp kittot kittot2 metrdist dist i.brick if walk == 0, vce(robust) // оценивание модели для квартир, не находящихся в пешей доступности от метро
scalar r_nowalk = e(rss) // сохранение суммы квадратов остатков модели, оцененной для квартир, не расположенных в пешей доступности от метро
scalar n_nowalk = e(N)
scalar ddf = n_nowalk + n_walk - k*2 // расчет степеней свободы для знаменателя


scalar r_tot = r_walk + r_nowalk // сохранение суммы квадратов остатков моделей, оцененных по разным подвыборкам
scalar f_obs = ((r_all-r_tot)/(k))/(r_tot/ddf) // расчет тестовой статистики
scalar pval = Ftail(k,ddf,f_obs) // определение p-value

scalar list ddf f_obs pval // вывод результатов


* Эндогенность.
clear
use CARD.DTA
describe // описание набора данных
summarize // описательные статистики переменных

* 1. Метод наименьших квадратов
reg lwage educ exper expersq black south smsa smsa66 reg662 reg663 reg664 reg665 reg666 reg667 reg668 reg669
est store ols
estat ovtest // RESET-test  (тест Рамсея). Ho: нет пропущенных переменных
predict yhat, xb // сохраняем прогноз по модели
tabstat yhat, statistics(mean) by(south) // среднее значение прогноза по двум категориям бинарной переменной south (tabstat отображает сводную статистику для ряда числовых переменных в одной таблице)
adjust exper = 10, by(south) // команда adjust отображает среднее модельное значение зависимой переменной для опыта = 10 лет в каждой категории переменной south

* 2. Метод инструментальных переменных и 2-МНК
* Проанализируем, какие из имеющихся переменных могут быть инструментами для educ

* nearc4 - инструмент для educ
ivregress 2sls lwage (educ = nearc4) exper expersq black south smsa smsa66 reg662 reg663 reg664 reg665 reg666 reg667 reg668 reg669, first
est store iv

estat firststage // тестирование релевантности (качества) инструментов. H0: инструменты слабые
hausman iv ols // тест Хаусмана на проверку эндогенности регрессоров. H0: OLS-оценки. H1: IV-оценки

* nearc4 и nearc2 - инструменты для educ
ivregress 2sls lwage (educ = nearc4 nearc2) exper expersq black south smsa smsa66 reg662 reg663 reg664 reg665 reg666 reg667 reg668 reg669, first
est store iv2
estat firststage // тестирование релевантности (качества) инструментов. H0: инструменты слабые
estat overid // тестирование валидности (экзогенности) инструментов (l>=k) (тест Саргана). H0: все инструменты экзогенные (не коррелируют с ошибкой). H1: хотя бы один из инструментов эндогенный
hausman iv2 ols // тест Хаусмана на проверку эндогенности регрессоров. H0: OLS-оценки. H1: IV-оценки

* fatheduc, motheduc - инструменты для educ
ivregress 2sls lwage (educ = fatheduc motheduc) educ exper expersq black south smsa smsa66 reg662 reg663 reg664 reg665 reg666 reg667 reg668 reg669, first
est store iv3
* Тестирование инструментов
estat firststage // тестирование релевантности (качества) инструментов. H0: инструменты слабые
estat overid // тестирование валидности (экзогенности) инструментов (l>=k) (тест Саргана). H0: все инструменты экзогенные (не коррелируют с ошибкой). H1: хотя бы один из инструментов эндогенный
hausman iv3 ols // тест Хаусмана на проверку эндогенности регрессоров. H0: OLS-оценки. H1: IV-оценки

* nearc4, fatheduc, motheduc - инструменты для educ
ivregress 2sls lwage (educ = nearc4 fatheduc motheduc) educ exper expersq black south smsa smsa66 reg662 reg663 reg664 reg665 reg666 reg667 reg668 reg669, first
estat iv4
estat firststage
estat overid
hausman iv4 ols // тест Хаусмана на проверку эндогенности регрессоров. H0: OLS-оценки. H1: IV-оценки

log close // заканчиваем запись в файл
