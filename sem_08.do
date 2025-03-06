clear
set more off
cd /Users/polinapogorelova/Desktop/
log using sem_8.log

* Tobit I. Цензурированная регрессия. Пример 1 (данные о дополнительных выплатах сотрудникам)
use http://fmwww.bc.edu/ec-p/data/wooldridge/fringe.dta, clear

sum annbens exper age educ tenure married male // описательные статистики

reg annbens exper expersq age annearn tenure // линейная модель регрессии, оцененная с помощью МНК
predict yhat_lm // прогноз доп. выплат согласно линейной модели регрессии

tobit annbens exper age annearn tenure, ll(0) // тобит-модель для данных, цензурированных слева нулем
predict yhat_tobit // прогноз доп. выплат согласно tobit-модели

margins, dydx(*) predict(ystar(0,.)) // предельный эффект для усеченной переменной
margins, dydx(*) predict(pr(0,.)) // вычисляет предельный эффект на Pr(a < yj < b) (вероятность того, что латентная переменная будет наблюдаемоа в интервале (a,b))


* Tobit II (Heckman model)
use http://www.stata.com/data/jwooldridge/eacsap/mroz,clear

tab inlf // таблица частот для переменной inlf
sum lwage if inlf == 1 // описательные статистики зарплаты работающих женщин

* Обычная модель линейной регрессии для логарифма зарплаты
reg lwage educ exper c.exper#c.exper, vce(robust)

* Оценивание модели Хекмана с помощью ММП
heckman lwage educ exper c.exper#c.exper, select(inlf = educ exper age nwifeinc c.exper#c.exper kidslt6 kidsge6) 

* Sigma - стандартная отклонение остатков в уравнении зарплаты
* Rho - коэффициент корреляции между ошибками в уравнении для участия и для интенсивности участия
* Lambda = rho*sigma - эффект самоотбора


* Оценивание модели Хекмана с помощью двухшаговой процедуры
* Шаг 1: probit-модель для inlf
probit inlf educ exper age nwifeinc c.exper#c.exper kidslt6 kidsge6
predict inlf_hat
gen imr = normalden(inlf_hat)/normal(inlf_hat)
label variable imr "inverse Mills ratio"

* Шаг 2: модель регрессии для зарплаты
reg lwage educ exper c.exper#c.exper imr, vce(robust)

* Двухшаговая процедура может быть воспроизведена автоматически с помощью опции twostep
heckman lwage educ exper c.exper#c.exper, select(inlf = educ exper c.exper#c.exper age nwifeinc kidslt6 kidsge6) twostep

* Прогнозирование
* ожидаемая заработная плата среди всех женщин, независимо от того, работали они на момент опроса или нет, т.е. E(y)
predict allwage
summarize allwage

* ycond вычисляет ожидаемое значение зависимой переменной при условии, что зависимая переменная наблюдаема, т.е. E(y|y was observed)
predict dwage, ycond
sum dwage
sum lwage dwage if lwage != .
 
* yexpected вычисляет ожидаемое значение зависимой переменной (y) для всех женщин,
* где зарплата принимается равной 0, когда ожидается, что она не будет наблюдаться;
* E(y) = P(y observed) * E(y|y was observed) + P(y unobserved) * 0. 
predict hcndwage1, yexpected

gen lwage0 = lwage
replace lwage0 = 0 if lwage == .
summarize hcndwage1 lwage0


* Пределные эффекты (Margins effect)
* предельный эффект только для уравнения интенсивности участия (зарплаты)
* нет четкой интерпретации, если независимая переменная присутствует в обоих уравнениях
margins, dydx(exper educ) atmean

* psel – предельный эффект для уравнения отбора - влияние на вероятность того, что inlf = 1
margins, dydx(exper educ kidslt6) predict(psel) atmean

* предельный эффект для ожидаемого среднего значения зависимой переменной (зарплаты), если женщина действительно работала на момент опроса
margins, dydx(exper educ) predict(ycond)

* предельный эффект для ожидаемой зарплаты для всех женщин, учитывая их вероятность работать,
* предполагается, что lwage=0, если ожидается, что женщина не будет работать
margins, dydx(exper educ) predict(yexpected)


* Панельные данные
import delimited /Users/polinapogorelova/Downloads/cornwell&rupert.csv, clear // импорт .csv

sum lwage exp smsa fem union ind if year == 1
sum lwage exp smsa fem union ind if year == 7

egen mlwage = mean(lwage), by(year)
egen mexp = mean(exp), by(year)

graph twoway connected mlwage year, sort
graph twoway connected mexp year, sort

* OLS estimation: Pooled Regression
reg lwage exp expsq smsa south wks union ms fem ed blk ind occ

* LSDV - проверим наличие индивидуального эффекта для каждого объекта выборки
reg lwage exp expsq smsa south wks occ union ms fem ed blk ind i.id
* Тот же самый результат можно получить следующим образом
areg lwage exp expsq smsa south wks occ union ms fem ed blk ind, absorb(id)
estimates store a_lsdv



* Далее перейдем к рассмотрению моделей с фиксированными и случайными эффектами
xtset id year // зададим пространственную и временную компоненту

* FE Модель с фиксированными эффектами
* Оценивание FE-модели+тест на спецификацию: H0: pooled-модели H1: FE-модель
xtreg lwage exp expsq smsa south wks occ union ms fem ed blk ind, fe

* Проверим гипотезу о гетероскедастичности данных
* Тест Вальда для проверки групповой гетероскедастичности. H0: гетероскедастичность отсутствует
ssc install xttest3
xttest3

* Оценим FE-модель в предположении о наличии гетероскедастичности в данных
xtreg lwage exp expsq smsa south wks occ union ms fem ed blk ind, fe vce(robust)

* Cохраним результаты оценивания FE-модели
est store fe
// Оценки within, fe и LSDV эквивалентны

* Оценки со случайным эффектом (b_RE=b_GLS)
xtreg lwage exp expsq smsa south wks occ union ms fem ed blk ind, re
est store re

* Between-оценки модели со случайными эффектами
xtreg lwage exp expsq smsa south wks occ union ms fem ed blk ind, be

* RE-оценка представляет собой средневзвешенное внутри- и межугрупповой оценок

* Тест Вальда на спецификацию. H0: pooled-модель H1: RE-модель
xttest0

* Тест Хаусмана на спецификацию. H0: RE-модель. H1: FE-модель
hausman fe re

* Выведем оценки, полученные по трем моделям в виде единой таблицы
estimates table ols  fe re, star stats(N r2 r2_a)

