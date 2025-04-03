clear
set more off
cd /Users/polinapogorelova/Desktop/СМЭФ_Эконометрика/Эконометрика_24_25
log using sem_10.log

* Динамические модели панельных данных
use dahlberg.dta, clear

xtset id year

* Arellano&Bond estimator
xtabond expend l.revenue l.grants, lags(1)
xtabond expend l.revenue l.grants year, lags(1) twostep
estat abond

* Модели бинарного выбора с панельными данными
* Рассмотрим данные о государственном медицинском страховании
use "/Users/polinapogorelova/Downloads/healthcare.dta", clear

* Создадим дамми-переменную doctor = 1, если за год индивид посетил хотя бы один раз врача
gen doctor = 0 
replace doctor = 1 if docvis > 0

* Logit models
* 1. Pooled logit model
logit doctor age educ female hhinc handdum hadper newhsat public working, or
margins, dydx(*) // средние ПЭ показывают на сколько изменится вероятность того, что Y=1, если объясняющая переменная увеличится на 1 единицу

* Зададим пространственную и временную компоненту
xtset id year
* 2. Conditional fixed-effects logit models (для probit-модели не существует статистика)
xtlogit doctor age educ female hhinc handdum hadper newhsat public working, fe or // OR показывают как изменится отношение шанса doctor = 1 при увеличении объясняющей переменной на 1 единицу
est store FE_logit

* Предельные эффекты
margins, dydx(*) atmeans predict(pu0)


* 3. Random-effects logit models
xtlogit doctor age educ female hhinc handdum hadper newhsat public working, re intpoints(20)
* Cluster correction
xtlogit doctor age educ female hhinc handdum hadper newhsat public working, re intpoints(20) vce(cluster id)
quadchk, nooutput
est store RE_logit

margins, dydx(*) atmeans
margins, dydx(*) atmeans predict(pu0)

* Тест Хаусмана
hausman FE_logit RE_logit


* Probit models
* 1. Pooled probit models
probit public age educ hhinc hadper i.working hsat
margins, dydx(*)
margins, dydx(*) atmeans

* 2. Random-effects probit models
* Оценим пробит-модель для панельных данных, используя квадратуру Гаусса-Эрмита для вычисления функции правдоподобия
xtprobit public age educ hhinc hadper i.working newhsat

* Проверим чувствительность аппроксимации (для RE-моделей)
quadchk, nooutput 
xtprobit public age educ hhinc hadper i.working newhsat, intpoints(150) 
quadchk, nooutput

* Рассчитаем предельные эффекты
margins, dydx(*) atmeans
margins, dydx(hhinc) at(working = 0) predict(pu0)
margins, dydx(hhinc) at(age = (25 50 75 90) educ = 5 working = 0 newhsat = 5 hadper = 0) predict(pr)
