clear
set more off
cd /Users/polinapogorelova/Desktop/
log using sem_7.log

* Multinomial logistic regression

use http://www.stata.com/data/jwooldridge/eacsap/keane.dta, clear
keep if year == 87
sum
drop if missing(status)
label define status 1 "school" 2 "home" 3 "work"
label values status status
table status, con(mean educ sd educ)
tab status black, chi2 // Хи2-критерий для проверки независимости двух факторов. H0: переменные независимы

mlogit status educ exper expersq i.black, base(1)
predict p_1 p_2 p_3, pr // прогноз вероятности принадлежности индивидуума к каждой категории
mlogit, rrr // relative-risk ratios (показывает, чему равно отношение вероятностей принадлежности к разным группам при увеличении объясняющей переменной на 1 единицу)
test [home]educ = [work]educ


margins black, pr(out(1))
margins, dydx(black) atmeans predict(out(1)) // предельный эффект, посчитанный для "среднего" наблюдения

margins black, atmeans pr(out(1)) // прогноз вероятности выбрать школу (y=1) в зависимости от значения факторной переменной black при условии, что все остальные переменные выбраны на их среднем уровне
marginsplot, name(schhol)
graph export scool, as(png) width(600) height(450) replace
margins black, atmeans pr(out(2))
marginsplot, name(home)
margins black, atmeans pr(out(3))
marginsplot, name(work)
graph combine school home work, ycommon

margins black, pr(out(3)) // средняя вероятность того, что y=3 на каждом уровне факторной переменной black

margins, at(educ = (8(2)18)) predict(outcome(1)) vsquish // средняя вероятность того, что y=1 на каждом уровне количественной переменной educ (от 8 до 18 с шагом равным 2 годам)
margins, at(educ = (8(2)18)) predict(outcome(2)) vsquish
margins, at(educ = (8(2)18)) predict(outcome(3)) vsquish

fitstat // проверка качества модели

* Пуассоновская регрессия
use "/Users/polinapogorelova/Downloads/crime.dta", clear
hist narr86, discrete scheme(sr1mono) title("Гистограмма распределения числа преступлений")

reg narr86 pcnv avgsen tottime ptime86 qemp86 inc86 durat black hispan born60, vce(robust) // оценим линейную модель с помощью МНК
predict narr86_f_ols

poisson narr86 pcnv avgsen tottime ptime86 qemp86 inc86 durat black hispan born60, vce(robust) // оценим пуассоновскую регрессию. Для получения робастных стандартных ошибок для оценок параметров используем vce(robust). Коэффициент при xj показывает, насколько увеличится log(narr86) при увеличении xj на 1 единицу
poisson, irr //  incident rate ratios - показывает, во сколько раз увеличится число преступлений при изменении xj на 1 единиц

poisson narr86 pcnv avgsen tottime ptime86 qemp86 inc86 durat black hispan born60, vce(robust) 
test black = hispan

predict narr86_f_p, n // прогноз числа преступлений
estat gof // goodness-of-fit H0: модель адекватна
fitstat

poisson narr86 pcnv avgsen tottime ptime86 qemp86 inc86 durat i.black hispan born60, vce(robust) 
margins i.black, atmeans // вычислим прогнозируемое количество преступлений на каждом уровне переменной black, сохраняя все другие переменные в модели на их среднем уровне

separate narr86_f_p, by(black)
twoway scatter narr86_f_p0 narr86_f_p1 inc86, connect(l l) sort ///
       ytitle("Predicted Count of Crimes") ylabel( ,nogrid) legend(rows(3)) ///
       legend(ring(0) position(10)) scheme(sr1mono)
	   

* Negative binomial regression analysis
use "/Users/polinapogorelova/Downloads/crime.dta", clear
hist narr86, discrete scheme(sr1mono) title("Гистограмма распределения числа преступлений")
hist narr86, discrete scheme(sr1mono)
tabstat narr86, by(hispan) stats(mean v n)

nbreg narr86 pcnv avgsen tottime ptime86 qemp86 inc86 durat black hispan born60, dispersion(constant) nolog // NB-I регрессия
nbreg, irr // incidence-rate ratios = exp(b_k) показывает, во скольок раз изменится зависимая пермеенная (частота события) при увеличении объясняющей переменной на 1 единицу
predict narr86_f_nb1, n
predict narr_12, pr(1,2) // прогноз вероятности P(a <= y_j <= b), где a=1, b=2 для данного случая
summarize narr86 numnarr narr_12

nbreg narr86 pcnv avgsen tottime ptime86 qemp86 inc86 durat i.black i.hispan i.born60, dispersion(mean) nolog // NB-II регрессия
predict narr86_f_nb2, n
test black hispan

margins i.black, atmeans // прогнозируемое количество преступлений в зависимости от значения переменной black при среднем уровне всех остальных переменных
margins, at(avgsen = (5(1)10)) vsquish // среднее количество преступлений, рассчитанное для avgsen={5,6,7,8,9,10}
margins, at(avgsen = (5(1)10)) atmeans // количество преступлений, рассчитанное для avgsen={5,6,7,8,9,10} при использовании средних значений для остальных переменных
fitstat


separate narr86_f_nb1, by(black)
twoway scatter narr86_f_nb10 narr86_f_nb11 inc86, connect(l l) sort ///
       ytitle("Predicted Count of Crimes") ylabel( ,nogrid) legend(rows(3)) ///
       legend(ring(0) position(10)) scheme(sr1mono)

	   
* Tobit I. Цензурированная регрессия. Пример 1 (данные о дополнительных выплатах сотрудникам)
use http://fmwww.bc.edu/ec-p/data/wooldridge/fringe.dta, clear

sum annbens exper age educ tenure married male // описательные статистики

reg annbens exper expersq age annearn tenure // линейная модель регрессии, оцененная с помощью МНК
predict yhat_lm // прогноз доп. выплат согласно линейной модели регрессии

tobit annbens exper age annearn tenure, ll(0) // тобит-модель для данных, цензурированных слева нулем
predict yhat_tobit // прогноз доп. выплат согласно tobit-модели

margins, dydx(*) predict(ystar(0,.)) // предельный эффект для усеченной переменной
margins, dydx(*) predict(pr(0,.)) // вычисляет предельный эффект на Pr(a < yj < b) (вероятность того, что латентная переменная будет наблюдаемоа в интервале (a,b))
