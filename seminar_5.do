* Семинар 5. Logit, ordered logit model
clear
set more off
log using sem_5.log

* Logit model
use http://www.stata.com/data/jwooldridge/eacsap/mroz, clear
describe
summarize
tab inlf city

reg inlf educ exper expersq age kidslt6 kidsge6 mtr huswage // оценим линейную модель вероятности
predict prob_ols

logit inlf educ exper  age kidslt6 kidsge6 // оценивание logit-модели с помощью ММП (логистические шансы). Коэффициент при переменной показывает, во сколько раз изменится логарифм отношения P("удачи")/P("неудачи")
logit, or // odds ratio (отношение шансов)
predict xb0, xb // прогнозирование латентной переменной
predict pl0, pr // прогнозирование вероятности "успеха"
sum pl0 // описательная статистика предсказанной вероятности

logit inlf educ exper expersq age kidslt6 kidsge6 mtr huswage, vce(robust) // оценивание logit-модели с помощью ММП (логистические шансы)
logit, or // odds ratio (exp(b))
* или альтернативная команда
logistic inlf educ exper expersq age kidslt6 kidsge6 mtr huswage // odds ratio

predict xb1, xb // прогнозирование латентной переменной
predict pl1, pr // прогнозирование вероятности "успеха"
sum pl1 // описательная статистика предсказанной вероятности

test kidslt6 = kidsge6 = 0 // тестирование гипотезы о незначимости переменных, характеризующих состав детей в семье

margins, dydx(*) // усредненные предельные эффекты 
margins, dydx(*) atmeans // предельные эффекты (marginal effects) для "среднего" наблюдения. ПЭ для переменной xj показывает, на сколько изменится вероятность "успеха" при увеличении xj на 1 единицу
margins, at(educ = (10(2)20)) // средняя вероятность "успеха" на каждом уровне educ от 10 до 20 лет с шагом 2 года
margins, at(educ = (10(2)20)) vsquish // средняя вероятность "успеха" на каждом уровне educ от 10 до 20 лет с шагом 2 года
margins, at(educ = (10(2)20)) atmeans // вероятность "успеха" на каждом уровне educ от 10 до 20 лет с шагом 2 года при средних значениях остальных переменных

lsens, genprob(prob_cutoff) gensens(se) genspec(sp) title("График зависимости чувствительности и специфичности") // график зависимости Se и Sp от cut-off
lroc // построение ROC-кривой и расчет AUC
roccomp inlf xb0 xb1, graph summary // сравнение двух логит-модельc разным набором объясняющих переменных с помощью ROC-кривой
estat clas, cutoff(0.55) // таблица классификации (сопряженности y^ и y)
estat gof // H0: модель адекватна (Хи2-критерий Пирсона)
net search fitstat // загрузка fitstat
fitstat // различные показатели качества моделей из разных классов, в том числе и бинарных

scalar cut_off = 0.55
gen inlf_f = 0
replace inlf_f = 1 if pl1 > cut_off // создание зависимой переменной, полученной в результате оценивания модели и соответствующей оптимальному параметру cut-off

* Пробит-модель
probit inlf educ exper expersq age kidslt6 kidsge6 mtr huswage
probit, or
predict xb2, xb
predict probit_pr, pr

margins, dydx(*) atmeans
margins, dydx(*)

lsens, title("График зависимости чувствительности и специфичности") // график зависимости Se и Sp от cut-off
lroc // ROC-curve
estat clas, cutoff(0.55) // таблица классификации (сопряженности)
estat gof // H0: модель адекватна (Хи2-критерий Пирсона)
fitstat


* Ordered logit regression
use https://stats.idre.ucla.edu/stat/data/ologit.dta, clear
tab apply
tab apply, nolabel
tab apply public
sum gpa

ologit apply i.pared i.public gpa // оценим порядковый логит. Коэффициенты показывают во сколько раз увеличится логарифм отношения шансов при изменении объясняющей переменной на 1 единицу
brant, detail
ologit apply i.pared i.public gpa, or // получим оценки изменения отношения шансов (odds ratio) перейти с одного уровня на более высокий
listcoef, help


margins, at(pared = (0/1)) predict(outcome(0)) atmeans // прогнозируемая вероятность попасть в низшую категорию в зависимости от значения категориальной переменной "pared"
margins, at(pared = (0/1)) predict(outcome(1)) atmeans // прогнозируемая вероятность попасть в среднюю категорию в зависимости от значения категориальной переменной "pared"
margins, at(pared = (0/1)) predict(outcome(2)) atmeans // прогнозируемая вероятность попасть в высшую категорию в зависимости от значения категориальной переменной "pared"

forvalues i = 0/2 {
  margins, at(gpa = 3.5 pared = 1 public = 1) predict(outcome(`i'))
} // прогноз вероятности попасть в каждую категорию для индивидуума, имеющего GPA равный 3.5, обучавшегося в частной школе и у которого хотя бы один из родителей обучался в аспирантуре

net search omodel
omodel logit apply pared public gpa // альтернативная команда для оценивания упорядоченных моделей логит и пробит, содержащая также реультаты теста на пропорциональность шансов (test for the equivalence of the estimates for cut levels)
predict prob_unlikely prob_somewhatlikely prob_verylikely, pr // прогнозирование вероятностей попасть в каждую категорию

log close
