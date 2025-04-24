* Семинар 13. SFA (Stochastic frontier analysis)
clear
set more off
cd /Users/polinapogorelova/Desktop/СМЭФ_Эконометрика
log using sem_13.log

use https://www.stata-press.com/data/r16/xtfrontier1, clear

* Датасет xtfrontier1 содержит данные о 91 фирме (от 6 до 14 наблюдений для каждой фирмы) о количестве
* произведенной продукции (widgets), о количестве затраченных машино-часов (machines) и о количестве
* затраченных человеко-часов (workers).

* данные представляют собой панель, поэтому нужно задать пространственную и временную компоненты
xtset id t

* q_it = f(x_it) - U_it, где q_it - выпуск, x_it - ресурсы, функция f описывает границу производственных возможностей, U_it -  разница между выпуском при наиболее эффективном использовании заданного набора ресурсов и фактическим выпуском 

* Базовая модель: q_i = f(x_i,b)*k_i*exp(v_i) или ln(q_i) = b0 + b1*ln(x_i1) + ... + bm*ln(x_im) - u_i + v_i,  где u_i = -ln(k_i),
* где  k_i - степень эффективности фирмы; x_i - факторы, влияющие на выпуск; u_i ~ N+(0,sigma2_u) - технологическая неэффективность, v_i ~ N(0, sigma2_v) - ошибки модели.

* Для начала оценим обычные модели панельных данных
xtreg lnwidgets lnmachines lnworkers, fe
est store res_fe
predict resfe, r // остатки модели
hist resfe // на гистограмме заметна левосторонняя асимметрия

xtreg lnwidgets lnmachines lnworkers, re
est store res_re
predict resre, ue // остатки модели
hist resre // на гистограмме заметна левосторонняя асимметрия


* Теперь перейдем к моделям SFA

* 1. Модель без гетерогенности и тренда (Time-invariant model)
* ln(q_it) = b0 + b1*ln(x_1it) + ... + bm*ln(x_mit) + v_it - u_i,
* 													где u_i ~ N+(mu, sigma2_u), v_it ~ N(0, sigma^2_v)
xtfrontier lnwidgets lnmachines lnworkers, ti
est store ti_sfa
predict uhat_ti, u // оценка -ln(TE) с помощью E(u_it|ε_it)
hist uhat_ti
predict te_ti, te // техническая эффективность  = E{exp(−u_i|ε_it)
hist te_ti

* 2. Модель без гетерогенности и с трендом
* ln(q_it) = b0 + b1*ln(x_1it) + ... + bm*ln(x_mit) + v_it- u_it,

* Battese, Coelli, 1992 (Time-varying decay model)
* u_it = exp(-η*(t-T_i))*u_i, где T_i - последний период для i-й панели, η - параметр затухания, u_i ~ N+(mu,sigma2_u)
* При η > 0 степень неэффективности уменьшается с течением времени, а при η < 0 степень неэффективности увеличивается с течением времени. Поскольку t = Ti
* в последнем периоде, последний период для фирмы i содержит базовый уровень неэффективности для этой фирмы. Если η > 0, то уровень неэффективности снижается до базового уровня. Если η < 0, то уровень неэффективности возрастает до базового уровня. При η = 0 получаем TI-модель.
xtfrontier lnwidgets lnmachines lnworkers, tvd
est store tvd_sfa
predict uhat_tvd, u // оценка -ln(TE)
hist uhat_tvd
predict te_tvd, te // техническая эффективность = E{exp(−u_it|ε_it)
hist te_tvd

* Сравним полученные двумя моделями оценки, построив график
twoway scatter te_ti te_tvd

* Cornwell et al., 1990
* ln(q_it) = b_0t + b1*ln(x_1it) + ... + bm*ln(x_mit) - u_it + v_it = b_it + b1*ln(x_1it) + ... + bm*ln(x_mit) + v_it , 
* b_it = c_t1 + c_t2 * t + c_t3*t^2 (квадратичный тренд)
sfpanel lnwidgets lnmachines lnworkers, model(fecss)
est store fecss_sf
predict uhat_fecss, u // техническая неэффективность
hist uhat_fecss
predict te_fecss1, jlms // техническая эффективность JLMS-оценка
hist te_fecss1
predict te_fecss2, bc // недоступно для FECSS-моделей


* 3. Модели «True» fixed-effect (Greene, 2005)
* ln(q_it) = b0_i + b1*ln(x_1it) + ... + bm*ln(x_mit) - u_it + v_it, b0_i - firm-specific коэффициент
sfpanel lnwidgets lnmachines lnworkers, model(tfe) distribution(exponential)
sfpanel lnwidgets lnmachines lnworkers, model(tfe) distribution(exponential) usigma(lnworkers) // учет гетерогенности
est store tfe_sf
predict uhat_tfe, u // техническая неэффективность = E(u|ε)
hist uhat_tfe
predict te_tfe1, jlms //  техническая эффективность = exp{−E(u|ε)} (Jondrow et al., 1982)
hist te_tfe1
predict te_tfe2, bc // техническая эффективность = E{exp(−u|ε)} (Battese, Coelli, 1988)
hist te_tfe2
twoway scatter te_tfe1 te_tfe2 // сравним с помощью диаграммы рассеяния JLMS- и BC-оценки технической эффективности

* Модели «True» random-effect (Greene, 2005)
* Шаг 1. ln(q_it) = b0 + b1*ln(x_1it) + ... + bm*ln(x_mit) + n_i + v_it
sfpanel lnwidgets lnmachines lnworkers, model(tre) distribution(hnormal) // здесь thetta - стандартное отклонение случайного эффекта
est store tre_sf
predict uhat_tre, u // техническая неэффективность
hist uhat_tre
predict te_tre1, jlms // техническая эффективность JLMS-оценка
hist te_tre1
predict te_tre2, bc // техническая эффективность BC-оценка
hist te_tre2
twoway scatter te_tre1 te_tre2 // сравним с помощью диаграммы рассеяния JLMS- и BC-оценки технической эффективности
