* Семинар 12. Модель квантильной регрессии.
clear
set more off
cd /Users/polinapogorelova/Desktop/СМЭФ_Эконометрика
log using sem_12.log

* Квантильная регрессия
use https://stats.idre.ucla.edu/stat/stata/webbooks/reg/elemapi2
sum api00 acs_k3 acs_46 full enroll

reg api00 acs_k3 acs_46 full enroll
test acs_k3 acs_46

* Квантильная регрессия
qreg api00 acs_k3 acs_46 full enroll, quant(.05) 
qreg api00 acs_k3 acs_46 full enroll, quant(.25) 
qreg api00 acs_k3 acs_46 full enroll, quant(.50) 
qreg api00 acs_k3 acs_46 full enroll, quant(.75) 
qreg api00 acs_k3 acs_46 full enroll, quant(.95)

* Бутстрапирование стандартных ошибок оценок коэффициентов
set seed 1001

bsqreg api00 acs_k3 acs_46 full enroll, quant(.75) 
sqreg api00 acs_k3 acs_46 full enroll, q(.1 .9)
test [q10]enroll = [q90]enroll


* Добавим графики для оценок коэффициентов разных квантилей
ssc install grqreg, replace

qreg api00 acs_k3 acs_46 full enroll
grqreg, cons ci title(Fig.1a Fig.1b Fig.1c Fig.1d Fig.1e) 

qreg api00 acs_k3 acs_46 full enroll
grqreg, cons ci ols olsci title(Fig.1a Fig.1b Fig.1c Fig.1d Fig.1e) qstep (0.2)

bsqreg api00 acs_k3 acs_46 full enroll
grqreg, cons ci ols olsci title(Fig.1a Fig.1b Fig.1c Fig.1d Fig.1e) qstep (0.1)

log close
