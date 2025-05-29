**# 第6章 分位数回归
cd "D:\傻瓜计量经济学2"

**## 6.2条件分位数回归
use "D:\傻瓜计量经济学2\mus03data.dta",clear
asdoc sum, replace
qplot ltotexp, recast(line) scale(1.4) yline(6.365 9.773, lwidth(vthin) lp(dash)) xline(0.1 0.9,lwidth(vthin) lp(dash))
des
qreg ltotexp totchr suppins white female age
dis 1-(1398.492/1555.48)

qreg2 ltotexp totchr suppins white female age

quietly reg ltotexp totchr suppins white female age
est sto ols //将估计结果储存为ols
quietly qreg ltotexp totchr suppins white female age, q(.1)
est sto q10
quietly qreg ltotexp totchr suppins white female age, q(.5) 
est sto q50
quietly qreg ltotexp totchr suppins white female age, q(.9) 
est sto q90
esttab ols q10 q50 q90 using xmm.rtf, replace ///
b(%6.3f) se(%6.3f) ar2(3) ///
mtitles(ols q10 q50 q90) ///
star(* 0.1 ** 0.05 *** 0.01) ///
compress nogap  


quietly reg ltotexp totchr suppins white female age 
outreg2 using jxufe, word replace
quietly qreg ltotexp totchr suppins white female age, q(.1)
outreg2 using jxufe, word
quietly qreg ltotexp totchr suppins white female age, q(.5) 
outreg2 using jxufe, word
quietly qreg ltotexp totchr suppins white female age, q(.9) 
outreg2 using jxufe, word


sqreg ltotexp totchr suppins white female age, q(.1 .5 .9) reps(400)
test [q10=q50=q90]: totchr
test [q10=q50=q90]: suppins

iqreg ltotexp totchr suppins white female age,reps(400)


set seed 1000
bsqreg ltotexp totchr suppins white female age, q(.5) reps(400)
grqreg, cons ci ols olsci seed(10101)
help grqreg


clear
set obs 150
set seed 10000
gen x2=rchi2(1)
gen x3=5*rnormal()
gen e=5*rnormal()
gen u=(0.1+0.5*x2)*e
gen y=1+x2+x3+u
sum  
qplot y, recast(line) scale(1.4) yline(-7.59 11.43, lwidth(vthin) lp(dash)) xline(0.1 0.9,lwidth(vthin) lp(dash))
reg y x2 x3, r
qreg y x2 x3
sqreg y x2 x3, q(.25 .5 .75) reps(400)


help ivqreg2

use mus03data.dta, clear
ivqreg2 ltotexp suppins totchr white female age, quantile(.25 .75) instruments(educyr totchr white female age)

help ivqte

ivqte ltotexp totchr white educyr age (suppins=female), quantile(0.5)   



**# 6.3面板条件分位数回归

help qregpd

*ssc install amcmc, replace
clear
use "D:\傻瓜计量经济学2\nlswork.dta"
xtset idcode year 
sum
qregpd ln_wage tenure union, id(idcode) fix(year) q(.5)
set seed 10101
 
qregpd ln_wage tenure union, q(.5) id(idcode) fix(year) optimize(mcmc) noisy draws(5000) burn(2000) arate(.234)
qregpd ln_wage tenure union , q(.5) id(idcode) fix(year) optimize(grid) grid1(0(0.002)0.06) grid2(0.05(0.01)0.1)  


set seed 10101
qregpd ln_wage tenure union, id(idcode) fix(year) optimize(mcmc) noisy draws(1000) burn(100) arate(.44) instruments(ttl_exp wks_work union)

qregpd ln_wage tenure union, id(idcode) fix(year)  optimize(grid) instruments(ttl_exp wks_work union)

















































































