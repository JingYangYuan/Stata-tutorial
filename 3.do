**# 第3章基本模型回归与诊断
cd "D:\傻瓜计量经济学2"
**## 3.1共线性问题
sysuse auto.dta,clear
sum
asdoc summarize
asdoc summarize, replace
asdoc summarize, save(jxufe.doc) replace

gen weight2=weight^2
asdoc corr price weight weight2 mpg headroom length trunk, replace
quietly regress price weight mpg headroom length trunk
outreg2 using jxufe, word replace
quietly regress price weight weight2 mpg headroom length trunk
outreg2 using jxufe, word

regress price weight weight2 mpg headroom length trunk
estat vif

egen weightstd = std(weight)
egen pricestd=std(price)
egen lengthstd=std(length)
gen weightstd2=weightstd^2

quietly regress price weight weight2 length mpg headroom trunk
outreg2 using jxufe, word replace
quietly regress price weight weight2 lengthstd mpg headroom trunk
outreg2 using jxufe, word
quietly regress pricestd weight weight2 length mpg headroom trunk
outreg2 using jxufe, word
quietly regress pricestd weightstd weightstd2 lengthstd mpg headroom trunk
outreg2 using jxufe, word

regress pricestd weightstd weightstd2 lengthstd mpg headroom trunk
estat vif


**## 3.2异方差问题
sysuse auto.dta, clear
quietly reg price length mpg trunk
rvfplot
estat imtest, white
estat hettest, iid

gen lprice=log(price)
gen llength=log(length)
gen lmpg=log(mpg)
gen ltrunk=log(trunk)
reg lprice llength lmpg ltrunk
estat hettest, iid
estat imtest, white
reg price length mpg trunk, vce(robust)

quietly reg price length mpg trunk
predict uhat, residual
gen uhat2=uhat^2
quietly reg length mpg trunk
predict rhat, residual
gen rhat2=rhat^2
egen ru2=sum(rhat2*uhat2)
display sqrt([ru2/(e(rss)^2)] *(74/70))


**## 3.3自相关问题
use icecream.dta,clear
tsset time
gen temp100=temp/100
graph twoway connect consumption temp100 time, msymbol(circle) msymbol(square) legend(pos(6) row(1))
reg consumption temp price income
predict e1, res
gen e2=L.e1
graph twoway (scatter e1 e2) (lfit e1 e2), legend(pos(6) row(1))

ac e1
pac e1

estat bgodfrey
wntestq e1
estat dwatson

newey consumption temp price income, lag(3)
newey consumption temp price income, lag(6)

reg consumption temp L.temp price income
estat bgodfrey
estat dwatson



**## 3.4可行的广义最小二乘估计FGLS)
sysuse auto.dta, clear
quietly regress price weight mpg headroom length trunk
estat hettest, iid
predict e1, res
gen e2=e1^2
quietly regress price weight mpg headroom length trunk [aw=l/e2]
estat hettest, iid

use icecream.dta, clear
prais consumption temp price income, corc
prais consumption temp price income, nolog















