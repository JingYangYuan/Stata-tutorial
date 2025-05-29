**#  第2章 数据处理与图形绘制
cd "D:\傻瓜计量经济学2"
**## 2.1 数据导入
import excel "D:\傻瓜计量经济学2\2019年中国县域统计年鉴.xlsx", sheet("Sheet1") firstrow clear
summarize
encode 地区, generate(province)
encode 县区市, generate(county)
generate code=province
by province, sort: list province code, constant // constant 代表重复变量只出现一次
sum 地区 县区市 地区生产总值万元 第二产业从业人员人 province county if province==16
summarize if province==16, detail
help summarize
rename 地区生产总值万元 gdp
rename 第二产业从业人员人 employ
summarize gdp employ
regress gdp employ



**## 2.3数据合并
cd "D:\傻瓜计量经济学2"
use autosize.dta, clear
merge 1:1 make using autoexpense.dta
drop _merge


use autosize.dta, clear
merge 1:1 id using autoexpense.dta
merge 1:1 make id using autoexpense.dta, keep(match) nogen
list

use autoexpense2.dta, clear
merge m:1 make using autosize.dta

use autosize.dta, clear 
merge 1:m make using autoexpense2.dta
sort id year
drop _merge
list id make year weight length price mpg, sepby(make)


**## 2.4数据转换
use reshape1, clear
list
reshape long inc ue, i(id) j(year)
list, sepby(id)
reshape wide inc ue, i(id) j(year)
use reshape3, clear
list
reshape long inc ue, i(id) j(year)
list, sepby(id)

use reshape3, clear
preserve //暂存数据便于后面恢复到初始状态
rename inc80r inc80
rename inc81r inc81
rename inc82r inc82
reshape long inc ue, i(id) j(year)
list
restore

reshape long inc@r ue, i(id) j(year)
list

**## 2.5数据插值
clear all // 清除所有的数据
set obs 20 //设定观测值数量为20
set seed 10000 //设定种子为!0000,以保证实验结果的可复制性
gen id=_n //设定id变量令它等于_n, _n是系统自动生成的序号变量从1开始
gen year = _n+2000  //生成时间变量year,从2001-2020年
gen x = runiform(5, 6) //解释变量x服从（5, 6）上的随机均匀分布
gen e = rnormal() 
gen y=1+3*x+e 
tsset year
asdoc list, replace //误差项e服从标准的随机正态分布
 
gen y1=y
replace y1=. if y>18  
replace y1=. if year==2001
replace y1=. if year==2020
sum  

ipolate y1 x, gen(y2) //线性内推如果把x换成t就是平均值插值
ipolate y1 x, gen(y3) epolate //线性外推
list //(见表2.23

quietly reg y x
outreg2 using xmm, word replace
quietly reg y1 x
outreg2 using xmm, word
quietly reg y2 x
outreg2 using xmm, word
quietly reg y3 x
outreg2 using xmm, word

eststo clear
eststo: quietly reg y x
eststo: quietly reg y1 x
eststo: quietly reg y2 x
eststo: quietly reg y3 x
esttab using xmm.rtf, replace se ar2 nogap



clear all
local N = 6 //设定研究对象的个体数local是指设定局部宏变量
local T = 5 //设定研究的年份
local NT =  `N'*`T'
set obs `NT'
set seed 10000 //种子值方便实验的可重复性
egen id =seq(), from(1) to(`N') block(`T') //使用命令egen调用等差数列函数seq(),即变量id从1到N=6,但每个id重复T=5次总共6 x 5=30个观测值
sort id
by id: gen year = _n + 2010 //生成时间变量year,从2011-2015 年
by id: gen x = runiform(5, 6) //解释变量x服从(5, 6)上的随机均匀分布
by id: gen e = rnormal()  //Z误差项e服从标准的随机正态分布
by id: gen y=1+3*x+e  //因变量y是x的函数
xtset id year
sum  //（见表2.25） 
drop if year==2013
replace y=. if y>19
list if _n<=15, sepby(id)

tsfill, full
list

ipolate y x, gen(y1) // 线性内推插值
ipolate y x, gen(y2) epolate //线性外推插值
ipolate y year, gen(y3) //线性平均值插值对y插值
ipolate x year, gen(x1) //线性平均值插值对x插值
list, table //见表2.28
by id: ipolate y year, gen(y4)  epolate //线性外推法

quietly xtreg y x, fe r
outreg2 using xmm, word replace
quietly xtreg y1 x1,fe r
outreg2 using xmm, word 
quietly xtreg y2 x1, fe r 
outreg2 using xmm, word 
quietly xtreg y3 x1, fe r 
outreg2 using xmm, word
quietly xtreg y4 x1, fe r
outreg2 using xmm, word


tssmooth ma var1 = x, w(2,0,2)
replace x=var1 if x==.
tssmooth ma var2 = y, w(2,0,2)
replace y=var2 if y==.


**## 2.6图形绘制
use "D:\傻瓜计量经济学2\2019countycross.dta", clear
graph twoway (scatter gdp employ) (lfit gdp employ), legend(pos(6) row(1))

binscatter gdp employ, msymbols(Oh)
binscatter gdp employ, nquantiles(30)  
binscatter gdp employ, line(qfit) 
binscatter gdp employ, rd(50000)
binscatter gdp employ, rd(50000 120000)
sysuse auto.dta, clear  
binscatter price length, by(foreign) msymbols(o T)
binscatter price length, controls(rep78 headroom)


binsreg gdp employ
sysuse auto.dta, clear  
binsreg price length rep78 headroom ,ci(T) cb(T) nbins(13)

use "D:\傻瓜计量经济学2\2019countycross.dta", clear
regress gdp employ
set seed 10101
gen y=rnormal(2237134, 3026096)
kdensity gdp
graph save kgdp.gph, replace
kdensity y
graph save ngdp.gph, replace
graph combine kgdp.gph ngdp.gph

generate lgdp=log(gdp)
quietly kdensity gdp
graph save gdp.gph, replace  
quietly kdensity lgdp
graph save lgdp.gph, replace
graph combine gdp.gph lgdp.gph

gen lemploy=log(employ)
quietly reg gdp employ
outreg2 using jufe, word replace
quietly reg lgdp lemploy
outreg2 using jufe, word

twoway (scatter gdp employ, msize(*.25) mcolor(*.6)) (lfit gdp employ) (function y=1695.9528*x^0.632, range(employ) yvarlab("Exponential Function Fit")), legend(pos(6) row(1)) 




use "D:\傻瓜计量经济学2\2019countycross.dta", clear
gen lgdp=log(gdp)
gen lgdp2=log(gdp/7) //假设美元兑人民币是1 ： 7,用美元计价地区GDP
gen lgdp3=log(gdp*20) //假设日元兑人民币是20 ： 1,用日元计价地区GDP
gen lemploy=log(employ)
quietly reg lgdp lemploy, vce(robust)
outreg2 using jufe, word replace
quietly reg lgdp2 lemploy, vce(robust)
outreg2 using jufe, word
quietly reg lgdp3 lemploy, vce(robust)
outreg2 using jufe, word //(见表2.32)。


use laborsub.dta, clear
sum
gen lwhrs=log(whrs+1 )
gen lwhrs2=log(whrs/8+1 ) //按工作日算假设每天工作8小时
gen lwhrs3=log(whrs/40+1 ) //按工作周算假设每周工作40小时
quietly reg lwhrs kl6 k618 wa we, vce (robust)
outreg2 using jufe, word replace
quietly reg lwhrs2 kl6 k618 wa we, vce (robust)
outreg2 using jufe, word
quietly reg lwhrs3 kl6 k618 wa we, vce (robust)
outreg2 using jufe, word



**## 2.7主成分分析
use "D:\傻瓜计量经济学2\pca1.dta", clear 
sum
pca pop employment area gdp consumption fiscal research edu finance wage
estat kmo
estat smc
screeplot, yline(1)
predict f1 f2
scoreplot, mlabel(city) yline(0) xline(0)
loadingplot, yline(0)   xline(0) 
asdoc pca pop employment area gdp consumption fiscal research edu finance wage, replace


**清除Stata中的所有数据将以上输出结果word中的variable、comp 1和comp2三列数据复制粘贴到Stata数据编辑器中
gen comp1e=comp1/sqrt(7.153)  
gen comp2e=comp2/sqrt(1.109)  
gen score=[comp1e*0.7153+comp2e*0.1019]/0.8173 //构建综合得分指标
quietly sum score
gen weight=score/r(sum) 
list comp1e comp2e score weight



use "D:\傻瓜计量经济学2\pca1.dta", clear 
factor pop employment area gdp consumption fiscal research edu finance wage, pcf
factortest pop employment area gdp consumption fiscal research edu finance wage
asdoc predict factor1 factor2, replace

**清除Stata中的所有数据将以上输出结果word中的variable、comp 1和comp2三列数据复制粘贴到Stata数据编辑器中

gen score=[factor1*0.7153+factor2*0.1019]/0.8173 //构建综合得分指标
quietly sum score
gen weight=score/r(sum) //归ー化处理,将每项综合得分除以总得分
list factor1 factor2 score weight


**## 2.8嫡权法分析
use "D:\傻瓜计量经济学2\pca1.dta", clear 
 
global positiveVar pop employment area gdp consumption fiscal research edu finance wage
global negativeVar  
global allVar $positiveVar $negativeVar
foreach v in $positiveVar{
	qui sum `v'
	gen z_`v' = (`v'-r(min))/(r(max)-r(min))
	replace z_`v'= 0.0001 if z_`v'== 0	
}

foreach v in $negativeVar{
	qui sum `v'
	gen z_`v' = (r(max)-`v')/(r(max)-r(min))
	replace z_`v'= 0.0001 if z_`v'== 0	
}

foreach v in $allVar {
egen sum_`v'  = sum(z_`v' )  
gen p_`v' = z_`v'/ sum_`v'
}

gen N=_N
foreach v in $allVar {
egen sump_`v' = sum(p_`v'*ln(p_`v'))
gen e_`v'=-1/ln(N)*sump_`v'
}

foreach v in $allVar {
gen d_`v' = 1 - e_`v'
}

egen sumd = rowtotal(d_*)
foreach v in $allVar  {
gen w_`v' =  d_`v' / sumd
}

foreach v in $allVar  {
gen score_`v' = w_`v' * z_`v'
}
egen score = rowtotal(score*) //计算综合指标得分
drop z_* p_* e_* d_* sum*

**## 2.9效应量分析
use double11.dta, clear
sum join peer gender exper city major grade poor income university
reg join peer gender exper city major grade poor income, vce(cluster university)
coefplot, drop(_cons) xline(0) nolabels
coefplot, drop(_cons) yline(0) vertical
quietly reg join c.peer#i.income gender exper city major grade poor, vce(cluster university)
coefplot, drop(gender exper city major grade poor _cons) baselevels xline(0) xlabel(-0.2(0.1)0.5)
use "D:\傻瓜计量经济学2\effectsize1.dta", clear
metan effectsize lci uci, label(namevar=author) fixed effect(Marginal Effect) nobox npts(obs)


**## 2.10样本缩尾
use "D:\傻瓜计量经济学2\nlswork.dta", clear
reg ln_wage union age race msp grade south collgrad occ_code ttl_exp
sum ln_wage, detail

preserve //为了避免原始数据被修改暂存数据以备恢复
replace ln_wage=. if ln_wage<.4135621
replace ln_wage=. if ln_wage>2.956471
reg ln_wage union age race msp grade south collgrad occ_code ttl_exp
restore //恢复数据（见表2.51 ）

preserve //为了避免原始数据被修改暂存数据以备恢复
replace ln_wage=.4135621 if ln_wage<.4135621
replace ln_wage=2.956471 if ln_wage>2.956471
reg ln_wage union age race msp grade south collgrad occ_code ttl_exp
restore //恢复数据


winsor ln_wage, gen(ln_wage2) p(0.01)
reg ln_wage2 union age race msp grade south collgrad occ_code ttl_exp

help winsor2

 










