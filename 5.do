**# 第5章 线性工具变量回归
cd "D:\傻瓜计量经济学2"

**## 5.1内生性与工具变量
use mus06data.dta, clear
sum ldrugexp hi_empunion female marry linc totchr black age educyr
des ldrugexp hi_empunion female marry linc totchr black age educyr
reg ldrugexp hi_empunion female marry linc totchr black age educyr

reg hi_empunion firmsz female marry linc totchr black age educyr
predict hi_empunion_hat
reg ldrugexp hi_empunion_hat female marry linc totchr black age educyr

ivregress 2sls ldrugexp (hi_empunion=firmsz) female marry linc totchr black age educyr


reg ldrugexp firmsz female marry linc totchr black age educyr
reg hi_empunion firmsz female marry linc totchr black age educyr


**## 5.2工具变量检验
use mus06data.dta, clear
ivregress 2sls ldrugexp (hi_empunion= multlc) age educyr female marry poverty totchr linc blhisp
estat endogenous


ivregress 2sls ldrugexp (hi_empunion= firmsz) age educyr female marry poverty totchr linc blhisp
outreg2 using xmm, word replace
ivregress 2sls ldrugexp (hi_empunion= ssiratio) age educyr female marry poverty totchr linc blhisp
outreg2 using xmm, word
ivregress 2sls ldrugexp (hi_empunion= multlc) age educyr female marry poverty totchr linc blhisp
outreg2 using xmm, word


corr  hi_empunion firmsz ssiratio multlc
quietly ivregress 2sls ldrugexp (hi_empunion= firmsz) age educyr female marry poverty totchr linc blhisp, r
estat firststage, forcenonrobust all
estat weakrobust 
weakivtest

quietly ivregress 2sls ldrugexp (hi_empunion= ssiratio) age educyr female marry poverty totchr linc blhisp, r
estat firststage, forcenonrobust all

ivregress 2sls ldrugexp (hi_empunion= firmsz multlc ssiratio) age educyr female marry poverty totchr linc blhisp, r
estat firststage, forcenonrobust all
estat overid


lasso linear ldrugexp hi_empunion age educyr female marry poverty totchr linc blhisp
lassocoef, display(coef, postselection)
lassograph coefpath
lassograph cvplot
coefpath, lineopts(lwidth(thick)) legend(on)  
help coefpath
coefpath, lineopts(lwidth(thin)) legend(on) xunits(lnlambda)  

help pdslasso


 
use AJR.dta,clear
reg logpgp95 avexpr lat_abst edes1975 avelf temp* humid* steplow-oilres
ivregress 2sls logpgp95 lat_abst edes1975 avelf temp* humid* steplow-oilres (avexpr =logem4 euro1900-cons00a), first
pdslasso logpgp95 avexpr (lat_abst edes1975 avelf temp* humid* steplow-oilres), partial(lat_abst)
ivlasso logpgp95 (lat_abst edes1975 avelf temp* humid* steplow-oilres) (avexpr=logem4 euro1900-cons00a), partial(logem4 lat_abst) idstats first


use AK91.dta,clear
xtset pob
ivlasso lnwage (i.yob) (edu=i.qob i.yob#i.qob i.pob#i.qob), fe first
 

**## 5.3广义矩估计GMM
use mus06data.dta, clear
ivreg2 ldrugexp (hi_empunion= firmsz multlc ssiratio) age educyr female marry poverty totchr linc blhisp, gmm2s r
 
qui ivregress gmm ldrugexp (hi_empunion= firmsz multlc ssiratio) age educyr female marry poverty totchr linc blhisp, r 
outreg2 using xmm, word replace
qui ivreg2 ldrugexp (hi_empunion= firmsz multlc ssiratio) age educyr female marry poverty totchr linc blhisp, gmm2s r
outreg2 using xmm, word
 
 
ivreg2 ldrugexp (hi_empunion= firmsz multlc ssiratio) age educyr female marry poverty totchr linc blhisp, gmm2s cluster(age)
 
 

use "D:\傻瓜计量经济学2\abdata.dta",clear
xtset id year 
sum id year ind emp wage cap indoutpt n w k ys yr1980-yr1984
ivreg2 n (w k ys = l(1/5).(w k ys)), gmm2s robust bw(3)
ivreg2 n (w k ys = d.w d.k d.ys d2.w d2.k d2.ys), gmm2s cluster(id)
 
ivreg2 n (w k ys = l(1/5).(w k ys)), bw(3) gmm2s kernel(qs) orthog(l(1/3).w) robust
 
**## 5.4似不相关估计
use mus05surdata.dta, clear
summarize ldrugexp ltotothr age age2 educyr actlim totchr medicaid private 
sureg (ldrugexp age age2 actlim totchr medicaid private) (ltotothr age age2 educyr actlim totchr private), corr
 
qui reg ldrugexp age age2 actlim totchr medicaid private
outreg2 using xmm, word replace
qui reg ltotothr age age2 actlim totchr medicaid private
outreg2 using xmm, word
sureg (ldrugexp age age2 actlim totchr medicaid private) (ltotothr age age2 actlim totchr medicaid private)
outreg2 using xmm, word
 
 
 
use mus06data.dta, clear 
sureg (ldrugexp firmsz female marry linc totchr black age educyr) (hi_empunion firmsz female marry linc totchr black age educyr), corr  isure
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
