**# 第7章 面板数据模型回归
cd "D:\傻瓜计量经济学2"


**## 7.1认识面板数据
use mus08psidextract.dta, clear
xtset id t  
xtsum id t exp wks occ


**## 7.2静态面板数据模型
use mus08psidextract.dta, clear
asdoc sum, replace
xtset id t
xtdes
xtsum
xttab fem

reg lwage exp wks occ ind south smsa ms union i.id
outreg2 using mmxi, word replace
xtreg lwage exp wks occ ind south smsa ms union, fe
outreg2 using mmxi, word

xtreg lwage exp wks occ ind south smsa ms union i.t, fe robust
xtreg lwage exp wks occ ind south smsa ms union tdum2-tdum7, fe
test tdum2 tdum3 tdum4 tdum5 tdum6 tdum7

xtreg lwage exp wks occ ind south smsa ms union i.t, fe
outreg2 using mmxi, word replace
xtreg lwage exp wks occ ind south smsa ms union tdum2-tdum7, fe
outreg2 using mmxi, word
xtreg lwage exp wks occ ind south smsa ms union i.t, fe cluster(id)
outreg2 using mmxi, word
xtreg lwage exp wks occ ind south smsa ms union tdum2-tdum7, fe cluster(id)
outreg2 using mmxi, word

xtreg lwage exp wks occ ind south smsa ms union fem ed blk, re
outreg2 using mmxi, word replace
xtreg lwage exp wks occ ind south smsa ms union fem ed blk, mle 
outreg2 using mmxi, word
xtreg lwage exp wks occ ind south smsa ms union, re
outreg2 using mmxi, word
xtreg lwage exp wks occ ind south smsa ms union, mle
outreg2 using mmxi, word

xtreg lwage exp wks occ ind south smsa ms union fem ed blk, be

reg lwage exp wks occ ind south smsa ms union fem ed blk, vce(cluster id)

xtreg lwage exp wks occ ind south smsa ms union, fe
est store fe
xtreg lwage exp wks occ ind south smsa ms union, re 
est store re
hausman fe re


xtreg lwage exp wks occ ind south smsa ms union fem ed blk, fe
est store fe
xtreg lwage exp wks occ ind south smsa ms union fem ed blk, re
est store re
xtreg lwage exp wks occ ind south smsa ms union fem ed blk, cre
est store cre
esttab fe re cre,se


use "D:\傻瓜计量经济学2\invest2.dta", clear
xtset company time
xtrc invest market stock
xtrc invest market stock, beta

reg invest stock market i.company#c.market i.company, vce(cluster company)

**## 7.3面板工具变量法
use "D:\傻瓜计量经济学2\nlswork.dta", clear
sum
xtset idcode year
xtreg ln_wage tenure age c.age#c.age not_smsa, fe

xtivreg ln_w age c.age#c.age not_smsa (tenure = union south), fe vce(cluster idcode)
xtivreg ln_w age c.age#c.age not_smsa (tenure = union south), fe vce(cluster idcode) first

xtivreg ln_w age c.age#c.age not_smsa (tenure = union south) i.year, fe vce(cluster idcode)

xtivreg ln_w age c.age#c.age not_smsa (tenure = union south), fe vce(bootstrap, reps(500))

xtivreg ln_w age c.age#c.age not_smsa 2.race (tenure = union birth_yr south), re vce(cluster idcode)
xtivreg ln_w age not_smsa (tenure = union birth_yr south), fd


**## 7.4面板交互固定效应与多维固定效应模型
use "D:\傻瓜计量经济学2\nlswork.dta",clear
xtset idcode year
keep if id <=100 
sum ln_w tenure idcode year
codebook idcode year
regife ln_w tenure, ife(ifeid=idcode ifet=year, 1)
codebook ifeid1 ifet1

regife ln_w tenure, ife(id year, 1) vce(bootstrap)


regife ln_w tenure, ife (id year, 1) residuals(newvar)

regife ln_w tenure, a(fe_id = id fe_year = year) ife(ife_id = id ife_year = year, 1)

scatter fe_id id //Z个体固定效应随个体变化散点图
graph save fei, replace
scatter fe_year year //Z时间固定效应随时间变化散点图
graph save fet, replace
scatter ife_id1 id //Z个体交互固定效应随个体变化散点图
graph save ifei, replace
scatter ife_year1 year //时间交互固定效应随时间变化散点图
graph save ifet, replace
graph combine fei.gph fet.gph ifei.gph ifet.gph  


use "D:\傻瓜计量经济学2\nlswork.dta",clear

reghdfe ln_w tenure age ttl_exp not_smsa south, absorb(FE1=idcode FE2=year FE3=occ)
sum FE1 FE2 FE3
codebook FE1 FE2 FE3

reghdfe ln_w tenure age ttl_exp not_smsa south, absorb(idcode year occ i.occ#i.year)

ivreghdfe ln_w age ttl_exp  not_smsa south (tenure = wks_ue), absorb(idcode year occ)
xtivreg ln_w age ttl_exp not_smsa south (tenure = wks_ue) i.year i.occ, fe


**## 7.5动态面板估计
use "D:\傻瓜计量经济学2\abdata.dta",clear
xtset id year
xtabond n l(0/1).w l(0/2).(k ys) yr1980-yr1984, lags(2)
xtabond n l(0/1).w l(0/2).(k ys) yr1980-yr1984, lags(2) vce(robust)
xtabond n l(0/1).w l(0/2).(k ys) yr1980-yr1984, lags(2) twostep  
xtabond n l(0/1).w l(0/2).(k ys) yr1980-yr1984, lags(2) twostep vce(robust)
estat abond
estat sargan

xtdpdsys n l(0/1).w l(0/2).(k ys) yr1980-yr1984, lags(2)
xtdpdsys n l(0/1).w l(0/2).(k ys) yr1980-yr1984, lags(2) twostep vce(robust)

xtabond2 n L.n w L.w k yr*, gmm(L.(w n k)) iv(yr*) noleveleq robust small


use mus08psidextract.dta, clear
xtabond2 lwage l(1/2).lwage l(0/1).wks ms union occ south smsa ind, nolevel twostep robust gmm(lwage, lag(2 4)) gmm(wks ms union, lag(2 3)) iv(occ south smsa ind)
xtabond2 lwage l(1/2).lwage l(0/1).wks ms union occ south smsa ind, twostep robust gmm(lwage, lag(2 4)) gmm(wks ms union, lag(2 3)) iv(occ south smsa ind)



**## 7.6长面板数据模型
use mus08cigar.dta, clear
xtset state year
sum
xtserial lnc lnp lnpmin lny year, output
qui xtreg lnc lnp lnpmin lny year, fe
xttest3

quietly xtreg lnc lnp lnpmin lny year, fe
xttest2

quietly xtreg lnc lnp lnpmin lny year, re
xttest1 
xttest0
 
help xtscc 
xtreg lnc lnp lnpmin lny year, fe
xtscc lnc lnp lnpmin lny year, fe lag(4)

quietly reg lnc lnp lnpmin lny year
est store ols
quietly reg lnc lnp lnpmin lny year, robust cluster(state)
est store olsrobust
quietly newey lnc lnp lnpmin lny year, lag(4) force
est store newey
quietly xtscc lnc lnp lnpmin lny year, lag(4)
est store dris_kraay
est table *, b se t


help xtpcse
xtpcse lnc lnp lnpmin lny i.state year, correlation(psar 1) rhotype(tscorr)

help xtgls
xtgls lnc lnp lnpmin lny i.state year, corr(psar l) panels(correlated)



qui reg lnc lnp lnpmin lny i.state year, cluster(state)
est store ols
qui xtpcse lnc lnp lnpmin lny i.state year,corr(psar l)
est store xtpcse
qui xtgls lnc lnp lnpmin lny i.state year,corr(psar l) panels(correlated)
est store xtgls
qui xtscc lnc lnp lnpmin lny i.state year
est store xtscc
esttab ols xtpcse xtgls xtscc using xmm.rtf, replace ///
b(%6.3f) se(%6.3f) ar2(3)  ///精确小数点后3位
mtitle(ols xtpcse xtgls xtscc)  ///
star(* 0.1 ** 0.05 *** 0.01) compress nogap ///
keep(lnp lnpmin lny year)



help xtdcce2

help xtlsdvc
xtlsdvc lnc lnp lnpmin lny year, initial(ah) vcov(50) bias(3) first 
xtlsdvc lnc lnp lnpmin lny year, initial(ab) vcov(50) bias(3) first
xtlsdvc lnc lnp lnpmin lny year, initial(bb) vcov(50) bias(3) first


**## 7.8面板随机前沿模型
help xtfrontier
use "D:\傻瓜计量经济学2\xtfrontier1.dta",clear

xtfrontier lnwidgets lnmachines lnworkers, ti
predict efficiency1, te
list id t lnwidgets lnworkers lnmachines efficiency1 if id<=3 & t<=3

xtfrontier lnwidgets lnmachines lnworkers, tvd
predict efficiency2, te 
xtsum efficiency1 efficiency2

help sfpanel
sfpanel lnwidgets lnmachines lnworkers, m(tfe)  usigma(lnworkers) robust
predict jlms1,jlms 
list efficiency1 efficiency2 jlms1 if id==1

























































