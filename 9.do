**# 第9章 匹配、双重差分与合成控制
cd "D:\傻瓜计量经济学2"


**## 9.1 因果识别中的匹配方法
use "D:\傻瓜计量经济学2\double11.dta"
set seed 10000
gen ranorder = runiform()
sort ranorder
psmatch2 peer gender grade exper income city, outcome(join) n(1) ate ties logit common

reg join peer

reg join peer [aweight=_weight]

set seed 10000
bootstrap r(att) r(atu) r(ate), reps(500): psmatch2 peer gender grade exper income city, outcome(join) n(1) ate ties logit common

quietly psmatch2 peer gender grade exper income city, outcome(join) n(1) ate ties logit common
pstest gender grade exper income city, both graph


psmatch2 peer gender grade exper income city, outcome(join) n(4) ate ties logit common

sum _pscore
display 0.25*0.0893578
psmatch2 peer gender grade exper income city, outcome(join) n(4) cal(0.02) ate ties logit common quietly
psmatch2 peer gender grade exper income city, outcome(join) n(4) cal(0.01) ate ties logit common quietly


psmatch2 peer gender grade exper income city, outcome(join) radius cal(0.01) ate ties logit common  
psmatch2 peer gender grade exper income city, outcome(join) kernel ate ties logit common
psmatch2 peer gender grade exper income city, outcome(join) llr ate ties logit common
psmatch2 peer, outcome(join) mahal(gender grade exper income city) n(4) ai(4) ate



nnmatch join peer gender grade exper income city, tc(att) m(4) robust(3)

nnmatch join peer gender grade exper income city, tc(att) m(4) robust(3) bias(bias)

nnmatch join peer gender grade exper income city, tc(att) m(4) robust(3) bias(bias) metric(maha) pop




use nsw_dw.dta, clear
sum re78 treat age education black nodegree re74
imb age education black nodegree re74, treatment(treat)
cem age(10 20 30 40 50) education black nodegree re74, treatment(treat)

reg re78 treat [aweight=cem_weights]


use "D:\傻瓜计量经济学2\mus08psidextract.dta", clear
xtset id t
sum exp-t
ebalance union exp exp2 wks occ ind south smsa ms, target(3)

reghdfe lwage union exp exp2 wks occ ind south smsa ms i.t [aweight=_webal], absorb(id) vce(cluster id)

forvalues i = 1/7{
ebalance union exp exp2 wks occ ind south smsa ms if t==`i', tar(3) keep(baltable_`i') g(_webal2_`i') replace
}
egen _webal2=rowtotal(_webal2_1 -_webal2_7)
reghdfe lwage union exp exp2 wks occ ind south smsa ms i.t [aweight=_webal2], absorb(id) vce(cluster id)

ebalance union exp exp2 wks occ, target(1 1 2 3) g(eweight)




**## 9.2 DID、PSM-DID和DDD
use hospdd.dta, clear
sum
ttable3 satis frequency, by(procedure) pvalue
xtset hospital
xtdidregress (satis frequency) (procedure), group(hospital) time(month) vce(robust)
estat trendplots,legend(pos(6))    
estat ptrends     
estat granger     
estat grangerplot 
estat bdecomp
xtreg satis frequency procedure i.month, fe vce(cluster hospital)


use hospdd.dta, clear
didregress (satis frequency) (procedure), group(hospital) time(month) vce(robust)

use  CardKrueger1994.dta, clear
sum
ttable3 fte bk kfc roys wendys, by(treated) pvalue
diff fte, t(treated) p(t)
diff fte, t(treated) p(t) cov(bk kfc roys) report bs
diff fte, t(treated) p(t) cov(bk kfc roys) kernel id(id) support
diff fte, t(treated) p(t) cov(bk kfc roys) id(id) logit kernel report support

xtset id
gen groups=0
replace groups=1 if wendys==1
replace groups=1 if kfc==1
xtreg fte c.treated#c.group#c.t c.treated#c.t c.group#c.t c.treated#c.group i.t i.group, fe r


**## 9.3 DID处理的典型案例分析
clear
set obs 1000
set seed 83816902
gen Treat=(uniform()<=0.6)  
bysort Treat: gen int id=uniform()*90+Treat*90+1  
bysort id: gen year=2016-_n+1 
gen Post=(year>=2012)   
gen y=ln(1+uniform()*100)
replace y=y+ln(1+uniform()*10+rnormal()*3) if Treat==1 & Post==1   
gen x1 =rnormal()* 3
gen x2=rnormal()+uniform()
xtset id year
sum
ttest x1, by(Treat)
ttest y, by(Treat)
gen Treat_Post=Treat*Post
xi: reg y Treat_Post x1 x2 i.year i.id, vce(cluster id) 
xtset id year
xtreg y Treat_Post x1 x2 i.year, fe vce(robust)
xtdidregress (y x1 x2) (Treat_Post), group(id) time(year) vce(robust)

gen Dyear=year-2012
gen Before2=(Dyear==-2 & Treat==1)
lab var Before2 "2 Year Prior"   
gen Before1 =(Dyear==-1 & Treat==1)
lab var Before1  "1 Year Prior"   
gen Current=(Dyear==0 & Treat==1)
lab var Current "Year of Adoption"
gen After1=(Dyear==1 &Treat==1)
lab var After1 "1 Year After"
gen After2=(Dyear==2 & Treat==1)
lab var After2 "2 Year After"
gen After3=(Dyear>=3 & Treat==1)
lab var After3 "3 or More Year After"

xtreg y Before2 Before1 Current After1 After2 After3 x1 x2 i.year, fe vce(robust)
coefplot, keep(Before2 Before1 Current After1 After2) vertical yline(0) addplot(line @b @at)

panelview y Treat_Post,i(id) t(year) type(treat) legend(pos(6))





use nlswork, clear
xtset idcode year
sum
gen age2= age^2
gen ttl_exp2= ttl_exp^2
gen tenure2= tenure^2
global xlist "grade age ttl_exp tenure not_smsa south race age2 ttl_exp2 tenure2"  
gen time=(year>=77)&!missing(year)  
gen treated= (idcode>2000)&!missing(idcode) 
gen did = time*treated
xtreg ln_wage did $xlist i.year, fe vce(cluster idcode)  
xtdidregress (ln_wage $xlist) (did), group(idcode) time(year) vce(robust)

gen Dyear=year-77
gen Before2=(Dyear==-2 & treated==1)
gen Before1=(Dyear==-1 & treated==1)
gen Current=(Dyear==0  & treated==1)
gen After1= (Dyear==1  & treated==1)
gen After2= (Dyear==2  & treated==1)
gen After3_=(Dyear>=3  & treated==1)
xtreg ln_wage Before2 Before1 Current  After1 After2 After3_ $xlist i.year, fe vce(robust)  
coefplot, keep(Before2 Before1 Current After1 After2 After3_) vertical yline(0) addplot(line @b @at) ciopts(recast(rcap)) scheme(slmono)




sort idcode
psmatch2 did $xlist, outcome(ln_wage) logit ate neighbor(1) common caliper(.05) ties
bootstrap r(att) r(atu) r(ate): psmatch2 did $xlist, out(ln_wage) logit ate neighbor(1) common caliper(.05) ties
gen common=_support
drop if common==0
xtreg ln_wage did $xlist i.year, fe vce(robust)






clear
set seed 10101
set obs 1000
gen id=_n  
expand 10  
bysort id: gen t=_n  
replace t=t+2010  
gen d=(t>=2016) 
label var d "=1 if post-treatment"
gen r=rnormal()
qui sum r, detail  
bysort id: gen i=(r>=r(p50)) if _n==1 
bysort id: replace i=i[_n-1] if i==. & _n!=1 
drop r
label var i "=1 if treated group, =0 if untreated group"
gen e = rnormal() 
label var e "normal random variable"
gen y = 1 + 0.5*i + 0.6*d + 0.8*i*d + e  
sum  
gen did=i*d
save mldid.dta, replace  
reghdfe y did, absorb(id t) vce(cluster id) 



mat b = J(1000,1,0)  
mat se = J(1000,1,0)  
mat p = J(1000,1,0)  
forvalues i=1/1000{
use mldid.dta, clear
xtset id t  
keep if t==2014  
sample 500, count 
keep id  
save match_id.dta, replace  
merge 1:m id using mldid.dta  
gen treat = (_merge == 3)  
gen period = (t>= 2016)  
gen did2 = treat*period
quietly reghdfe y did2, absorb(id t) vce(cluster id)  
mat b[`i',1] = _b[did2]  
mat se[`i',1] = _se[did2]  
mat p[`i',1] = 2*ttail(e(df_r), abs(_b[did2]/_se[did2]))  
}  
svmat b, names(coef1)  
svmat se, names(se1)  
svmat p, names(pvalue1)  
twoway (kdensity coef1) (scatter pvalue1 coef1, msymbol(smcircle_hollow) mcolor(blue)), ///
title("Placebo Test1")  ///
xlabel(-1 (0.1) 1) ylabel(,angle(0)) ///  
xline(0.75, lwidth(vthin) lp(shortdash)) xtitle("Coefficients")  ///
yline(0.1 ,lwidth(vthin) lp(dash)) ytitle(p value)  ///
legend(label(1 "kdensity of estimates") label( 2 "p value") pos(6))   ///
plotregion(style(none))  ///
graphregion(color(white)) 





**## 9.4 多期DID的异质性稳健估计
clear
use "D:\傻瓜计量经济学2\mpdta.dta"
xtset countyreal year
summarize
 

csdid lemp lpop,ivar(countyreal) time(year) gvar(first_treat) method(dripw) woob rseed(1) add(event)
estat event, window(-3 3)
 
csdid_plot



clear
set seed 10000 
global T=20
global N=200
set obs `=$N*$T'
gen id=int((_n-1)/$T)+1
sort id
by id:gen t=_n
xtset id t 
gen first_treat=ceil(runiform()*9)+$T -7 if t==1
gen x=rnormal(2,0.2)
by id (t): replace first_treat=first_treat[1]
gen K=t-first_treat
gen treat=K>=0 & first_treat !=.
gen beta=cond(treat==1,(t-16.2),0)
gen u=rnormal()
gen Y=1+3*t+5*x+beta*treat+u
summarize
gen gvar=cond(first_treat>20,0,first_treat)
csdid Y x ,ivar(id) time(t) gvar(gvar) method(dripw) woob rseed(1) add(event)
estat event, window(-18 6)
csdid_plot, xline(0,lpattern(dash)) legend(pos(6) row(1))
estat simple



gen gvar2=gvar 
replace gvar2=. if gvar==0
did_imputation Y id t gvar2,fe(id t) cluster(id)
did_imputation Y id t gvar2,horizon(0/5) pretrend(10) fe(id t)  
event_plot, default_look ///
graph_opt(xtitle("Periods since the event") ///
ytitle("Average causal effect")  xlabel(-10(1)5)) ///
trimlead(10) trimlag(5)  together

    

gen lastcohort=first_treat==r(max)
forvalues i=0/5{
gen L`i'event=K==`i'	
}
forvalues i=1/14{
gen F`i'event=K==-`i'		
}

eventstudyinteract Y L*event F*event ,vce(cluster id) absorb(id t) cohort(first_treat) control_cohort(lastcohort)

event_plot e(b_iw)#e(V_iw), default_look graph_opt(xtitle("Periods since the event") ytitle("Average causal effect") xlabel(-14(1)5)) stub_lag(L#event) stub_lead(F#event) together

 

did2s Y, first_stage(i.id i.t) second_stage(treat) treatment(treat) cluster(id)
did2s Y, first_stage(i.id i.t) second_stage(F*event L*event) treatment(treat) cluster(id)
event_plot  , default_look graph_opt(xtitle("Periods since the event") ytitle("Average causal effect") xlabel(-14(1)5)) stub_lag(L#event) stub_lead(F#event) together


did_multiplegt_old Y id t treat, robust_dynamic dynamic(5) placebo(5) breps(100) cluster(id)

event_plot e(estimates)#e(variances), default_look   e(estimates)#e(variances) graph_opt(xtitle("Periods since the event")   ytitle("Average causal effect") xlabel(-5(l)5))  




use "D:\傻瓜计量经济学2\bacon_example.dta", clear
sum 
bacondecomp asmrs post pcinc asmrh cases, stub(Bacon_) robust ddetail gropt(legend(pos(6) row(1)))


bacondecomp Y treat, stub(Bacon_) robust ddetail gropt(legend(pos(6) row(1)))
xtdidregress (asmrs pcinc asmrh cases) (post), group(stfips) time(year) vce(robust)
estat bdecomp,graph







**# 9.5合成控制法及安慰剂检验
use smoking.dta, clear
tsset state year
sum 
synth cigsale beer(1984(1)1988) lnincome retprice age15to24 cigsale(1988) cigsale(1980) cigsale(1975), trunit(3) trperiod(1989) fig nested keep(smoking_synth, replace)

use smoking_synth.dta, clear
twoway (line _Y_treated _time, xline(1989, lp(dash))) (line _Y_synthetic _time, lp(dash)), legend(pos(6) row(1))


use smoking_synth.dta, clear
gen Treffect= _Y_treated- _Y_synthetic 
line Treffect _time, xline(1989, lp(dash)) yline(0, lp(dash)) legend(pos(6) row(1))



use smoking.dta, clear
tsset state year  
forvalue i=1/39{
qui synth cigsale retprice cigsale(1988) cigsale(1980) cigsale(1975), trunit(`i') trperiod(1989) xperiod(1980(1)1988) keep(synth`i', replace)
}  
forvalue i=1/39{
use synth`i', clear
rename _time year
gen Treffect`i' = _Y_treated - _Y_synthetic
keep year Treffect`i'
drop if year==.  
save synth`i', replace
}  
use synth1, clear
forvalue i=2/39{
qui merge 1:1 year using synth`i', nogenerate
} 
twoway (line Treffect1- Treffect2 year, lpattern(dash ..) lcolor(gs8 ..)) (line Treffect4- Treffect15 year, lpattern(dash ..) lcolor(gs8 ..)) (line Treffect16- Treffect26 year, lpattern(dash ..) lcolor(gs8 ..)) (line Treffect27- Treffect39 year, lpattern(dash ..) lcolor(gs8 ..)) (line Treffect3 year, lcolor(black) xline(1989, lpattern(dash)) legend(off))







use smoking,clear
tsset state year  
synth_runner cigsale beer(1984(1)1988) lnincome(1972(1)1988) retprice age15to24 cigsale(1988) cigsale(1980) cigsale(1975), trunit(3) trperiod(1989) gen_vars

single_treatment_graphs, trlinediff(-1) effects_ymax(35) effects_ymin(-35) effects_ylabels(-30(10)30) do_color(gray) raw_options(legend(pos(6) row(1))) effects_options(legend(pos(6) row(1)))
effect_graphs, trlinediff(-1) tc_options(legend(pos(6) row(1))) effect_options(legend(pos(6) row(1)))

pval_graphs 


capture drop pre_rmspe post_rmspe lead effect cigsale_synth
generate byte D = (state==3 & year>=1989)  
synth_runner cigsale beer(1984(1)1988) lnincome(1972(1)1988) retprice age15to24, d(D) trends training_propr(`=13/18') gen_vars pre_limit_mult(10)
single_treatment_graphs, scaled
effect_graphs, scaled
pval_graphs
 

program drop _all
use smoking.dta, clear
tsset state year
capture drop D  
program my_pred, rclass
args tyear  
return local predictors "beer(`= `tyear' -4'(1)`=`tyear'-1') lnincome(`=`tyear'- 4'(1)`=`tyear' -1')"  
end 
program my_drop_units
args tunit  
if `tunit'==39 qui drop if inlist(state,21,38)  
if `tunit'==3 qui drop if state==21  
end
program my_xperiod, rclass
args tyear  
return local xperiod "`=`tyear'-12'(1)`=`tyear'-1'"
end 
program my_mspeperiod, rclass
args tyear 
return local mspeperiod "`=`tyear'-12'(1)`=`tyear'-1'"
end 
generate byte D = (state==3 & year>=1989) | (state==7 & year>=1988)   
synth_runner cigsale retprice age15to24, d(D) pred_prog(my_pred) trends training_propr(`=13/18') drop_units_prog(my_drop_units) xperiod_prog(my_xperiod) speperiod_prog(my_mspeperiod) 
effect_graphs, tc_options(legend(pos(6) row(1)))
pval_graphs
 
generate byte D2 = (state==3 & year>=1989) | (state==7 & year>=1988) | (state==12 & year>= 1987)| (state==14 & year>=1986)
synth_runner cigsale retprice age15to24, d(D2) pred_prog(my_pred) trends training_ propr(`=13/18') drop_units_prog(my_drop_units)) xperiod_prog(my_xperiod) speperiod_ prog(my_mspeperiod)


use prop99_example.dta,clear
sum
sdid packspercapita state year treated, vce(placebo) seed(1213) graph g1_opt(xtitle("")) g2_opt(ylabel(0(0.2)1, axis(2))) 

use smoking.dta, clear
gen treated=1 if state==3 & year>=1989
replace treated=0 if treated==.
sdid cigsale state year treated, vce(placebo) seed(1213) graph g1_opt(xtitle("")) g2_opt(ylabel(0(0.5)1, axis(2))) g1on 
 

capture matrix drop b

use prop99_example.dta, clear
encode state, gen(state2)
save prop99_example_1.dta,replace
clear  
mat c = J(100,1,0)   
forvalues i=1/100{
use prop99_example_1.dta, clear
xtset state2 year  
keep if year==1989  
sample 1, count  
keep state2  
save match_state2.dta, replace  
merge 1:m state2 using prop99_example_1.dta  
gen treat = (_merge == 3)  
gen period = (year>= 1989)
gen treated2 = treat*period
sdid packspercapita state year treated2, vce(placebo) 
mat c[`i',1] = e(ATT)
}  
svmat c, names(coef1) 
twoway (kdensity coef1), ///
title("Placebo Test") ///
xlabel(-30(2)30) ylabel(,angle(0)) ///
xline(-15.6, lwidth(vthin) lp(shortdash)) xtitle("Coefficients") ///
plotregion(style(none)) ///
graphregion(color(white))  
 

use quota_example.dta, clear
sum 
sum quota year if quota==1
list if quota==1 
drop if lnmmrt==.  
sdid lnmmrt country year quota, vce(bootstrap ) seed(123) graph
 
drop if lngdp==.
sdid lnmmrt country year quota, covariates(lngdp, projected) vce(bootstrap) graph
 
 
 
 
 
 
 
 
 
 
 
 
 