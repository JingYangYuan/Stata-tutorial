**# 第4章模 拟
cd "D:\傻瓜计量经济学2"
**## 4.1函数
help function
scalar u=runiform(4, 5)
display u

set seed 1000 //seed的值不能超过2^31值一般控制在10位数以内
scalar u1=runiform()
display u1
set seed 1000
scalar u2=runiform()
display u2
  
clear
set obs 1000  
set seed 10101  
generate x=runiform()
summarize
generate t=_n
tsset t
sum
corrgram x

corrgram x, lag(3)
pwcorr x L.x L2.x L3.x L4.x

clear
set obs 2000  
set seed 10101
gen xu=runiform(-2, 2)  
gen xn=rnormal()  
gen xc=rchi2(2000)
sum
gen xc2= rchi2(2)
gen xc5= rchi2(5)
gen xc10 =rchi2(10)
gen xc20=rchi2(20)
graph twoway (kdensity xc2, lp(dash)) (kdensity xc5, lp(dash dot)) (kdensity xc10, lp(longdash)) (kdensity xc20, lp(solid)), legend(pos(6) row(1))

gen xt=rt(2000)
gen xt5= rt(5)
gen xt10=rt(10)
gen xt20=rt(20)
graph twoway (kdensity xt, lp(solid)) (kdensity xt5, lp(dash dot)) (kdensity xt10, lp(longdash)) (kdensity xt20, lp(dash)), legend(pos(6) row(1))


gen xf2_10= (xc2/2)/(xc10/10)
gen xf5_10= (xc5/5)/(xc10/10)
gen xf5_2000=(xc5/5)/(xc/2000)
graph twoway (kdensity xf2_10, lp(dash)) (kdensity xf5_10, lp(longdash)) (kdensity xf5_2000, lp(solid)), legend(pos(6) row(1))
graph twoway (kdensity xn) (histogram xn), legend(pos(6) row(1))
graph save xn.gph, replace
graph twoway (kdensity xt) (histogram xt), legend(pos(6) row(1))
graph save xt.gph, replace
graph twoway (kdensity xc) (histogram xc), legend(pos(6) row(1))
graph save xc.gph, replace
graph twoway (kdensity xf5_2000) (histogram xf5_2000),  legend(pos(6) row(1))
graph save xf5_2000.gph, replace
graph combine xn.gph xt.gph xc.gph xf5_2000.gph

**## 4.2模拟
clear
program jxufe, rclass  
drop _all  
quietly set obs 10  
gen x=runiform()
summarize x  
return scalar meanx=r(mean)
end  
jxufe  
simulate xbar=r(meanx), seed(330013) reps(10000) nodots: jxufe
sum xbar
kdensity xbar

clear
program drop jxufe2
program jxufe2, rclass
drop _all
set obs 150
gen double x=rchi2(1)  
gen double u=rchi2(1)-1
gen y=1 + 2*x + u  
regress y x
return scalar b2 = _b[x] 
return scalar se2 =_se[x]  
return scalar t2 = (_b[x]-2)/_se[x]  
return scalar r2 = abs(return(t2))>invttail(148,0.025)
return scalar p2 = 2*ttail(148,abs(return(t2)))
end
jxufe2

simulate b2f=r(b2) se2f=r(se2) t2f=r(t2) r2f=r(r2) p2f=r(p2), reps(1000) nodots: jxufe2 
sum








































