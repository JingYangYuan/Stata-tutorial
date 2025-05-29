**# 第8章 离散与受限因变量模型
cd "D:\傻瓜计量经济学2"


**## 8.1 二值Logit 和Probit 模型
use "D:\傻瓜计量经济学2\double11.dta",clear
asdoc sum join peer ivfriends gender exper major poor income grade, replace

logit join peer gender exper major poor income grade
margins, dydx(*)
probit join peer gender exper major poor income grade
margins, dydx(*)
cloglog join peer gender exper major poor income grade
margins, dydx(*)
display 1.204725/0.7330709
logit join peer gender exper major poor income grade,or




ivprobit join gender exper major poor income grade (peer=ivfriends)
margins, dydx(*) predict(pr)

ivprobit join gender exper major poor income grade (peer=ivfriends), twostep first

predict xb, xb  
scalar beta_peer = _b[peer]  
gen me_peer = normalden(xb)* beta_peer
sum me_peer


help eprobit
eprobit join gender exper major poor income grade,endog(peer=ivfriends gender exper major poor income grade)
margins, dydx(*) 



use "D:\傻瓜计量经济学2\union.dta", clear
sum
xtset idcode year
xtprobit union age grade i.not_smsa south##c.year, re
margins, dydx(*) 

xtlogit union age grade i.not_smsa south##c.year, re
margins, dydx(*) 

xtlogit union age grade i.not_smsa south##c.year, fe
margins, dydx(*) 


**## 8.2多元结果模型
use "D:\傻瓜计量经济学2\nhanes2f.dta",clear
sum sampl health female black age height weight houssiz
ologit health female black age c.age#c.age height weight houssiz
predict y_hat, xb
predict one two three four five, pr
list health y_hat one two three four five if sampl==1400
margins, dydx(*) 


oprobit health female black age c.age#c.age height weight houssiz
margins, dydx(*) 
predict y_hat2, xb
predict pl p2 p3 p4 p5, pr
margins, dydx(female)


use "D:\傻瓜计量经济学2\mus15data.dta",clear
sum
table mode
mlogit mode income, baseoutcome(1)
mlogit mode income, baseoutcome(1) rrr
margins,  dydx(*) 

mlogit mode income
mlogtest, hausman     
help mlogtest
mlogtest, hausman  iia  



mlogit mode income
estimates store all
mlogit mode income if mode != 3 
estimates store partial
hausman partial all, alleqs constant

mprobit mode income, base(1)




**## 8.3决策选择模型(choice model)
use "D:\傻瓜计量经济学2\travel.dta",clear
cmset id mode
cmmprobit choice travelcost termtime, casevars(income)
margins
margins, at(income=(30(10)70)) outcome(car)
marginsplot
margins, at(income=(30(10)70)) outcome(train)
marginsplot
margins, at(income=(30(10)70)) 
marginsplot, noci legend(pos(6) row(1)) plot1opts(lp(dash)) plot2opts(lp(dot)) plot3opts(lp(dash_dot)) plot4opts(lp(solid))

cmclogit choice travelcost termtime, casevars(income)
margins

help cmxtmixlogit
help cmmprobit
help cmclogit
help clogit

asclogit choice travelcost termtime, case(id) alternatives(mode) casevars(income)
est store asc
cmclogit choice travelcost termtime, casevars(income)
est store cmc
esttab asc cmc,se



use "D:\傻瓜计量经济学2\choice.dta", clear
sum
cmset id car
asclogit choice dealer, case(id) alternatives(car) casevars(sex income)
estat mfx




use "D:\傻瓜计量经济学2\restaurant.dta", clear
sum
cmset family_id restaurant
nlogitgen type = restaurant(fast: Freebirds | MamasPizza, family: CafeEccell | LosNortenos | WingsNmore, fancy: Christophers | MadCows)
nlogittree restaurant type, choice(chosen) case(family_id)
nlogit chosen cost distance rating || type: income kids, base(family) || restaurant:, noconst case(family_id)


**## 8.4 样本选择模型(sample selection model)
use "D:\傻瓜计量经济学2\laborsub.dta",clear
sum
kdensity whrs, xline(0, lwidth(vthin) lp(shortdash))
histogram whrs, percent

truncreg whrs kl6 k618 wa we, ll(0)


quietly reg whrs kl6 k618 wa we
outreg2 using xmm, word replace
quietly reg whrs kl6 k618 wa we if whrs>0  
outreg2 using xmm, word
quietly truncreg whrs kl6 k618 wa we, ll(0)
outreg2 using xmm, word


tobit whrs kl6 k618 wa we, ll(0)
margins, dydx(*)

use "D:\傻瓜计量经济学2\nlswork3.dta",clear
xtset idcode year
sum idcode year ln_wage union age grade not_smsa south

xttobit ln_wage union age grade not_smsa south##c.year, ll(0) intpoint(25) tobit

xttobit ln_wage union age grade not_smsa south##c.year, ul(1.9) intpoints(25) tobit

use "D:\傻瓜计量经济学2\womenwk.dta",clear
sum
reg wage age education

heckman wage age education, select(age education married children) 

heckman wage age education, select(age education married children) twostep








