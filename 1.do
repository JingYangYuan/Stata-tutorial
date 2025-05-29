**# 1 计量经济学导论

**## 1.5计量经济学的核心逻辑
sysuse auto.dta,clear
summarize

describe //描述数据
graph twoway (scatter price length) (lfit price length) // graph twoway 是Stata作图的命令尤指直角坐标图Scatter用于画散点图Ifit用于绘制线性拟合图见图1.6
graph twoway (scatter price length) (lfit price length), legend(pos(6) col(2)) //选项legend(pos⑹col(2))代表图例放在6点钟方向呈一排两列形状见图1.7
regress price length // regress 命令代表OLS 估计见表1.2
reg price length mpg // reg 是命令regress 的缩写(见表1.3


*FWL定理
quietly reg length mpg
predict uhat, residuals
reg price uhat


reg price length mpg foreign
reg length mpg foreign
display [5967110.1/((e(mss)+e(rss))*(1-e(r2)))]^(1/2)



**## 1.7控制变量的选择与判定系数
sysuse auto.dta,clear
quietly reg price length mpg foreign
estat ovtest

reg price length mpg foreign
gen SSR_r=e(rss) // e(rss)代表上一个回归的残差平方和
predict yhat
gen yhat2=yhat^2
gen yhat3=yhat^3
gen yhat4=yhat^4
reg price length mpg foreign yhat2 yhat3 yhat4
gen SSR_ur=e(rss)
display [(SSR_r- SSR_ur)/3]/(SSR_ur/67) //计算 F(3, 67)统计量






