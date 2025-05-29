*-----------------------《"傻瓜"计量经济学与Stata应用（第二版）》学习笔记------------------
*-----------------------------------作者：习明明-------------------------------------------

	* dotemplate // 生成新do-file

	**# 基础知识

	// Stata 是统计学和计量经济学的工具，广泛用于学术研究。
	// 默认界面分为三部分：左侧为历史记录，上方为输出窗口，下方为命令输入窗口。

	edit // 打开数据编辑器（编辑模式）

	browse // 打开数据编辑器（浏览模式），与 edit 类似，但不可编辑。

	// 注释方式：
	// 1. 单行注释：使用双斜杠 `//`，需与命令间留空格。
	// 2. 整行注释：行首加星号 `*`。
	// 3. 跨行注释：使用 `/* 注释内容 */`。

	display /* 示例：跨行注释
	可以写多行内容
	*/ 10 * 10 // 计算 10 乘以 10 的结果。

	help mathematical function // 使用 help 查看数学函数的帮助文档。

	// 新版本 Stata 支持自动换行，无需手动分行。
	// 自动换行可能导致注释中空格过多，影响阅读体验，建议用 `-` 替代多余空格。

	global libname "D:\BaiduNetdiskDownload\data" // 定义全局路径
	cd "D:\stata"                                 // 设置工作目录

	sysdir                                        // 查看系统目录（较少用）
	sysuse auto.dta, clear                        // 加载系统自带数据集 auto.dta，并清除原有数据
	help dta_examples                             // 查看系统自带数据集列表

	// 回归分析示例
	regress mpg foreign weight headroom trunk length turn displacement 
	// 结果解读：
	// - 左上角：TSS、ESS 和 RSS 等统计量。
	// - 右上角：F 检验、拟合优度（R²）、调整 R²、均方误差（MSE）等。
	// - 下方：回归系数、标准误、t 值、p 值及置信区间。

	est store mm                                  // 存储回归结果
	esttab mm , mtitle(回归结果) se(3) b(%6.3f) r2 ar2 aic bic compress nogaps star(* 0.1 ** 0.05 *** 0.01)
	// 输出回归结果表格，包含标准误、系数、R²、调整 R²、AIC、BIC 等信息。

	esttab mm using "111.rtf", label mtitle(回归结果) se(3) b(%6.3f) r2 ar2 aic bic compress nogaps star(* 0.1 ** 0.05 *** 0.01) replace
	// 将回归结果导出为 RTF 文件。

	// 安装外部命令
	ssc install outreg2, replace                  // 安装或更新 outreg2 扩展包

	* 小练习：了解 outreg2 函数
	// 1. 使用 help outreg2 查看帮助文档。
	// 2. 找到 Example-0.Basic-game-plan 的示例代码。
	// 3. 复述示例内容并注释在 dofile 文件后，保存为独立文件以便复用。

	* 示例内容：
	// 加载数据集 auto.dta，运行回归模型 regress price mpg weight，
	// 使用 outreg2 导出回归结果到 Word 或 Excel 文件中。
	// 可通过选项控制输出格式（如标准误、系数、R² 等）。

*------------------------------------------------------------------------------------------
**# 第一章 计量经济学导论

**# -1、计量经济学的核心逻辑
    sysuse auto.dta, clear          // 加载Stata自带数据集auto.dta，并清除当前内存中的数据
    summarize                       // 对数据进行描述性统计分析，显示变量的基本统计信息

    describe                        // 查看数据集的结构和变量类型
    graph twoway (scatter price length) (lfit price length), /// 
		title("图1.6") note("注：绘制价格与长度的散点图，并添加线性拟合线")
    graph twoway (scatter price length) (lfit price length), legend(pos(6) col(2)) /// 
		title("图1.7") note("注：调整图例位置为6点钟方向，呈两列布局")
    regress price length            // 使用OLS方法估计price对length的回归模型
    reg price length mpg            // 简写形式，等价于regress命令

    * FWL定理（Frisch-Waugh-Lovell Theorem）
    quietly reg length mpg          // 静默运行回归，提取length对mpg的残差
    predict uhat, residuals         // 提取残差并命名为uhat
    reg price uhat                  // 回归price对uhat，验证FWL定理

    reg price length mpg foreign    // 多元回归模型，加入foreign作为控制变量
    reg length mpg foreign          // 回归length对mpg和foreign
    display [5967110.1 / ((e(mss) + e(rss)) * (1 - e(r2)))] ^ (1 / 2) 
                                    // 手动计算标准误相关公式

**# -2、控制变量的选择与判定系数
    sysuse auto.dta, clear          // 再次加载数据集
    quietly reg price length mpg foreign 
    estat ovtest                    // 进行RESET检验以检测模型设定是否正确

    reg price length mpg foreign    // 多元回归模型
    gen SSR_r = e(rss)              // 提取残差平方和（RSS），命名为SSR_r
    predict yhat                    // 预测拟合值，命名为yhat
    gen yhat2 = yhat^2              // 构造二次项
    gen yhat3 = yhat^3              // 构造三次项
    gen yhat4 = yhat^4              // 构造四次项
    reg price length mpg foreign yhat2 yhat3 yhat4 
                                    // 包含高阶项的回归模型
    gen SSR_ur = e(rss)             // 提取新的残差平方和（RSS），命名为SSR_ur
    display [(SSR_r - SSR_ur) / 3] / (SSR_ur / 67) 
                                    // 计算F统计量，用于检验高阶项的显著性




