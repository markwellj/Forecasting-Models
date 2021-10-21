*Joshua Markwell
*Final Memo
*Due Date
*Sales


clear all
cd "U:\ECO392\Memos\Final_Memo"
capture log close
set logtype text
set more off



log using "final_memo", replace

use "project_data"

/*rename date date_bad
gen date = date_bad(date_bad, "YM")
format date %tq
tsset date*/

drop E F G H I J K L M N date

gen date = ym(year,month)
format date %tm
order date sales

tsset date

tsappend, add(6)
gen t=_n

replace month=1 if date==m(2016m1)
replace month=2 if date==m(2016m2)
replace month=3 if date==m(2016m3)
replace month=4 if date==m(2016m4)
replace month=5 if date==m(2016m5)
replace month=6 if date==m(2016m6)




/*What does data look like*/
tsline sales
/*Data looks seasonal*/
/*Will try Winters Smoother, AR, and Time
Series Decomposition*/
*********************************************************************************************************
/*Time Decomposition*/
/*Step 1: Deseasonalize Data*/
tssmooth ma ma=sales, window(6,1,5)

replace ma=. if date>m(2015m7) /*correcting stata trying to fill in gaps*/
replace ma=. if date<m(2011m7) /*correcting stata trying to fill in gaps*/

/*Centered moving average*/
tssmooth ma cma = ma, window(0,1,1)

replace cma=. if date==m(2015m7)
replace cma=. if date==m(2011m6)

/*Creating sf's*/
gen sf=sales/cma

list sf

tsline sf

/*Making S values*/
sort month
by month: egen S=mean(sf)
sort date

tab S



/*Step 2: Making trend estimates*/
reg cma t
predict T

/*Step 3: Making Cf's*/
gen cf=cma/T

tsline cf

/*Finding C*/
/*What should I use to forecast cf's?*/
/*appears to be an upward trend, going to use holts*/
tssmooth hwinters C_holt=cf, forecast(12)
list date C_holt if date>m(2015m12)

gen holt_present= C_holt if date>m(2015m6)

tsline cf holt_present

gen C=C_holt

list date C if date>m(2015m12)

/*Irregular is set to 1*/

/*Actual Forecast*/
gen sales_forecast_td=C*T*S
gen td_present=sales_forecast_td if date>m(2015m11)
tsline sales td_present

order sales_forecast
tab sales_forecast if date>m(2015m12)

	/*Pseudo Forecast/MAPE, RMSE*/
	/*Step 1: Deseasonalize Data*/
	tssmooth ma ma_pf=sales if date<m(2015m7), window(6,1,5)
	
	replace ma_pf=. if date>m(2015m1) /*correcting stata trying to fill in gaps*/
	replace ma_pf=. if date<m(2011m7) /*correcting stata trying to fill in gaps*/
	
	/*Centered moving average*/
	tssmooth ma cma_pf = ma_pf, window(0,1,1)

	replace cma_pf=. if date>m(2014m12)
	replace cma_pf=. if date==m(2011m6)

	/*Creating sf's*/
	gen sf_pf=sales/cma_pf
	
	/*Making S values*/
	sort month
	by month: egen S_pf=mean(sf_pf)
	sort date
	
	/*Step 2: Making trend estimates*/
	reg cma_pf t
	predict T_pf
	
	/*Step 3: Making Cf's*/
	gen cf_pf=cma_pf/T_pf

	tsline cf_pf
	
	/*Finding C*/
	/*What should I use to forecast cf's?*/
	/*appears to be an upward trend, going to use holts*/
	tssmooth hwinters C_holt_pf=cf_pf, forecast(12)
	list date C_holt_pf

	tsline cf_pf C_holt_pf

	gen C_pf=C_holt_pf
	
	/*Irregular is set to 1*/

	/*Pseudo Forecast*/
	gen sales_pf_td=C_pf*T_pf*S_pf
	tsline sales sales_pf_td

	order sales_pf_td
	tab sales_pf if date>m(2015m6)
	
	/*Creating MAPE and RMSE*/
	/*MAPE*/
	gen abs_err_pf_td=abs(sales-sales_pf) if date>m(2015m6) & date<m(2016m1)
	gen pct_err_pf_td=abs_err_pf_td/sales
	egen mape_td=mean(pct_err_pf_td)
	
	/*RMSE*/
	gen err_pf_td=sales-sales_pf if date>m(2015m6) & date<m(2016m1)
	gen err_pf_td_sq=err_pf_td*err_pf_td
	egen err_mse_pf_td=mean(err_pf_td_sq)
	gen rmse_td =err_mse_pf_td^0.5
	
	sum mape_td rmse_td
	
	

*********************************************************************************************************
/*Smoother*/

	/*Pseudo MAPE/RMSE*/
	tssmooth shwinters sales_wint_pf=sales if date <m(2015m7), forecast(6) iterate(25) /*Pseudo forecast*/
	
	/*MAPE*/
	gen abs_err_wint_pf=abs(sales-sales_wint_pf) if date>m(2015m6)
	gen pcterr_wint_pf=abs_err_wint_pf/sales
	egen mape_wint_pf=mean(pcterr_wint_pf)
	
	/*RMSE*/
	gen err_wint_pf=sales-sales_wint_pf if date>m(2015m6)
	gen err_wint_pf_sq=err_wint_pf*err_wint_pf
	egen mse_wint_pf=mean(err_wint_pf_sq)
	gen rmse_wint_pf=(mse_wint_pf)^0.5
	
	
	sum mape_wint_pf rmse_wint_pf
	
	
	
	/*Actual Forecast*/
	tssmooth shwinters sales_wint_f=sales, forecast(6) iterate(25)
	gen wint_present=sales_wint_f if date>m(2015m11)
	
	tsline sales wint_present
	
	list date sales_wint_f if date>m(2015m12)
	
********************************************************************************************************

/*Auto Regressive*/
tsline sales
dfuller sales
/*p-value=0, therefore we reject Ho, and the data is stationary, we 
can use AR model*/

/*Begin at AR(5)*/
reg sales l1.sales l2.sales l3.sales l4.sales l5.sales
estat ic

/*AR(4)*/
reg sales l1.sales l2.sales l3.sales l4.sales if date>m(2011m6)
estat ic

/*AR(3)*/
reg sales l1.sales l2.sales l3.sales if date>m(2011m6)
estat ic

/*AR(4) is best model, will improve AR(4)*/
/*AR(4)*/
reg sales l1.sales l2.sales l3.sales l4.sales if date>m(2011m6)
estat ic

/*Remove the insignificant lags-->l1 and l3*/
reg sales l2.sales l4.sales if date>m(2011m6)
estat ic

/*Can add monthly dummy variables*/
tab month, gen(m)
reg sales l2.sales l4.sales m1-m11 if date>m(2011m6)
/*Uses December as reference variable*/
estat ic
/*Best AIC and BIC yet*/

/*Test insiginificant months*/
test m2 m4 m5 m7 m8 m10 m11
/*p=.8058, therefore fail to reject Ho, months have affect of 0, remove
from model*/


/*Model with only significant lags and months*/
reg sales l2.sales l4.sales m1 m3 m6 m9 if date>m(2011m6)
estat ic

/*Actual Forecast*/
/*Forecast 1*/
reg sales l2.sales l4.sales m1 m3 m6 m9
predict ar_f1
gen ar_forecast=sales
replace ar_forecast=ar_f1 if date==m(2016m1)

/*Forecast 2*/
reg sales l2.ar_forecast l4.ar_forecast m1 m3 m6 m9 if date<m(2016m1)
predict ar_f2
replace ar_forecast=ar_f2 if date==m(2016m2)

/*Forecast 3*/
reg sales l2.ar_forecast l4.ar_forecast m1 m3 m6 m9 if date<m(2016m1)
predict ar_f3
replace ar_forecast=ar_f2 if date==m(2016m3)

/*Forecast 4*/
reg sales l2.ar_forecast l4.ar_forecast m1 m3 m6 m9 if date<m(2016m1)
predict ar_f4
replace ar_forecast=ar_f4 if date==m(2016m4)

/*Forecast 5*/
reg sales l2.ar_forecast l4.ar_forecast m1 m3 m6 m9 if date<m(2016m1)
predict ar_f5
replace ar_forecast=ar_f5 if date==m(2016m5)

/*Forecast 6*/
reg sales l2.ar_forecast l4.ar_forecast m1 m3 m6 m9 if date<m(2016m1)
predict ar_f6
replace ar_forecast=ar_f6 if date==m(2016m6)

list date ar_forecast if date>m(2015m12)

tsline ar_forecast sales



	/*Pseudo Forecast*/
	/*Pseudo 1*/
	reg sales l2.sales l4.sales m1 m3 m6 m9 if date <m(2015m7)
	predict ar_f1_pf
	gen ar_pf=sales
	order ar_pf
	replace ar_pf=ar_f1_pf if date==m(2015m7)
	
	/*Pseudo 2*/
	reg sales l2.sales l4.sales m1 m3 m6 m9 if date <m(2015m7)
	predict ar_f2_pf
	replace ar_pf=ar_f2_pf if date==m(2015m8)
	
	/*Pseudo 3*/
	reg sales l2.sales l4.sales m1 m3 m6 m9 if date <m(2015m7)
	predict ar_f3_pf
	replace ar_pf=ar_f3_pf if date==m(2015m9)
	
	/*Pseudo 4*/
	reg sales l2.sales l4.sales m1 m3 m6 m9 if date <m(2015m7)
	predict ar_f4_pf
	replace ar_pf=ar_f4_pf if date==m(2015m10)
	
	/*Pseudo 5*/
	reg sales l2.sales l4.sales m1 m3 m6 m9 if date <m(2015m7)
	predict ar_f5_pf
	replace ar_pf=ar_f5_pf if date==m(2015m11)
	
	/*Pseudo 6*/
	reg sales l2.sales l4.sales m1 m3 m6 m9 if date <m(2015m7)
	predict ar_f6_pf
	replace ar_pf=ar_f6_pf if date==m(2015m12)
	
	tsline sales ar_pf 
	
	
	/*MAPE*/
	gen abs_err_ar=abs(sales-ar_pf) if date >m(2015m6) & date<m(2016m1)
	gen pct_err_ar=abs_err_ar/sales
	egen mape_ar=mean(pct_err_ar)
	
	/*RMSE*/
	gen err_ar=sales-ar_pf if date>m(2015m6) & date<m(2016m1)
	gen err_ar_sq=err_ar*err_ar 
	egen ar_mse=mean(err_ar_sq)
	gen rmse_ar=ar_mse^0.5
	
	sum mape_ar rmse_ar
	
************************************************************************************************************
/*Combined Forecast*/


reg sales l2.sales l4.sales m1 m3 m6 m9
predict ar_comb_f
replace ar_comb_f=ar_forecast if date>m(2015m12)


reg sales sales_forecast_td sales_wint_f ar_comb_f
/*This forecast isnt all weird, combined forecasts
above are strange*/
/*Bo, is insignificant! Woohoo*/

reg sales sales_forecast_td sales_wint_f ar_comb_f, noc
/*Winters is very insignificant, remove from model*/

reg sales sales_forecast_td ar_comb_f, noc
/*Both forecast are significant at the 15% level,
go ahead and use them b/c I only have 60 observations*/
predict f_comb

list date f_comb if date>m(2015m12)

gen comb_present=f_comb if date>m(2015m11)
tsline sales comb_present


/*Pseudo Forecast*/
reg sales l2.sales l4.sales m1 m3 m6 m9 if date<m(2015m7)
predict ar_comb_pf
replace ar_comb_pf=ar_pf if date>m(2015m6)
list date ar_comb_pf if date>m(2015m6)

reg sales sales_forecast_td ar_comb_pf if date<m(2015m7), noc
predict comb_pf if date<m(2016m1)

list date comb_pf if date>m(2015m6) & date<m(2016m1)


/*MAPE*/
gen abs_err_comb=abs(sales-comb_pf) if date>m(2015m6) & date<m(2016m1)
gen pct_err_comb=abs_err_comb/sales
egen mape_comb=mean(pct_err_comb)
sum mape_comb

/*RMSE*/
gen err_comb=sales-comb_pf if date>m(2015m6) & date<m(2016m1)
gen err_comb_sq=err_comb*err_comb
egen mse_comb=mean(err_comb_sq)
gen rmse_comb=mse_comb^0.5

sum mape_comb rmse_comb

******************************************************************************************************
order sales ar_pf sales_wint_pf sales_pf_td



log close
