
clear
window manage forward results

use "data/itt_other_studies.dta"

drop if te_pct < 0  // we are only reporting positive earning estimates

ge cb =  (te_USD /cost)*100
ge cb_UB =  (UB_USD /cost)*100
ge cb_LB =  (LB_USD /cost)*100

ge rb = te_pct
ge rb_UB =  UB_pct
ge rb_LB =  LB_pct

ge workshop = (Intervention == "Information about skills" & Country == "Ethiopia")
ge transport = (Intervention == "Transport subsidy" & Country == "Ethiopia")

 
replace rb = rb/100  //
replace rb_UB = rb_UB /100
replace rb_LB = rb_LB /100

sort rb


ge z = _n
ge ad = (transport ==1 | workshop ==1)

twoway (bar rb z if z<= 13 & workshop == 1, msize(*1.2) pstyle(p2) fcolor(red) fi(inten30)   )  ///
	(bar rb z if z<= 13 & transport == 1, msize(*1.2) pstyle(p3) fcolor(green) fi(inten30)   )  ///
	(bar rb z if z<= 13 & ad ==0, pstyle(p1)  fcolor(white) ) ///
	(rcap rb_UB rb_UB z if z<= 13 & ad == 0, pstyle(p1) ) ///
	(rcap rb_LB rb_LB z if z<= 13 & ad == 0, pstyle(p1) ) ///
	(rspike rb rb_LB z if z<= 13 & ad == 0, pstyle(p1)) ///
	(rspike rb rb_UB z if z<= 13 & ad == 0, pstyle(p1)) ///
	(rcap rb_UB rb_UB z if z<= 13 & workshop == 1, pstyle(p2) ) ///
	(rcap rb_LB rb_LB z if z<= 13 & workshop == 1, pstyle(p2) ) ///
	(rspike rb rb_LB z if z<= 13 & workshop == 1, pstyle(p2) ) ///
	(rspike rb rb_UB z if z<= 13 & workshop == 1, pstyle(p2) )  /// 
		(rcap rb_UB rb_UB z if z<= 13 & transport == 1, pstyle(p3) ) ///
	(rcap rb_LB rb_LB z if z<= 13 & transport == 1, pstyle(p3) ) ///
	(rspike rb rb_LB z if z<= 13 & transport == 1, pstyle(p3) ) ///
	(rspike rb rb_UB z if z<= 13 & transport == 1, pstyle(p3) )  /// 
	(scatter cost z if z<= 13 & workshop == 1, pstyle(p2) yaxis(2) )  ///
		(scatter cost z if z<= 13 & transport == 1, pstyle(p3) yaxis(2) )  ///
	(scatter cost z if z<= 13 & ad == 0, pstyle(p1) yaxis(2) ), ///
  ytitle("Percent impact on monthly earnings ") ytitle("Cost (USD)", axis(2)) xtitle("Intervention")  ylabel(0(.2)1, grid gst(foreground) glw(medthick)) yticks(0(.2)1) ///
       yscale(range(-.2 1)) yscale(range(-400 1700) axis(2))  xscale(range(0.5 10.5) axis(1))   xlabel("", nolabels) xticks(, nolabels) ///
      graphr(fc(white) lc(white)) graphregion(margin(l+5 r+5)) xtitle("", margin(medium)) ///
        yline(0, lcolor(black)) legend (order (1 2 )) legend(label (1 "Workshop")) legend(label (2 "Transport"))  legend(label (3 "Other interventions"))
  
graph export "results/figures/figure5a.png", replace width(1600) height(1200)



drop z
sort cb
ge z = _n

 
 twoway (bar cb z if  workshop ==1, pstyle(p2) msize(*1.2) fcolor(red) fi(inten30) ) ///
 (bar cb z if  transport ==1, pstyle(p3) msize(*1.2) fcolor(green) fi(inten30) ) ///
 (bar cb z if  ad ==0, pstyle(p1)  fcolor(white)  )  ///
 (rcap cb_UB cb_UB z if   ad == 0, pstyle(p1) ) ///
	(rcap cb_LB cb_LB z if ad == 0, pstyle(p1) ) ///
	(rspike cb cb_LB z if  ad == 0, pstyle(p1)) ///
	(rspike cb cb_UB z if  ad == 0, pstyle(p1)) ///
	(rcap cb_UB cb_UB z if  workshop == 1, pstyle(p2) ) ///
	(rcap cb_LB cb_LB z if  workshop == 1, pstyle(p2) ) ///
	(rspike cb cb_LB z if workshop == 1, pstyle(p2) ) ///
	(rspike cb cb_UB z if  workshop == 1, pstyle(p2) ) ///
	(rcap cb_UB cb_UB z if  transport == 1, pstyle(p3) ) ///
	(rcap cb_LB cb_LB z if  transport == 1, pstyle(p3) ) ///
	(rspike cb cb_LB z if transport == 1, pstyle(p3) ) ///
	(rspike cb cb_UB z if  transport == 1, pstyle(p3) ) , ///
  ytitle("Impact on monthly earnings / cost (in 100 USD)") xtitle("Intervention")  ylabel(-10(20)100, grid gst(foreground) glw(medthick))  ///
       yscale(range(-10 100))     xlabel("", nolabels) xticks(, nolabels) ///
      graphr(fc(white) lc(white)) graphregion(margin(l+5 r+5)) xtitle("", margin(medium)) ///
       yline(0, lcolor(black)) legend (order (1 2 )) legend(label (1 "Workshop")) legend(label (2 "Transport"))  legend(label (3 "Other interventions")) 
graph export "results/figures/figure5b.png", replace width(1600) height(1200)




