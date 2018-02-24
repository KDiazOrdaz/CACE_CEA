******Stata Do file performing multiple imputation followed by IV analysis using 3sls******

****MI
*data needs to be sorted by randomised treatment 
sort rnd
mi set mlong
*register the variables to be imputed (here two outcomes and one baseline covariate)
mi register imputed total_qalys total_costs eq5d_b   
*perform MI (here by predictive mean matching, using treatment received in the imputation models and doing it *separately by randomised group (the by statement)

mi impute chained (pmm, knn(5) ) total_qalys  eq5d_b total_costs=   txreceived,///
	 by (rnd) add(50)   rseed (110)

*3sls and Rubin's rules
mi estimate, cmdok: reg3 (total_costs total_qalys== txreceived), exog(rnd) ireg
mi estimate(inb: _b[total_qalys:txreceived] *30000 - _b[total_costs:txreceived]), ///
	cmdok: reg3 (total_costs total_qalys==txreceived), exog(rnd)

mi estimate (icer: _b[total_costs:txreceived]/_b[total_qalys:txreceived] ), ///
	cmdok: reg3 (total_costs total_qalys== txreceived), exog(rnd)

			
			
