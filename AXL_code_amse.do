net install rdmse, from(https://raw.githubusercontent.com/peizhuan/rdmse/master) replace

******************************************************************************************
** TREATMENT D1
******************************************************************************************

use "C:\Users\1604834\OneDrive - UAB\PhD thesis\00A_thesis\3_mix_Veneto\Intermediate_outputs\indi_ns_ss1_oct2023.dta"


* Y1
rdmse post_interval6 scoringD1_0, deriv(0) c(0) p(1) h(0.063) b(0.1) kernel(triangular)

rdmse post_interval6 scoringD1_0, deriv(0) c(0) p(2) h(0.08) b(0.109) kernel(triangular)

rdmse post_interval6 scoringD1_0, deriv(0) c(0) p(1) h(0.042) b(0.073) kernel(uniform)

rdmse post_interval6 scoringD1_0, deriv(0) c(0) p(2) h(0.06) b(0.093) kernel(uniform)

* Y2
rdmse post_interval712 scoringD1_0, deriv(0) c(0) p(1) h(0.062) b(0.094) kernel(triangular)

rdmse post_interval712 scoringD1_0, deriv(0) c(0) p(2) h(0.097) b(0.13) kernel(triangular)

rdmse post_interval712 scoringD1_0, deriv(0) c(0) p(1) h(0.071) b(0.123) kernel(uniform)

rdmse post_interval712 scoringD1_0, deriv(0) c(0) p(2) h(0.062) b(0.094) kernel(uniform)

* Y3
rdmse post_interval1318 scoringD1_0, deriv(0) c(0) p(1) h(0.045) b(0.073) kernel(triangular)

rdmse post_interval1318 scoringD1_0, deriv(0) c(0) p(2) h(0.081) b(0.11) kernel(triangular)

rdmse post_interval1318 scoringD1_0, deriv(0) c(0) p(1) h(0.049) b(0.09) kernel(uniform)

rdmse post_interval1318 scoringD1_0, deriv(0) c(0) p(2) h(0.056) b(0.083) kernel(uniform)


* Y4
rdmse post_interval1924 scoringD1_0, deriv(0) c(0) p(1) h(0.053) b(0.079) kernel(triangular)

rdmse post_interval1924 scoringD1_0, deriv(0) c(0) p(2) h(0.085) b(0.112) kernel(triangular)

rdmse post_interval1924 scoringD1_0, deriv(0) c(0) p(1) h(0.056) b(0.096) kernel(uniform)

rdmse post_interval1924 scoringD1_0, deriv(0) c(0) p(2) h(0.05) b(0.073) kernel(uniform)

* MECHANISMS (js hours)
rdmse jshours scoringD1_0, deriv(0) c(0) p(1) h(0.045) b(0.08) kernel(triangular)

rdmse jshours scoringD1_0, deriv(0) c(0) p(2) h(0.076) b(0.108) kernel(triangular)

rdmse jshours scoringD1_0, deriv(0) c(0) p(1) h(0.049) b(0.094) kernel(uniform)

rdmse jshours scoringD1_0, deriv(0) c(0) p(2) h(0.059) b(0.09) kernel(uniform)

* MECHANISMS (tr hours)
rdmse attiv_form_ore_prev scoringD1_0, deriv(0) c(0) p(1) h(0.088) b(0.133) kernel(triangular)

rdmse attiv_form_ore_prev scoringD1_0, deriv(0) c(0) p(2) h(0.067) b(0.09) kernel(triangular)

rdmse attiv_form_ore_prev scoringD1_0, deriv(0) c(0) p(1) h(0.054) b(0.094) kernel(uniform)

rdmse attiv_form_ore_prev scoringD1_0, deriv(0) c(0) p(2) h(0.062) b(0.09) kernel(uniform)

* PSEUDO-TREATMENT ANALYSIS ---------------------------------------------------
*use "C:\Users\alvar\UAB\OneDrive - Universitat Autònoma de Barcelona\PhD thesis\00A_thesis\3_mix_Veneto\Intermediate_outputs\indi_ns_ss1_pse_n2023.dta"
use "C:\Users\1604834\OneDrive - UAB\PhD thesis\00A_thesis\3_mix_Veneto\Intermediate_outputs\indi_ns_ss1_pse_n2023.dta"

*** Pruned sample
use "C:\Users\1604834\OneDrive - UAB\PhD thesis\00A_thesis\3_mix_Veneto\Intermediate_outputs\indi_ns_ss1_pse_n2023_rmT1.dta"

rdmse prewd1_6 scoringD1_0, deriv(0) c(0) p(1) h(0.062) b(0.093) kernel(triangular)

rdmse prewd1_6 scoringD1_0, deriv(0) c(0) p(2) h(0.073) b(0.107) kernel(triangular)

rdmse prewd1_6 scoringD1_0, deriv(0) c(0) p(1) h(0.062) b(0.093) kernel(uniform)

rdmse prewd1_6 scoringD1_0, deriv(0) c(0) p(2) h(0.073) b(0.107) kernel(uniform)



******************************************************************************************
** TREATMENT D2
******************************************************************************************
use "C:\Users\1604834\OneDrive - UAB\PhD thesis\00A_thesis\3_mix_Veneto\Intermediate_outputs\indi_ns_ss2_oct2023.dta"

* Y1
rdmse post_interval6 scoringD2_0, deriv(0) c(0) p(1) h(0.037) b(0.071) kernel(triangular)

rdmse post_interval6 scoringD2_0, deriv(0) c(0) p(2) h(0.061) b(0.091) kernel(triangular)

rdmse post_interval6 scoringD2_0, deriv(0) c(0) p(1) h(0.032) b(0.067) kernel(uniform)

rdmse post_interval6 scoringD2_0, deriv(0) c(0) p(2) h(0.052) b(0.089) kernel(uniform)

* Y2
rdmse post_interval712 scoringD2_0, deriv(0) c(0) p(1) h(0.043) b(0.083) kernel(triangular)

rdmse post_interval712 scoringD2_0, deriv(0) c(0) p(2) h(0.056) b(0.086) kernel(triangular)

rdmse post_interval712 scoringD2_0, deriv(0) c(0) p(1) h(0.058) b(0.117) kernel(uniform)

rdmse post_interval712 scoringD2_0, deriv(0) c(0) p(2) h(0.053) b(0.093) kernel(uniform)

* Y3
rdmse post_interval1318 scoringD2_0, deriv(0) c(0) p(1) h(0.048) b(0.085) kernel(triangular)

rdmse post_interval1318 scoringD2_0, deriv(0) c(0) p(2) h(0.05) b(0.079) kernel(triangular)

rdmse post_interval1318 scoringD2_0, deriv(0) c(0) p(1) h(0.052) b(0.095) kernel(uniform)

rdmse post_interval1318 scoringD2_0, deriv(0) c(0) p(2) h(0.048) b(0.083) kernel(uniform)

* Y4
rdmse post_interval1924 scoringD2_0, deriv(0) c(0) p(1) h(0.043) b(0.080) kernel(triangular)

rdmse post_interval1924 scoringD2_0, deriv(0) c(0) p(2) h(0.047) b(0.077) kernel(triangular)

rdmse post_interval1924 scoringD2_0, deriv(0) c(0) p(1) h(0.053) b(0.097) kernel(uniform)

rdmse post_interval1924 scoringD2_0, deriv(0) c(0) p(2) h(0.045) b(0.082) kernel(uniform)

* MECHANISMS (js hours)
rdmse jshours scoringD2_0, deriv(0) c(0) p(1) h(0.074) b(0.114) kernel(triangular)

rdmse jshours scoringD2_0, deriv(0) c(0) p(2) h(0.074) b(0.101) kernel(triangular)

rdmse jshours scoringD2_0, deriv(0) c(0) p(1) h(0.056) b(0.098) kernel(uniform)

rdmse jshours scoringD2_0, deriv(0) c(0) p(2) h(0.063) b(0.095) kernel(uniform)

* MECHANISMS (tr hours)
rdmse attiv_form_ore_prev scoringD2_0, deriv(0) c(0) p(1) h(0.055) b(0.085) kernel(triangular)

rdmse attiv_form_ore_prev scoringD2_0, deriv(0) c(0) p(2) h(0.097) b(0.135) kernel(triangular)

rdmse attiv_form_ore_prev scoringD2_0, deriv(0) c(0) p(1) h(0.05) b(0.093) kernel(uniform)

rdmse attiv_form_ore_prev scoringD2_0, deriv(0) c(0) p(2) h(0.06) b(0.092) kernel(uniform)

* PSEUDO-TREATMENT ANALYSIS ---------------------------------------------------
use "C:\Users\1604834\OneDrive - UAB\PhD thesis\00A_thesis\3_mix_Veneto\Intermediate_outputs\indi_ns_ss2_pse_n2023.dta"


** Pruned sample (filter to treated at semester t>=2)
use "C:\Users\1604834\OneDrive - UAB\PhD thesis\00A_thesis\3_mix_Veneto\Intermediate_outputs\indi_ns_ss2_pse_n2023_rmT1.dta"

rdmse prewd1_6 scoringD2_0, deriv(0) c(0) p(1) h(0.055) b(0.086) kernel(triangular)

rdmse prewd1_6 scoringD2_0, deriv(0) c(0) p(2) h(0.065) b(0.090) kernel(triangular)

rdmse prewd1_6 scoringD2_0, deriv(0) c(0) p(1) h(0.055) b(0.086) kernel(uniform)

rdmse prewd1_6 scoringD2_0, deriv(0) c(0) p(2) h(0.065) b(0.09) kernel(uniform)
