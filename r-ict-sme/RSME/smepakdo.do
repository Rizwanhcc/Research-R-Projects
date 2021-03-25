*===============SME SURVEY OF PAKSITAN=================


*import data from excel


*recoding variablesss
gen bank = k6
replace bank = 0 if bank==2

*overdraft
gen over = k7
replace over = 0 if over==2

*loc
gen loc = k8
replace loc = 0 if loc==2

*wcapital
gen wc = k3bc
replace wc = 0 if wc==2

*take log of total sales
gen lnsales= ln(sales)
  
*email
gen email = c22a
replace email = 0 if email==2

*email
gen web = c22b
replace web = 0 if web==2

*Rejected application
gen rej = k20a
replace rej = . if rej==-6

*applying probit model

probit bank i.a6b i.a3a b7a l9b age c22a
probit bank i.a6b i.a3a b7a l9b age c22b
probit bank i.a6b i.a3a l9b c22a c22b lnage b6 lnsales
probit web bank i.a6b i.a3a l9b c22a lnage b6 lnsales
probit web loc i.a6b i.a3a l9b lnage b6 lnsales


**female top manager is insignificant everywher b7a

*by applying ivprobit

ivprobit bank i.a6b i.a3a l9b  (c22a c22b=lnage b6 lnsales )
ivprobit over i.a6b i.a3a l9b lnsales (c22b=lnage)
ivprobit loc i.a6b i.a3a l9b b6 lnsales (web=lnage)
ivprobit wc i.a6b i.a3a l9b age lnsales (c22b=lnage)

**ordered probit for access to finance as constraints k30

oprobit k30 i.a6b i.a3a l9b age lnsales c22b lnage

**post ivregess tests
estat endog
estate firststage

*\DATA for sankey diagram in R SIZE AND FINANCIAL CONSTRAINTS
*USUALLY DEPENDENT/RESPONSE VARIABLE IS KEPT IN ROW OF THE CONT TABLE
tab k30 a2, cell /*by grand total*/
tab k30 a2, column /*by grand column*/
tab k13 a6b, row /*by grand row*/

**\\\\\

tab k30 a2, row   /*a2 region */
tab k30 a6b, row   /*a6b size */
tab k20a a6b  /* firm size and loan application rejection*/
tab rej a6b  /* firm size and loan application rejection*/
tab rej a6b, row /* firm size and loan application rejection*/
tab k9 a6b
**three way table
*Now the 3-way table.

table k13 a6b k20, contents(freq)
table k9 a6b k13, contents(freq)
table k9 a6b, contents(freq)
table k9 a2, contents(freq)
table k9 k13 a6b , contents(freq)

*Followed by a 4-way table.

table k13 k20 k9, by(a6b) contents(freq)



