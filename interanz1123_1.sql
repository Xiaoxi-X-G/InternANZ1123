/* 10449
select distinct DB.companyId from debt_balance DB
inner join cashflow CF
on CF.capiq_company_id = DB.companyId
*/

/* 10661
select distinct DB.companyId from debt_balance DB
inner join income_statement INS
on INS.capiq_company_id = DB.companyId
*/

/* 10448
select distinct DB.companyId from debt_balance DB
inner join balance_sheet BS
on BS.capiq_company_id = DB.companyId
inner join income_statement INS
on INS.capiq_company_id = DB.companyId
inner join cashflow CF
on CF.capiq_company_id = DB.companyId
*/

select distinct DB.companyId from debt_balance DB
inner join balance_sheet BS
on BS.capiq_company_id = DB.companyId
inner join 

select distinct DB.companyId from debt_balance DB

select distinct CF.capiq_company_id  from cashflow CF




