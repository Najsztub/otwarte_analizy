/* Wydatki planowane na vs wykonane */
with wyd as (
select 
	rok,
	sum(wykonanie + wydatki_wygasle) wyd,
	sum(plan) wyd_plan
from trezor.rb28 r 
left join (
	select distinct on (id_dysp)
	id_dysp,
	id_dysp_nadrz,
	nazwa
	from trezor.slownik_dysponenci
	order by id_dysp, aktywny_od desc
) sld
on r.id_dysp = sld.id_dysp
where miesiac = 12 and id_dysp_nadrz = 1
group by rok
),
plan as (
SELECT
	rok,
	sum(case when wersja = 'Projekt ustawy budżetowej – Rada dialogu społecznego' then kwota else 0 end) / 1e6 wyd_rds,
	sum(case when wersja = 'Rządowy projekt ustawy budżetowej' then kwota else 0 end) / 1e6 wyd_projekt,
	sum(case when wersja = 'Ustawa budżetowa' then kwota else 0 end) / 1e6 wyd_ustawa,
	sum(case when wersja = 'Nowelizacja Ustawy budżetowej' then kwota else 0 end) / 1e6 wyd_nowela
FROM trezor.ub_wydatki
GROUP BY rok
)
select
	coalesce(wyd.rok, plan.rok) as rok,
	plan.wyd_rds, wyd_projekt, wyd_ustawa, wyd_nowela,
	round((wyd.wyd_plan/1e9)::numeric, 3) wyd_plan_mld,
	round((wyd.wyd/1e9)::numeric, 3) wyd_mld
from wyd
full join plan
on wyd.rok = plan.rok;
