select 
r.*
from trezor.rb27 r
inner join (
	select
		id_dysp 
	from trezor.slownik_dysponenci
	where id_dysp_nadrz = 1
) sd 
on r.id_dysp = sd.id_dysp 
where miesiac = 12
and czesc = '77'
and dzial = '756'
-- and rozdzial = '75601' 
-- and paragraf = '0010'
-- and rok = 2020
order by rok, miesiac