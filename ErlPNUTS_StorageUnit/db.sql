select * from (select * from `rev` where fieldversion <= 2 order by fieldversion desc) as R group by fieldname order by fieldversion desc 
