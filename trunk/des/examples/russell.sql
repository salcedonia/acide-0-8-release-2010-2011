/sql
create table man(man string);
insert into man values ('barber');
insert into man values ('mayor');
create view shaves(man,to) as select 'barber',man from man,shaves where man.man=shaves.man except select * from shaves where man=to;
select * from shaves;
