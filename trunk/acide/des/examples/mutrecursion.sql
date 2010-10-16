/sql
/assert p(a)
/assert p(b)
/assert q(c)
/assert q(d)
create view q(x) as select * from q;
create or replace view p(x) as select * from q;
create or replace view q(x) as select * from p;
select * from p;
select * from q;
