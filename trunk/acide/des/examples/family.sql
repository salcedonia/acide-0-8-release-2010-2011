/sql
create table father(father string,child string);
insert into father values('tom','amy');
insert into father values('jack','fred');
insert into father values('tony','carolII');
insert into father values('fred','carolIII');
create table mother(mother string,child string);
insert into mother values('grace','amy');
insert into mother values('amy','fred');
insert into mother values('carolI','carolII');
insert into mother values('carolII','carolIII');
create view parent(parent,child) as select * from father union select * from mother;
create or replace view ancestor(ancestor,descendant) as select parent,child from parent union select parent,descendant from parent,ancestor where parent.child=ancestor.ancestor;  
select * from ancestor where ancestor='tom';
select child,father,mother from father,mother where father.child=mother.child;
