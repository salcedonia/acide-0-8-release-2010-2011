%
% Paths in a Graph
%

/sql
create table edge(origin string,destination string);
insert into edge values('a','b');
insert into edge values('a','c');
insert into edge values('b','a');
insert into edge values('b','d');
create view paths(origin,destination) as with recursive path(origin,destination) as (select * from edge) union (select path.origin,edge.destination from path,edge where path.destination=edge.origin) select * from path;
select * from paths;
