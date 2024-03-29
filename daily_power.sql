/****** SSMS의 SelectTopNRows 명령 스크립트 ******/
;with elec as ( 
SELECT 
      [DATA_NO]
      ,[FARM_NO]
      ,[HOST_NO]
      ,[GET_DATE]
	  , CAST(LEFT(GET_DATE,4)+'-'+SUBSTRING(GET_DATE,5,2)+'-'+ SUBSTRING(GET_DATE,7,2)+' '+SUBSTRING(GET_DATE,9,2)+':'+SUBSTRING(GET_DATE,11,2)+':'+RIGHT(GET_DATE,2)as datetime) as 데이터데이트타임
	   ,CAST(LEFT(GET_DATE,4)+'-'+SUBSTRING(GET_DATE,5,2)+'-'+SUBSTRING(GET_DATE,7,2) as date ) as 데이터일자
	   ,SUBSTRING(GET_DATE,9,2) as 데이터hr
	   , SUBSTRING(GET_DATE,9,2)+':'+SUBSTRING(GET_DATE,11,2)+':'+RIGHT(GET_DATE,2) as 데이터시각 
      ,[PAVR]
  FROM [BK_MILK_W2].[dbo].[DATA_ECPH]
  where DATA_NO >= 882 
  ), elec2 as (
  select 
  ROW_NUMBER() over (partition by 데이터일자, 데이터hr order by 데이터시각 desc ) as rownum
  ,*
  from elec
  ), elec3 as ( 
  select * from elec2 where rownum = 1
  ) 
  select 
  데이터일자,
  sum(PAVR) as sumPower
  from elec3
  group by 데이터일자  
  order by 데이터일자 