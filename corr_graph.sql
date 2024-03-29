--상관관계그래프 조회 
;  with ecph1 as ( 
SELECT 
      [DATA_NO]
      ,[GET_DATE]
	  , CAST(LEFT(GET_DATE,4)+'-'+SUBSTRING(GET_DATE,5,2)+'-'+ SUBSTRING(GET_DATE,7,2)+' '+SUBSTRING(GET_DATE,9,2)+':'+SUBSTRING(GET_DATE,11,2)+':'+RIGHT(GET_DATE,2)as datetime) as 데이터데이트타임
	   ,CAST(LEFT(GET_DATE,4)+'-'+SUBSTRING(GET_DATE,5,2)+'-'+SUBSTRING(GET_DATE,7,2) as date ) as 데이터일자
	   ,SUBSTRING(GET_DATE,9,2) as 데이터hr
	   , SUBSTRING(GET_DATE,9,2)+':'+SUBSTRING(GET_DATE,11,2)+':'+RIGHT(GET_DATE,2) as 데이터시각 
      ,[VC_VAL]
      ,[AC_VAL]
      ,[TPOW]
      ,[PAVR]
      ,[EC_VAL_I]
      ,[EC_VAL_O]
      ,[EC_T_I]
      ,[EC_T_O]
      ,[PH_VAL_I]
      ,[PH_VAL_O]
	  ,[PH_T_I]
      ,[PH_T_O]
      ,[PD2_VALUE]
  FROM [BK_MILK_W2].[dbo].[DATA_ECPH]
  where DATA_NO >= 882 --and DATA_NO < 4662
  ),  ecph2 as ( 
  select 
  데이터일자
  , avg(ABS(EC_VAL_I-EC_VAL_O)) as 전기전도도차
  ,avg(ec_val_i)/10 as 전기전도도인
  ,avg(EC_VAL_O)/10 as 전기전도도아웃
  ,avg(ph_val_i) as ph인
  ,avg(PH_VAL_O) as ph아웃
  ,avg(EC_T_I) as 온도인
  ,avg(EC_T_O) as 온도아웃
  ,avg(PD2_VALUE) as 압력 
  from ecph1 
  group by 데이터일자
) ,  water1 as ( 
SELECT
       [DATA_NO]
      ,[FARM_NO]
      ,[HOST_NO]
      ,[GET_DATE]
      ,[WATER_METER]
      ,[WATER_LITER]
	  , left(GET_DATE,8) as 데이터일자
	  ,SUBSTRING(GET_DATE,9,4) as 시간 
      ,[DIRECTION]
  FROM [BK_MILK_W2].[dbo].[DATA_WATER_UPLOAD]
  ) , waterIN as (
  select 
  데이터일자
  ,DIRECTION
  ,sum(WATER_LITER) as sumwaterIN
  from water1
  group by 데이터일자, DIRECTION
  having DIRECTION = 0
  ),
  waterOUT as (
  select 
  데이터일자
  ,DIRECTION
  ,sum(WATER_LITER) as sumwaterOUT
  from water1
  group by 데이터일자, DIRECTION
  having DIRECTION = 1
  )
 select t1.*, t2.sumwaterIN, t3.sumwaterOUT
 from ecph2 as t1 
 left outer join waterIN as t2	
					on t1.데이터일자 = t2.데이터일자
 left outer join waterOUT as t3 
					on t1.데이터일자 = t3.데이터일자
order by t1.데이터일자
  

