;with water as ( 
SELECT
        [GET_DATE]
      ,[WATER_LITER]
	  , CAST(LEFT(GET_DATE,4)+'-'+SUBSTRING(GET_DATE,5,2)+'-'+
	    SUBSTRING(GET_DATE,7,2)+' '+SUBSTRING(GET_DATE,9,2)+':'+SUBSTRING(GET_DATE,11,2)+':'+RIGHT(GET_DATE,2)as datetime) as 데이터시각
	  ,CONVERT(DATE, '2019-08-05') as 선택일자
      ,substring(convert(NVARCHAR,GETDATE(),121),12,8) as 시스템시각
      ,[DIRECTION]
  FROM [BK_MILK_W2].[dbo].[DATA_WATER]
  ) , water2  as ( 
  select *
  ,convert(DATETIME,CONCAT(선택일자,' ',시스템시각)) as 선택일자시스템시각
  from water
 ) , final as ( 
 select *
 from water2
 where  데이터시각 < 선택일자시스템시각 and 데이터시각 >= dateadd(hh, -24,선택일자시스템시각) 
 ) 
 select *
 from final 