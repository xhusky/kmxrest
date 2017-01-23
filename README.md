# 调用 Kmx rest api 通用接口

## 用法

```java
private KmxInitParams initParams = new KmxInitParams(KmxRestVersion.V040)
            .setIdField("turbineId")
            .addUrl(KmxCondType.dataRows, "http://192.168.130.115:8089/data-service/v4/data-rows")
            .addUrl(KmxCondType.dataPoints, "http://192.168.130.115:8089/data-service/v4/data-points")
            .addUrl(KmxCondType.dataStreams, "http://192.168.130.115:8089/data-service/v4/data-streams");
            
KmxCond kmxCond = KmxCond.dataRows(initParams)
                .idValue("turbine_001_FCJtT_002")
                .start("2016-07-01T00:00:00.000%2B08:00")
                .end("2016-07-02T00:00:00.000%2B08:00")
                .fields(new String[]{"windSpeed", "powerActive"})
                .valueTrans("powerActive", "powerActive * 1/120")
                .andNonIdFieldFilter("pitchAngle", Sign.GTE, 0)
                .andNonIdFieldFilter("pitchAngle", Sign.LTE, 25)
                .aggregations("windSpeed", Aggregation.AVG)
                .aggregations("powerProduction", "powerActive", Aggregation.AVG)
                .interval("1h")
                .fill(0)
                .resultFormatIso()
                .build();
                
DataRowsDomain dataRowsDomain = KmxClient.postSync(kmxCond);
```

更多例子见 test
---

## 测试
```java
long start = System.currentTimeMillis();
for (int i = 0; i < 100000; i++) {
    KmxCond kmxCond = KmxCond.dataRows(initParams)
            .idValue("turbine_001_FCJtT_002")
            .start("2016-07-01T00:00:00.000%2B08:00")
            .end("2016-07-08T00:00:00.000%2B08:00")
            .fields(new String[]{"windSpeed", "pitchAngle"})
            .valueFilters(" { \"pitchAngle\": { \"$and\": [{\"$gte\": 0}, {\"$lte\": 25}] } }")
            .aggregations(Aggregation.AVG)
//                .resultFormatIso()
            .build();
}

long end = System.currentTimeMillis();
System.out.println(end - start);
        
// 1415ms
```
