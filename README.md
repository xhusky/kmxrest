kmxrest

# 测试
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