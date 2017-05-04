# 调用 Kmx rest api 通用接口

## 支持的版本

v1.2.0

## 支持的接口

### 动态数据查询

- Data-points
- Data-rows
- Data-streams

### 元数据和静态数据

- FieldGroup
- Asset

### 数据实时写入

- Restful api

## 用法

1.首先创建一个全局的设置变量

```java
private KmxInitParams initParams = new KmxInitParams(KmxRestVersion.V040)
            .setIdField("turbineId")
            .addUrl(KmxCondType.dataRows, "http://192.168.130.115:8089/data-service/v4/data-rows")
            .addUrl(KmxCondType.dataPoints, "http://192.168.130.115:8089/data-service/v4/data-points")
            .addUrl(KmxCondType.dataStreams, "http://192.168.130.115:8089/data-service/v4/data-streams");
```

`idField` 为 `fieldGroup` 的 `idFieldId`

`addUrl()` 用来添加调用 kmx 接口的地址，`KmxCondType` 枚举为当前支持的接口，和 `KmxCond` 中的类型对应。这里添加需要调用的 api 地址即可，不用全部添加。

2.查询 api

`KmxCond` 为调用 kmx api 的参数设置，全部为链式调用，添加完必要的参数后调用 `build()` 生成请求条件。

以下为一个例子，更多例子见代码 test

```java
// initParams 是 1 中的创建的 KmxInitParams
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
```

参数设备好后使用 `KmxClient` 请求 Kmx，`KmxClient` 提供两种调用方法. 

- sync 同步调用

```java
DataRowsDomain dataRowsDomain = KmxClient.postSync(kmxCond);
```

- async 异步调用

默认并发数为 20, 暂不支持修改

```java
KmxClient.postAsync(kmxCond, (rsp) -> {
    // callback do something
});
```

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
