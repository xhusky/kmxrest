package com.k2data.app.kmx;

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.serializer.SerializerFeature;
import com.k2data.app.kmx.cond.KmxCond;
import com.k2data.app.kmx.domain.DataRows;
import com.k2data.app.kmx.enums.Aggregation;
import com.k2data.app.kmx.enums.KmxCondType;
import com.k2data.app.kmx.enums.KmxRestVersion;
import com.k2data.app.kmx.enums.Sign;
import org.junit.Assert;
import org.junit.Test;

/**
 * @author lidong 17-1-17.
 */
public class DataRowsTest {

    private KmxInitParams initParams = new KmxInitParams(KmxRestVersion.V040)
            .setIdField("turbineId")
            .addUrl(KmxCondType.dataRows, "http://192.168.130.115:8089/data-service/v4/data-rows")
            .addUrl(KmxCondType.dataPoints, "http://192.168.130.115:8089/data-service/v4/data-points")
            .addUrl(KmxCondType.dataStreams, "http://192.168.130.115:8089/data-service/v4/data-streams");

    @Test
    public void F2_1() {
        KmxCond kmxCond = KmxCond.dataRows(initParams)
                .idValue("turbine_001_FCJtT_002")
                .start("2016-07-01T00:00:00.000%2B08:00")
                .end("2016-07-08T00:00:00.000%2B08:00")
                .fields(new String[]{"windSpeed", "pitchAngle"})
                .valueFilters(" { \"pitchAngle\": { \"$and\": [{\"$gte\": 0}, {\"$lte\": 25}] } }")
                .aggregations(Aggregation.AVG)
                .resultFormatIso()
                .build();

        DataRows dataRowsDomain = KmxClient.sync(kmxCond);

        for (int i = 0; i < 10; i++) {
            System.out.println(dataRowsDomain.getFirstResult().getFieldIndex("pitchAngle"));
        }

        System.out.println(JSON.toJSONString(dataRowsDomain, SerializerFeature.DisableCircularReferenceDetect));

        Assert.assertNotNull(JSON.toJSONString(dataRowsDomain, SerializerFeature.DisableCircularReferenceDetect));
    }

    @Test
    public void F2_2() {
        KmxCond kmxCond = KmxCond.dataRows(initParams)
                .idValue("turbine_001_FCJtT_002")
                .start("2016-07-01T00:00:00.000%2B08:00")
                .end("2016-07-08T00:00:00.000%2B08:00")
                .fields(new String[]{"windSpeed", "powerActive"})
                .valueTrans("powerActive", "powerActive * 1/120")
                .andNonIdFieldFilter("pitchAngle", Sign.GTE, 0)
                .andNonIdFieldFilter("pitchAngle", Sign.LTE, 25)
                .aggregations("windSpeed", Aggregation.AVG)
                .aggregations("{ \"powerProduction\": { \"type\": \"sum\", \"name\": \"powerActive\" } }")
                .resultFormatIso()
                .build();

        DataRows dataRowsDomain = KmxClient.sync(kmxCond);
        System.out.println(JSON.toJSONString(dataRowsDomain, SerializerFeature.DisableCircularReferenceDetect));

        Assert.assertNotNull(JSON.toJSONString(dataRowsDomain, SerializerFeature.DisableCircularReferenceDetect));
    }

    @Test
    public void F2_4() {
        KmxCond kmxCond = KmxCond.dataRows(initParams)
                .idValue("C206D3")
                .orIdValue("deviceid", "C20EAD")
                .start("2012-01-01T00:00:00.000%2B08:00")
                .end("2017-02-01T23:59:59.999%2B08:00")
                .fields(new String[]{"accStatus"})
//                .size(10)
//                .page(1)
                .resultFormatIso()
                .build();

        DataRows dataRowsDomain = KmxClient.sync(kmxCond);
        System.out.println(JSON.toJSONString(dataRowsDomain, SerializerFeature.DisableCircularReferenceDetect));

        Assert.assertNotNull(JSON.toJSONString(dataRowsDomain, SerializerFeature.DisableCircularReferenceDetect));
    }

    @Test
    public void F2_6() {
        KmxCond kmxCond = KmxCond.dataRows(initParams)
                .idValue("turbine_001_FCJtT_002")
                .start("2016-07-01T00:00:00.000%2B08:00")
                .end("2016-07-08T00:00:00.000%2B08:00")
                .fields(new String[]{"generatorSpeed", "powerActive"})
                .orNonIdFieldFilter("generatorSpeed", Sign.GT, 10)
                .andNonIdFieldFilter("pitchAngle", Sign.GTE, 0)
                .andNonIdFieldFilter("pitchAngle", Sign.LTE, 25)
                .resultFormatIso()
                .build();

        DataRows dataRowsDomain = KmxClient.sync(kmxCond);
        System.out.println(JSON.toJSONString(dataRowsDomain, SerializerFeature.DisableCircularReferenceDetect));

        Assert.assertNotNull(JSON.toJSONString(dataRowsDomain, SerializerFeature.DisableCircularReferenceDetect));
    }

    @Test
    public void F2_7() {
        KmxCond kmxCond = KmxCond.dataRows(initParams)
                .idValue("turbine_001_FCJtT_002")
                .start("2016-07-01T00:00:00.000%2B08:00")
                .end("2016-07-08T00:00:00.000%2B08:00")
                .fields(new String[]{"windSpeed", "powerActive"})
                .valueTrans("powerActive", "powerActive * 1/120")
                .andNonIdFieldFilter("pitchAngle", Sign.GTE, 0)
                .andNonIdFieldFilter("pitchAngle", Sign.LTE, 25)
                .aggregations("windSpeed", Aggregation.VARIANCE)
                .aggregations("powerProduction", "powerActive", Aggregation.STDDEV)
                .interval("1d")
                .fill(0)
                .resultFormatIso()
                .build();

        DataRows dataRowsDomain = KmxClient.sync(kmxCond);
        System.out.println(JSON.toJSONString(dataRowsDomain, SerializerFeature.DisableCircularReferenceDetect));

        Assert.assertNotNull(JSON.toJSONString(dataRowsDomain, SerializerFeature.DisableCircularReferenceDetect));
    }

    @Test
    public void F2_8_1() {
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

        DataRows dataRowsDomain = KmxClient.sync(kmxCond);
        System.out.println(JSON.toJSONString(dataRowsDomain, SerializerFeature.DisableCircularReferenceDetect));

        Assert.assertNotNull(JSON.toJSONString(dataRowsDomain, SerializerFeature.DisableCircularReferenceDetect));
    }

}
