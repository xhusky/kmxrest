package com.k2data.app.kmx;

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.serializer.SerializerFeature;
import com.k2data.app.kmx.cond.KmxCond;
import com.k2data.app.kmx.domain.DataRows;
import com.k2data.app.kmx.enums.KmxCondType;
import com.k2data.app.kmx.enums.KmxRestVersion;
import org.junit.Assert;
import org.junit.Test;

/**
 * @author lidong 17-1-19.
 */
public class LinGongTest {

    private KmxInitParams initParams = new KmxInitParams(KmxRestVersion.V040)
            .setIdField("deviceid")
            .addUrl(KmxCondType.dataRows, "http://192.168.130.115:8089/data-service/v4/data-rows")
            .addUrl(KmxCondType.dataPoints, "http://192.168.130.115:8089/data-service/v4/data-points")
            .addUrl(KmxCondType.dataStreams, "http://192.168.130.115:8089/data-service/v4/data-streams");

    @Test
    public void test1() {

        KmxCond kmxCond = KmxCond.dataRows(initParams)
                .idValue("C206D3")
                .field("accStatus")
                .start("2016-01-01T00:00:00.000%2B08:00")
                .end("2017-02-01T00:00:00.000%2B08:00")
                .valueFilters(String.format("{ \"%s\": { \"$eq\": \"10\" } }", "accStatus"))
                .page(1)
                .size(1)
                .desc()
                .build();

        DataRows dataRowsDomain = KmxClient.sync(kmxCond);
        System.out.println(JSON.toJSONString(dataRowsDomain, SerializerFeature.DisableCircularReferenceDetect));

        Assert.assertNotNull(JSON.toJSONString(dataRowsDomain, SerializerFeature.DisableCircularReferenceDetect));
    }

}
