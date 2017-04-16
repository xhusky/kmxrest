package com.k2data.app.kmx;

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.serializer.SerializerFeature;
import com.k2data.app.kmx.cond.KmxCond;
import com.k2data.app.kmx.domain.AssetsDomain;
import com.k2data.app.kmx.enums.KmxCondType;
import com.k2data.app.kmx.enums.KmxRestVersion;
import org.junit.Assert;
import org.junit.Test;

/**
 * @author lidong 17-1-19.
 */
public class AssetsTest {

    private KmxInitParams initParams = new KmxInitParams(KmxRestVersion.V040)
            .setIdField("turbineId")
            .addUrl(KmxCondType.assets, "http://192.168.130.115:8081/data-service/v2/assets");

    @Test
    public void test1() {
        KmxCond kmxCond = KmxCond.assets(initParams)
                .addParams("select", "fieldGroupId")
                .addParams("deviceid", "C20AD3")
                .build();

        AssetsDomain assetsDomain = KmxClient.sync(kmxCond);

        System.out.println(JSON.toJSONString(assetsDomain));

        Assert.assertNotNull(JSON.toJSONString(assetsDomain, SerializerFeature.DisableCircularReferenceDetect));
    }

}
