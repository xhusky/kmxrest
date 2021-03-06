package com.k2data.app.kmx;

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.serializer.SerializerFeature;
import com.k2data.app.kmx.cond.KmxCond;
import com.k2data.app.kmx.domain.Assets;
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

}
