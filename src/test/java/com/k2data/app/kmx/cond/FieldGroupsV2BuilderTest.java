package com.k2data.app.kmx.cond;

import com.alibaba.fastjson.JSON;
import com.k2data.app.kmx.KmxInitParams;
import com.k2data.app.kmx.enums.KmxCondType;
import com.k2data.app.kmx.enums.KmxRestVersion;
import org.junit.Test;

/**
 * @author lidong9144@163.com
 */
public class FieldGroupsV2BuilderTest {

    private KmxInitParams initParams = new KmxInitParams(KmxRestVersion.V040)
            .setIdField("turbineId")
            .addUrl(KmxCondType.fieldGroups, "http://192.168.130.115:8081/data-service/v2/assets");


    @Test
    public void testPostBuild() {
        KmxCond cond = KmxCond.fieldGroups(initParams)
                .post()
                .id("aaa")
                .name("name")
                .build();
        System.out.println(JSON.toJSONString(cond));
    }

}
