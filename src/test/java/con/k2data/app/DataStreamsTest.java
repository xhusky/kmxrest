package con.k2data.app;

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.serializer.SerializerFeature;
import com.k2data.app.kmx.KmxClient;
import com.k2data.app.kmx.KmxInitParams;
import com.k2data.app.kmx.cond.KmxCond;
import com.k2data.app.kmx.domain.DataStreamsDomain;
import com.k2data.app.kmx.enums.Aggregation;
import com.k2data.app.kmx.enums.KmxCondType;
import com.k2data.app.kmx.enums.KmxRestVersion;
import com.k2data.app.kmx.enums.Order;
import org.junit.Assert;
import org.junit.Test;

/**
 * @author lidong 17-1-18.
 */
public class DataStreamsTest {

    private KmxInitParams initParams = new KmxInitParams(KmxRestVersion.V040)
            .setIdField("turbineId")
            .addUrl(KmxCondType.dataRows, "http://192.168.130.115:8089/data-service/v4/data-rows")
            .addUrl(KmxCondType.dataPoints, "http://192.168.130.115:8089/data-service/v4/data-points")
            .addUrl(KmxCondType.dataStreams, "http://192.168.130.115:8089/data-service/v4/data-streams");

    @Test
    public void F1_2() {
        KmxCond kmxCond = KmxCond.dataStreams(initParams)
                .idValue("turbine_001_FCJtT_002")
                .start("2016-11-01T00:00:00.000%2b08:00")
                .end("2017-04-01T00:00:00.000%2b08:00")
                .field("windSpeed")
                .aggregations("windSpeed", Aggregation.MAX, Aggregation.AVG)
                .interval("1m")
                .fill(0)
                .size(10000)
                .order("windSpeed", Order.DESC)
                .resultFormatIso()
                .build();

        DataStreamsDomain dataStreamsDomain = KmxClient.getSync(kmxCond);
        System.out.println(JSON.toJSONString(dataStreamsDomain, SerializerFeature.DisableCircularReferenceDetect));

        Assert.assertNotNull(JSON.toJSONString(dataStreamsDomain, SerializerFeature.DisableCircularReferenceDetect));
    }

}
