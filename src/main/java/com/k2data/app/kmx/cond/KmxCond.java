package com.k2data.app.kmx.cond;

import com.k2data.app.kmx.KmxInitParams;
import com.k2data.app.kmx.enums.RequestType;
import lombok.Data;

import java.util.Map;

/**
 * kmx rest 查询条件
 *
 * @author lidong 17-1-10.
 */
@Data
public class KmxCond {

    private String url;
    private Map<String, String> params;
    private Class<?> clazz;
    private RequestType requestType;

    public static DataRowsV4Builder dataRows(KmxInitParams initParams) {
        return new DataRowsV4Builder(initParams);
    }

    public static DataPointsV4Builder dataPoints(KmxInitParams initParams) {
        return new DataPointsV4Builder(initParams);
    }

    public static DataStreamsV4Builder dataStreams(KmxInitParams initParams) {
        return new DataStreamsV4Builder(initParams);
    }

    public static AssetsBuilder assets(KmxInitParams initParams) {
        return new AssetsBuilder(initParams);
    }

    public static FieldGroupsV2Builder fieldGroups(KmxInitParams initParams) {
        return new FieldGroupsV2Builder(initParams);
    }

}
