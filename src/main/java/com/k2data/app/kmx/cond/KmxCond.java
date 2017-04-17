package com.k2data.app.kmx.cond;

import com.k2data.app.kmx.KmxInitParams;
import com.k2data.app.kmx.enums.RequestType;

import java.util.Map;

/**
 * kmx rest 查询条件
 *
 * @author lidong 17-1-10.
 */
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

    public static AssetsV2Builder assets(KmxInitParams initParams) {
        return new AssetsV2Builder(initParams);
    }

    public static FieldGroupsV2Builder fieldGroups(KmxInitParams initParams) {
        return new FieldGroupsV2Builder(initParams);
    }

    public String getUrl() {
        return url;
    }

    public void setUrl(String url) {
        this.url = url;
    }

    public Map<String, String> getParams() {
        return params;
    }

    public void setParams(Map<String, String> params) {
        this.params = params;
    }

    public Class<?> getClazz() {
        return clazz;
    }

    public void setClazz(Class<?> clazz) {
        this.clazz = clazz;
    }

    public RequestType getRequestType() {
        return requestType;
    }

    public void setRequestType(RequestType requestType) {
        this.requestType = requestType;
    }

}
