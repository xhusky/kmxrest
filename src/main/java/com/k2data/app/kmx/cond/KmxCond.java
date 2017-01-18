package com.k2data.app.kmx.cond;

import com.k2data.app.kmx.KmxInitParams;

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

    public static DataRowsV120Builder dataRows(KmxInitParams initParams) {
        return new DataRowsV120Builder(initParams);
    }

    public static DataPointsV120Builder dataPoints(KmxInitParams initParams) {
        return new DataPointsV120Builder(initParams);
    }

    public static DataStreamsV120Builder dataStreams(KmxInitParams initParams) {
        return new DataStreamsV120Builder(initParams);
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

}
