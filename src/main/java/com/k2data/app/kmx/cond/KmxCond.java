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
    private Boolean handleResponse = true;  // 是否处理请求的返回结果, 如果请求不返回结果就置为 false

    /**
     * Data-rows restful api
     *
     * @param initParams
     * @return
     */
    public static DataRowsV4Builder dataRows(KmxInitParams initParams) {
        return new DataRowsV4Builder(initParams);
    }

    /**
     * Data-points restful api
     *
     * @param initParams
     * @return
     */
    public static DataPointsV4Builder dataPoints(KmxInitParams initParams) {
        return new DataPointsV4Builder(initParams);
    }

    /**
     * Data-streams restful api
     *
     * @param initParams
     * @return
     */
    public static DataStreamsV4Builder dataStreams(KmxInitParams initParams) {
        return new DataStreamsV4Builder(initParams);
    }

    /**
     * Assets restful api
     *
     * @param initParams
     * @return
     */
    public static AssetsV2Builder assets(KmxInitParams initParams) {
        return new AssetsV2Builder(initParams);
    }

    /**
     * FieldGroups restful api
     *
     * @param initParams
     * @return
     */
    public static FieldGroupsV4Builder fieldGroups(KmxInitParams initParams) {
        return new FieldGroupsV4Builder(initParams);
    }

    /**
     * 实时数据写入 restful api
     *
     * @param initParams
     * @return
     */
    public static PostDataBuilder postData(KmxInitParams initParams) {
        return new PostDataBuilder(initParams);
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

    public Boolean getHandleResponse() {
        return handleResponse;
    }

    public void setHandleResponse(Boolean handleResponse) {
        this.handleResponse = handleResponse;
    }

}
