package com.k2data.app.kmx;

import com.k2data.app.kmx.enums.KmxCondType;
import com.k2data.app.kmx.enums.KmxRestVersion;

import java.util.HashMap;
import java.util.Map;

/**
 * Kmx 请求初始化参数, 构建查询条件时使用
 * <p>必须包含 {@code idField} 和 {@code urls}
 *
 * @see com.k2data.app.kmx.cond.KmxCond#dataPoints(KmxInitParams)
 *
 * @author lidong 17-1-18.
 */
public class KmxInitParams {

    private KmxRestVersion version;
    private Map<KmxCondType, String> urls = new HashMap<>();
    private String idField;

    public KmxInitParams(KmxRestVersion version) {
        this.version = version;
    }

    /**
     * 设置默认主键
     *
     * @param idField 主键
     * @return
     */
    public KmxInitParams setIdField(String idField) {
        this.idField = idField;
        return this;
    }

    /**
     * 添加接口类型对应的 url
     *
     * @param type 接口类型
     * @param url 接口地址
     * @return
     */
    public KmxInitParams addUrl(KmxCondType type, String url) {
        urls.put(type, url);
        return this;
    }

    public KmxRestVersion getVersion() {
        return version;
    }

    public void setVersion(KmxRestVersion version) {
        this.version = version;
    }

    public Map<KmxCondType, String> getUrls() {
        return urls;
    }

    public void setUrls(Map<KmxCondType, String> urls) {
        this.urls = urls;
    }

    public String getIdField() {
        return idField;
    }

}
