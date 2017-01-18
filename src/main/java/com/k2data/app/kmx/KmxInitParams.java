package com.k2data.app.kmx;

import com.k2data.app.kmx.cond.KmxCond;
import com.k2data.app.kmx.enums.KmxCondType;
import com.k2data.app.kmx.enums.KmxRestVersion;

import java.util.HashMap;
import java.util.Map;

/**
 * @author lidong 17-1-18.
 */
public class KmxInitParams {

    private KmxRestVersion version;
    private Map<KmxCondType, String> urls = new HashMap<>();
    private String idField;

    public KmxInitParams(KmxRestVersion version) {
        this.version = version;
    }

    public KmxInitParams setIdField(String idField) {
        this.idField = idField;
        return this;
    }

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
