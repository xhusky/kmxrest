package com.k2data.app.kmx.domain;

import java.util.List;
import java.util.Map;

/**
 * @author lidong 17-1-19.
 */
public class AssetsDomain {

    private Integer code;
    private String message;
    private PageInfoDomain pageInfoDomain;
    private List<Map<String, Object>> assets;

    public Integer getCode() {
        return code;
    }

    public void setCode(Integer code) {
        this.code = code;
    }

    public String getMessage() {
        return message;
    }

    public void setMessage(String message) {
        this.message = message;
    }

    public PageInfoDomain getPageInfoDomain() {
        return pageInfoDomain;
    }

    public void setPageInfoDomain(PageInfoDomain pageInfoDomain) {
        this.pageInfoDomain = pageInfoDomain;
    }

    public List<Map<String, Object>> getAssets() {
        return assets;
    }

    public void setAssets(List<Map<String, Object>> assets) {
        this.assets = assets;
    }
}
