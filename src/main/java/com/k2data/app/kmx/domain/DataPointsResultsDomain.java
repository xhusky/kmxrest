package com.k2data.app.kmx.domain;

import com.alibaba.fastjson.annotation.JSONField;
import com.k2data.app.kmx.utils.KmxClientUtils;

import java.util.Date;
import java.util.Map;

/**
 * @author lidong 17-1-18.
 */
public class DataPointsResultsDomain {

    private Map<String, String> compoundId;
    private String field;
    private String iso;
    private Long timestamp;
    private Object value;

    /**
     * 将时间字段格式化成 {@link Date}
     *
     * @return {@link Date}
     */
    @JSONField(serialize = false, deserialize = false)
    public Date getIsoFormatDate() {
        if (iso != null) {
            return KmxClientUtils.formatDate(iso.replace("+08:00", "Z"), KmxClientUtils.ISO_DATE_PATTERN);
        } else {
            return new Date(timestamp);
        }
    }

    public Map<String, String> getCompoundId() {
        return compoundId;
    }

    public void setCompoundId(Map<String, String> compoundId) {
        this.compoundId = compoundId;
    }

    public String getField() {
        return field;
    }

    public void setField(String field) {
        this.field = field;
    }

    public String getIso() {
        return iso;
    }

    public void setIso(String iso) {
        this.iso = iso;
    }

    public Object getValue() {
        return value;
    }

    public void setValue(Object value) {
        this.value = value;
    }

    public Long getTimestamp() {
        return timestamp;
    }

    public void setTimestamp(Long timestamp) {
        this.timestamp = timestamp;
    }

}
