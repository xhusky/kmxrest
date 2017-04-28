package com.k2data.app.kmx.domain;

import com.alibaba.fastjson.annotation.JSONField;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.k2data.app.kmx.utils.KmxClientUtils;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

/**
 * @author lidong 17-1-17.
 */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class RowDomain {

    private List<Object> values = new ArrayList<>();
    private String iso;
    private Long timestamp;

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

    public List<Object> getValues() {
        return values;
    }

    public void setValues(List<Object> values) {
        this.values = values;
    }

    public String getIso() {
        return iso;
    }

    public void setIso(String iso) {
        this.iso = iso;
    }

    public Long getTimestamp() {
        return timestamp;
    }

    public void setTimestamp(Long timestamp) {
        this.timestamp = timestamp;
    }

}
