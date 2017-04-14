package com.k2data.app.kmx.domain;

import com.alibaba.fastjson.annotation.JSONField;
import com.k2data.app.kmx.KmxException;
import com.k2data.app.kmx.utils.Assert;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @author lidong 17-1-17.
 */
public class DataRowsResultsDomain {

    private PageInfo pageInfo;
    private Map<String, String> compoundId;
    private List<String> fields = new ArrayList<>();
    private List<RowDomain> rows = new ArrayList<>();

    @JSONField(serialize = false, deserialize = false)
    private volatile Map<String, Integer> fieldIdxMap;

    /**
     * 获取列的索引值，用于从 rows.value 中 get 取值
     *
     * @param field 字段名
     * @return 索引值
     */
    @JSONField(serialize = false, deserialize = false)
    public int getFieldIndex(String field) {
        Assert.notNull(field);

        if (fieldIdxMap == null) {
            synchronized (this) {
                if (fieldIdxMap == null) {
                    fieldIdxMap = new HashMap<>();
                    for (int i = 0; i < fields.size(); i++) {
                        fieldIdxMap.put(fields.get(i), i);
                    }
                }
            }
        }

        Integer idx = fieldIdxMap.get(field);
        if (idx == null) {
            throw new KmxException(String.format("field 未找到, input field: %s, fields: %s", field, fields));
        } else {
            return fieldIdxMap.get(field);
        }
    }

    public Map<String, String> getCompoundId() {
        return compoundId;
    }

    public void setCompoundId(Map<String, String> compoundId) {
        this.compoundId = compoundId;
    }

    public PageInfo getPageInfo() {
        return pageInfo;
    }

    public void setPageInfo(PageInfo pageInfo) {
        this.pageInfo = pageInfo;
    }

    public List<String> getFields() {
        return fields;
    }

    public void setFields(List<String> fields) {
        this.fields = fields;
    }

    public List<RowDomain> getRows() {
        return rows;
    }

    public void setRows(List<RowDomain> rows) {
        this.rows = rows;
    }

}
