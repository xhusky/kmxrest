package com.k2data.app.kmx.domain;

import com.k2data.app.kmx.KmxException;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

/**
 * @author lidong 17-1-17.
 */
public class DataRowsResultsDomain {

    private PageInfoDomain pageInfo;
    private Map<String, String> compoundId;
    private List<String> fields = new ArrayList<>();
    private List<RowDomain> rows = new ArrayList<>();

    /**
     * 获取列的索引值，用于从 rows.value 中 get 取值
     *
     * @param field 字段名
     * @return 索引值
     */
    public int getFieldIndex(String field) {
        for (int i = 0; i < fields.size(); i++) {
            if (field.equals(fields.get(i))) {
                return i;
            }
        }
        throw new KmxException("field 未找到, field: " + field + " fields: " + fields);
    }

    public Map<String, String> getCompoundId() {
        return compoundId;
    }

    public void setCompoundId(Map<String, String> compoundId) {
        this.compoundId = compoundId;
    }

    public PageInfoDomain getPageInfo() {
        return pageInfo;
    }

    public void setPageInfo(PageInfoDomain pageInfo) {
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
