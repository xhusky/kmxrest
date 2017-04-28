package com.k2data.app.kmx.domain;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.k2data.app.kmx.enums.ValueType;

import java.util.List;

/**
 * @author lidong9144@163.com 17-4-13.
 */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class Field {

    private String id;
    private String name;
    private Boolean isIdField;
    private String description;
    private ValueType valueType;
    private List<Long> intervals;
    private String unit;

    private String systemId;
    private String unifiedField;

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public Boolean getIdField() {
        return isIdField;
    }

    public void setIdField(Boolean idField) {
        isIdField = idField;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public ValueType getValueType() {
        return valueType;
    }

    public void setValueType(ValueType valueType) {
        this.valueType = valueType;
    }

    public List<Long> getIntervals() {
        return intervals;
    }

    public void setIntervals(List<Long> intervals) {
        this.intervals = intervals;
    }

    public String getUnit() {
        return unit;
    }

    public void setUnit(String unit) {
        this.unit = unit;
    }

    public String getSystemId() {
        return systemId;
    }

    public void setSystemId(String systemId) {
        this.systemId = systemId;
    }

    public String getUnifiedField() {
        return unifiedField;
    }

    public void setUnifiedField(String unifiedField) {
        this.unifiedField = unifiedField;
    }

}
