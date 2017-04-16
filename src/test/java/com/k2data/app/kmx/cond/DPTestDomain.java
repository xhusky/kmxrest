package com.k2data.app.kmx.cond;

import java.util.List;

/**
 * @author lidong 17-1-20.
 */
public class DPTestDomain {

    private String fieldGroup;
    private List<String> fields;
    private String sampleTime;
    private List<String> shift;
    private IdFilterTestDomain coValueFilter;
    private OptionTest options;

    public OptionTest getOptions() {
        return options;
    }

    public void setOptions(OptionTest options) {
        this.options = options;
    }

    public String getFieldGroup() {
        return fieldGroup;
    }

    public void setFieldGroup(String fieldGroup) {
        this.fieldGroup = fieldGroup;
    }

    public List<String> getFields() {
        return fields;
    }

    public void setFields(List<String> fields) {
        this.fields = fields;
    }

    public String getSampleTime() {
        return sampleTime;
    }

    public void setSampleTime(String sampleTime) {
        this.sampleTime = sampleTime;
    }

    public List<String> getShift() {
        return shift;
    }

    public void setShift(List<String> shift) {
        this.shift = shift;
    }

    public IdFilterTestDomain getCoValueFilter() {
        return coValueFilter;
    }

    public void setCoValueFilter(IdFilterTestDomain coValueFilter) {
        this.coValueFilter = coValueFilter;
    }
}
