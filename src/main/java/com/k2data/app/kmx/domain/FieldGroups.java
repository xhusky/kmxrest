package com.k2data.app.kmx.domain;

import com.fasterxml.jackson.annotation.JsonInclude;

import java.util.ArrayList;
import java.util.List;

/**
 * @author lidong9144@163.com 17-4-13.
 */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class FieldGroups {

    private Integer code;
    private String message;
    private PageInfo pageInfo;
    private FieldGroup fieldGroup;
    private List<FieldGroup> fieldGroups = new ArrayList<>();
    private Integer count;
    private List<String> idGroup = new ArrayList<>();

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

    public PageInfo getPageInfo() {
        return pageInfo;
    }

    public void setPageInfo(PageInfo pageInfo) {
        this.pageInfo = pageInfo;
    }

    public FieldGroup getFieldGroup() {
        return fieldGroup;
    }

    public void setFieldGroup(FieldGroup fieldGroup) {
        this.fieldGroup = fieldGroup;
    }

    public List<FieldGroup> getFieldGroups() {
        return fieldGroups;
    }

    public void setFieldGroups(List<FieldGroup> fieldGroups) {
        this.fieldGroups = fieldGroups;
    }

    public Integer getCount() {
        return count;
    }

    public void setCount(Integer count) {
        this.count = count;
    }

    public List<String> getIdGroup() {
        return idGroup;
    }

    public void setIdGroup(List<String> idGroup) {
        this.idGroup = idGroup;
    }

}
