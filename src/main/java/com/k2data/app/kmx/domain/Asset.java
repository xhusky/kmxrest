package com.k2data.app.kmx.domain;

import com.fasterxml.jackson.annotation.JsonInclude;

import java.util.*;

/**
 * @author lidong9144@163.com 17-4-17.
 */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class Asset {

    private Map<String, String> compoundId = new HashMap<>();
    private String id;
    private String name;
    private String url;
    private String description;
    private Date createdAt;
    private Date updatedAt;
    private String fieldGroupId;
    private FieldGroup fieldGroup;
    private String status;
    private List<String> tags = new ArrayList<>();
    private List<Attribute> attributes = new ArrayList<>();
    private List<Field> fields = new ArrayList<>();

    public Map<String, String> getCompoundId() {
        return compoundId;
    }

    public void setCompoundId(HashMap<String, String> compoundId) {
        this.compoundId = compoundId;
    }

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

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public Date getCreatedAt() {
        return createdAt;
    }

    public void setCreatedAt(Date createdAt) {
        this.createdAt = createdAt;
    }

    public Date getUpdatedAt() {
        return updatedAt;
    }

    public void setUpdatedAt(Date updatedAt) {
        this.updatedAt = updatedAt;
    }

    public String getFieldGroupId() {
        return fieldGroupId;
    }

    public void setFieldGroupId(String fieldGroupId) {
        this.fieldGroupId = fieldGroupId;
    }

    public String getStatus() {
        return status;
    }

    public void setStatus(String status) {
        this.status = status;
    }

    public List<String> getTags() {
        return tags;
    }

    public void setTags(List<String> tags) {
        this.tags = tags;
    }

    public List<Attribute> getAttributes() {
        return attributes;
    }

    public void setAttributes(List<Attribute> attributes) {
        this.attributes = attributes;
    }

    public List<Field> getFields() {
        return fields;
    }

    public void setFields(List<Field> fields) {
        this.fields = fields;
    }

    public String getUrl() {
        return url;
    }

    public void setUrl(String url) {
        this.url = url;
    }

    public FieldGroup getFieldGroup() {
        return fieldGroup;
    }

    public void setFieldGroup(FieldGroup fieldGroup) {
        this.fieldGroup = fieldGroup;
    }

}
