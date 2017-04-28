package com.k2data.app.kmx.domain;

import com.fasterxml.jackson.annotation.JsonInclude;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

/**
 * @author lidong9144@163.com
 */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class FieldGroup {

    private String id;
    private String url;
    private List<String> idGroup = new ArrayList<>();
    private String name;
    private String description;
    private Date createAt;
    private Date updateAt;
    private String status;
    private List<Field> fields = new ArrayList<>();
    private List<String> tags = new ArrayList<>();
    private List<Attribute> attributes = new ArrayList<>();

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    public String getUrl() {
        return url;
    }

    public void setUrl(String url) {
        this.url = url;
    }

    public List<String> getIdGroup() {
        return idGroup;
    }

    public void setIdGroup(List<String> idGroup) {
        this.idGroup = idGroup;
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

    public Date getCreateAt() {
        return createAt;
    }

    public void setCreateAt(Date createAt) {
        this.createAt = createAt;
    }

    public Date getUpdateAt() {
        return updateAt;
    }

    public void setUpdateAt(Date updateAt) {
        this.updateAt = updateAt;
    }

    public String getStatus() {
        return status;
    }

    public void setStatus(String status) {
        this.status = status;
    }

    public List<Field> getFields() {
        return fields;
    }

    public void setFields(List<Field> fields) {
        this.fields = fields;
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

}
