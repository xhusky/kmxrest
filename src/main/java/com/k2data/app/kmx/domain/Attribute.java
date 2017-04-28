package com.k2data.app.kmx.domain;

import com.fasterxml.jackson.annotation.JsonInclude;

/**
 * @author lidong9144@163.com 17-4-13.
 */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class Attribute {

    private String name;
    private String attributeValue;

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getAttributeValue() {
        return attributeValue;
    }

    public void setAttributeValue(String attributeValue) {
        this.attributeValue = attributeValue;
    }

}
