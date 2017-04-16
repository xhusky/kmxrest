package com.k2data.app.kmx.domain;

import lombok.Data;

import java.util.Date;
import java.util.List;

/**
 * @author lidong9144@163.com
 */
@Data
public class FieldGroup {

    private String id;
    private String url;
    private List<String> idGroup;
    private String name;
    private String description;
    private Date createAt;
    private Date updateAt;
    private String status;
    private List<Field> fields;
    private List<String> tags;
    private List<Attribute> attributes;

}
