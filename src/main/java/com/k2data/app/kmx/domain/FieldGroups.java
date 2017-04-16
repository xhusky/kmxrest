package com.k2data.app.kmx.domain;

import lombok.Data;

import java.util.List;

/**
 * @author lidong9144@163.com 17-4-13.
 */
@Data
public class FieldGroups {

    private Integer code;
    private String message;
    private PageInfo pageInfo;
    private FieldGroup fieldGroup;
    private List<FieldGroup> fieldGroups;
    private Integer count;
    private List<String> idGroup;

}
