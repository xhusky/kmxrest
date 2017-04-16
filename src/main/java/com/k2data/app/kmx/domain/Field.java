package com.k2data.app.kmx.domain;

import com.k2data.app.kmx.enums.ValueType;
import lombok.Data;

import java.util.List;

/**
 * @author lidong9144@163.com 17-4-13.
 */
@Data
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

}
