package com.k2data.app.kmx.enums;

/**
 * @author lidong 17-1-16.
 */
public enum Sign {

    EQ("$eq"),
    GT("$gt"),
    LT("$lt"),
    GTE("$gte"),
    LTE("$lte"),
    OR("$or"),
    AND("$and");

    private String value;

    Sign(String value) {
        this.value = value;
    }

    public String getValue() {
        return value;
    }
    public void setValue(String value) {
        this.value = value;
    }

}
