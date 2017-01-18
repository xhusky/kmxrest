package com.k2data.app.kmx.enums;

/**
 * @author lidong 17-1-16.
 */
public enum Aggregation {

    MIN,
    MAX,
    COUNT,
    AVG,
    VARIANCE,
    STDDEV;

    public String getLowerValue() {
        return this.toString().toLowerCase();
    }

}
