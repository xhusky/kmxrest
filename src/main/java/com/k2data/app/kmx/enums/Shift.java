package com.k2data.app.kmx.enums;

/**
 * @author lidong 17-1-18.
 */
public enum Shift {

    NEAR,
    BEFORE,
    AFTER;

    public String getLowerValue() {
        return this.toString().toLowerCase();
    }

}
