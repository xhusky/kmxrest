package com.k2data.app.kmx.cond;

import java.util.List;

/**
 * @author lidong 17-1-20.
 */
public class IdFilterTestDomain {

    private String deivce;
    private List<String> $or;

    public String getDeivce() {
        return deivce;
    }

    public void setDeivce(String deivce) {
        this.deivce = deivce;
    }

    public List<String> get$or() {
        return $or;
    }

    public void set$or(List<String> $or) {
        this.$or = $or;
    }
}
