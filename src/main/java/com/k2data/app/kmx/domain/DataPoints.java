package com.k2data.app.kmx.domain;

import java.util.ArrayList;
import java.util.List;

/**
 * @author lidong 17-1-17.
 */
public class DataPoints {

    private Integer code;
    private String message;
    private List<DataPointsResults> results = new ArrayList<>();

    public Integer getCode() {
        return code;
    }

    public void setCode(Integer code) {
        this.code = code;
    }

    public String getMessage() {
        return message;
    }

    public void setMessage(String message) {
        this.message = message;
    }

    public List<DataPointsResults> getResults() {
        return results;
    }

    public void setResults(List<DataPointsResults> results) {
        this.results = results;
    }

}
