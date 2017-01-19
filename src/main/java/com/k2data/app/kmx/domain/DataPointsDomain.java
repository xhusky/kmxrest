package com.k2data.app.kmx.domain;

import com.alibaba.fastjson.annotation.JSONField;

import java.util.ArrayList;
import java.util.List;

/**
 * @author lidong 17-1-17.
 */
public class DataPointsDomain {

    private Integer code;
    private String message;
    private List<DataPointsResultsDomain> results = new ArrayList<>();

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

    public List<DataPointsResultsDomain> getResults() {
        return results;
    }

    public void setResults(List<DataPointsResultsDomain> results) {
        this.results = results;
    }

}
