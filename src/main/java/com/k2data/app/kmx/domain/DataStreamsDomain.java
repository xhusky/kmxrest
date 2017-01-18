package com.k2data.app.kmx.domain;

import java.util.List;

/**
 * @author lidong 17-1-18.
 */
public class DataStreamsDomain {

    private Integer code;
    private String message;
    private List<DataStreamsResultsDomain> results;

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

    public List<DataStreamsResultsDomain> getResults() {
        return results;
    }

    public void setResults(List<DataStreamsResultsDomain> results) {
        this.results = results;
    }

}
