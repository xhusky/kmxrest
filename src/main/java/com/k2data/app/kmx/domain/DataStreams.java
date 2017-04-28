package com.k2data.app.kmx.domain;

import com.fasterxml.jackson.annotation.JsonInclude;

import java.util.ArrayList;
import java.util.List;

/**
 * @author lidong 17-1-18.
 */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class DataStreams {

    private Integer code;
    private String message;
    private List<DataStreamsResults> results = new ArrayList<>();

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

    public List<DataStreamsResults> getResults() {
        return results;
    }

    public void setResults(List<DataStreamsResults> results) {
        this.results = results;
    }

}
