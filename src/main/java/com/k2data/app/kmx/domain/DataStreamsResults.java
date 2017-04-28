package com.k2data.app.kmx.domain;

import com.fasterxml.jackson.annotation.JsonInclude;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @author lidong 17-1-17.
 */
@JsonInclude(JsonInclude.Include.NON_NULL)
public class DataStreamsResults {

    private PageInfo pageInfo;
    private Map<String, String> compoundId = new HashMap<>();
    private String field;
    private List<List<String>> streams = new ArrayList<>();

    public PageInfo getPageInfo() {
        return pageInfo;
    }

    public void setPageInfo(PageInfo pageInfo) {
        this.pageInfo = pageInfo;
    }

    public Map<String, String> getCompoundId() {
        return compoundId;
    }

    public void setCompoundId(Map<String, String> compoundId) {
        this.compoundId = compoundId;
    }

    public String getField() {
        return field;
    }

    public void setField(String field) {
        this.field = field;
    }

    public List<List<String>> getStreams() {
        return streams;
    }

    public void setStreams(List<List<String>> streams) {
        this.streams = streams;
    }

}
