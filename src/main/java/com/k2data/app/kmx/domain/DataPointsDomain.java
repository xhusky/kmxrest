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

    /**
     * 获取第一个 result
     *
     * @return results.get(0)
     */
    @JSONField(serialize = false, deserialize = false)
    public DataPointsResultsDomain getFirstResult() {
        return getResults().get(0);
    }

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
