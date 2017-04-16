package com.k2data.app.kmx.domain;

import lombok.Data;

import java.util.List;
import java.util.Map;

/**
 * @author lidong 17-1-17.
 */
@Data
public class DataStreamsResults {

    private PageInfo pageInfo;
    private Map<String, String> compoundId;
    private String field;
    private List<List<String>> streams;

}
