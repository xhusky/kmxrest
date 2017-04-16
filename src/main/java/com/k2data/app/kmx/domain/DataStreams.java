package com.k2data.app.kmx.domain;

import lombok.Data;

import java.util.List;

/**
 * @author lidong 17-1-18.
 */
@Data
public class DataStreams {

    private Integer code;
    private String message;
    private List<DataStreamsResults> results;

}
