package com.k2data.app.kmx.domain;

import lombok.Data;

import java.util.ArrayList;
import java.util.List;

/**
 * @author lidong 17-1-17.
 */
@Data
public class DataPoints {

    private Integer code;
    private String message;
    private List<DataPointsResults> results = new ArrayList<>();

}
