package com.k2data.app.kmx.domain;

import lombok.Data;

import java.util.List;
import java.util.Map;

/**
 * @author lidong 17-1-19.
 */
@Data
public class AssetsDomain {

    private Integer code;
    private String message;
    private PageInfo pageInfo;
    private List<Map<String, Object>> assets;

}
