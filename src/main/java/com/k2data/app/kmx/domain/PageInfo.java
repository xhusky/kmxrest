package com.k2data.app.kmx.domain;

import lombok.Data;

/**
 * @author lidong 17-1-17.
 */
@Data
public class PageInfo {

    private Integer pageNum;
    private Integer pageSize;
    private Integer size;
    private Integer total;
    private Integer pages;

}
